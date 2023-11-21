{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates
  ( module Reexport,
    whenWithLocationUpdatesLock,
    buildRideInterpolationHandler,
  )
where

import Data.List (sort)
import qualified Data.Vector as V
import Domain.Action.Beckn.Search
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Ride
import qualified Domain.Types.RideRoute as RI
import Environment
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import "location-updates" Lib.LocationUpdates as Reexport
import qualified Storage.CachedQueries.Merchant.TransporterConfig as MTC
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as TMaps

lowerBoundSearch :: (Ord a) => a -> V.Vector a -> Int
lowerBoundSearch target vec = binarySearch 0 (V.length vec - 1)
  where
    binarySearch l r =
      if l > r
        then l
        else
          let mid = l + ((r - l) `div` 2)
           in if vec V.! mid >= target
                then binarySearch l (mid - 1)
                else binarySearch (mid + 1) r

isWithinTolerance :: LatLong -> V.Vector LatLong -> Meters -> Bool
isWithinTolerance pt estimatedRoute routeDeviationThreshold = do
  let gap = fromIntegral routeDeviationThreshold / 1e5
      startingIndex = validateIndex $ lowerBoundSearch (LatLong (pt.lat - 1.25 * gap) pt.lon) estimatedRoute
      endingIndex = validateIndex $ lowerBoundSearch (LatLong (pt.lat + 1.25 * gap) pt.lon) estimatedRoute
      slicedRoute = V.slice startingIndex (endingIndex - startingIndex + 1) estimatedRoute
      minDistance = highPrecMetersToMeters (V.minimum $ V.map (distanceBetweenInMeters pt) slicedRoute)
   in minDistance <= routeDeviationThreshold
  where
    validateIndex index = if index >= V.length estimatedRoute then V.length estimatedRoute - 1 else index

checkForDeviation :: Meters -> V.Vector LatLong -> [LatLong] -> Int -> Bool
checkForDeviation _ _ [] deviationCount
  | deviationCount >= 3 = True
  | otherwise = False
checkForDeviation routeDeviationThreshold estimatedRoute (pt : batchWaypoints) deviationCount
  | deviationCount >= 3 = True
  | otherwise = do
    if isWithinTolerance pt estimatedRoute routeDeviationThreshold
      then checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints 0
      else checkForDeviation routeDeviationThreshold estimatedRoute batchWaypoints (deviationCount + 1)

updateDeviation :: (HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Meters -> Maybe (Id Ride) -> [LatLong] -> m Bool
updateDeviation _ Nothing _ = do
  logInfo "No ride found to check deviation"
  return False
updateDeviation routeDeviationThreshold (Just rideId) batchWaypoints = do
  logInfo "Updating Deviation"
  let key = searchRequestKey (getId rideId)
  routeInfo :: Maybe RI.RouteInfo <- Redis.get key
  case routeInfo >>= (.points) of
    Just estimatedRoute ->
      let estimatedRouteVec = V.fromList $ sort estimatedRoute
       in if checkForDeviation routeDeviationThreshold estimatedRouteVec batchWaypoints 0
            then do
              logInfo $ "Deviation detected for rideId: " <> show rideId
              QRide.updateDriverDeviatedFromRoute rideId True
              return True
            else do
              logInfo $ "No deviation detected for rideId: " <> show rideId
              return False
    Nothing -> do
      logWarning $ "Ride route points not found for rideId: " <> show rideId
      return False

buildRideInterpolationHandler :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Flow (RideInterpolationHandler Person Flow)
buildRideInterpolationHandler merchantId merchantOpCityId isEndRide = do
  transportConfig <- MTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let snapToRoad' =
        if transportConfig.useWithSnapToRoadFallback
          then TMaps.snapToRoadWithFallback merchantId merchantOpCityId
          else snapToRoadWithService
  return $
    mkRideInterpolationHandler
      isEndRide
      (\driverId dist googleSnapCalls osrmSnapCalls -> void (QRide.updateDistance driverId dist googleSnapCalls osrmSnapCalls))
      ( \driverId batchWaypoints -> do
          mRide <- QRide.getInProgressOrNewRideIdAndStatusByDriverId driverId
          updateDeviation transportConfig.routeDeviationThreshold (mRide <&> fst) batchWaypoints
      )
      snapToRoad'
  where
    snapToRoadWithService req = do
      resp <- TMaps.snapToRoad merchantId merchantOpCityId req
      return (Google, resp)

whenWithLocationUpdatesLock :: (HedisFlow m r, MonadMask m) => Id Person -> m () -> m ()
whenWithLocationUpdatesLock driverId f = do
  redisLockDriverId <- Redis.tryLockRedis lockKey 60
  if redisLockDriverId
    then do
      logDebug $ lockKey <> " Locked"
      finally
        f
        ( do
            Redis.unlockRedis lockKey
            logDebug $ "DriverId: " <> show driverId <> " Unlocked"
        )
    else do
      logDebug $ lockKey <> " unable to get lock"
      throwError (HitsLimitError 5)
  where
    lockKey = "DriverLocationUpdate:DriverId-" <> driverId.getId
