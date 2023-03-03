{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Init where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Veh
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.FareCalculator.OneWayFareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRFP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Geometry as QGeometry
import Tools.Error
import Tools.Maps as MapSearch

data InitReq = InitReq
  { vehicleVariant :: Veh.Variant,
    transactionId :: Text,
    fromLocation :: LatLong,
    startTime :: UTCTime,
    bapId :: Text,
    bapUri :: BaseUrl,
    initTypeReq :: InitTypeReq
  }

data InitTypeReq = InitOneWayTypeReq InitOneWayReq | InitRentalTypeReq InitRentalReq

newtype InitOneWayReq = InitOneWayReq
  { toLocation :: LatLong
  }

data InitRentalReq = InitRentalReq
  { distance :: Kilometers,
    duration :: Hours
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant
  }

init ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasField "driverEstimatedPickupDuration" r Seconds,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  InitReq ->
  m InitRes
init transporterId req = do
  transporter <- QM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  now <- getCurrentTime
  booking <- case req.initTypeReq of
    InitOneWayTypeReq oneWayReq -> do
      initOneWayTrip req oneWayReq transporter now
    InitRentalTypeReq rentalReq -> do
      initRentalTrip req rentalReq transporter now
  return $
    InitRes
      { booking = booking,
        transporter = transporter
      }

initOneWayTrip ::
  forall m r.
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasField "driverEstimatedPickupDuration" r Seconds,
    CoreMetrics m
  ) =>
  InitReq ->
  InitOneWayReq ->
  DM.Merchant ->
  UTCTime ->
  m DRB.Booking
initOneWayTrip req oneWayReq transporter now = do
  unlessM (rideServiceable transporter.geofencingConfig (\a b -> QGeometry.someGeometriesContain a b (Proxy @m)) req.fromLocation (Just oneWayReq.toLocation)) $
    throwError RideNotServiceable
  driverEstimatedPickupDuration <- asks (.driverEstimatedPickupDuration)
  mbDistRes <- CD.getCacheDistance req.transactionId
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          MapSearch.getDistance transporter.id $
            MapSearch.GetDistanceReq
              { origin = req.fromLocation,
                destination = oneWayReq.toLocation,
                travelMode = Just MapSearch.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  let estimatedRideDuration = duration
  let estimatedRideFinishTime = realToFrac (driverEstimatedPickupDuration + estimatedRideDuration) `addUTCTime` req.startTime
  fareParams <- calculateFare transporter.id req.vehicleVariant distance estimatedRideFinishTime
  toLoc <- buildRBLoc oneWayReq.toLocation now
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
      owDetails =
        DRB.OneWayDetails $
          DRB.OneWayBookingDetails
            { DRB.toLocation = toLoc,
              DRB.estimatedDistance = distance,
              DRB.estimatedFinishTime = estimatedRideFinishTime,
              DRB.estimatedDuration = estimatedRideDuration
            }
  fromLoc <- buildRBLoc req.fromLocation now
  booking <- buildBooking req transporter.id estimatedFare discount estimatedTotalFare owDetails fromLoc now
  DB.runTransaction $ do
    QRB.create @m booking
  return booking

initRentalTrip ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  InitReq ->
  InitRentalReq ->
  DM.Merchant ->
  UTCTime ->
  m DRB.Booking
initRentalTrip req rentalReq transporter now = do
  unlessM (rideServiceable transporter.geofencingConfig (\a b -> QGeometry.someGeometriesContain a b (Proxy @m)) req.fromLocation Nothing) $
    throwError RideNotServiceable
  let estimatedFare = 0
      discount = Nothing
      estimatedTotalFare = 0
  rentalFarePolicy <- QRFP.findByOffer transporter.id req.vehicleVariant rentalReq.distance rentalReq.duration >>= fromMaybeM NoFarePolicy
  let rentDetails = DRB.RentalDetails $ DRB.RentalBookingDetails {rentalFarePolicyId = rentalFarePolicy.id}
  fromLoc <- buildRBLoc req.fromLocation now
  booking <- buildBooking req transporter.id estimatedFare discount estimatedTotalFare rentDetails fromLoc now
  DB.runTransaction $ do
    QRB.create @m booking
  return booking

buildRBLoc ::
  (MonadFlow m) =>
  LatLong ->
  UTCTime ->
  m DLoc.BookingLocation
buildRBLoc latLon now = do
  locId <- generateGUID
  return
    DLoc.BookingLocation
      { id = locId,
        lat = latLon.lat,
        lon = latLon.lon,
        address =
          DLoc.LocationAddress
            { street = Nothing,
              city = Nothing,
              state = Nothing,
              country = Nothing,
              building = Nothing,
              areaCode = Nothing,
              area = Nothing
            },
        createdAt = now,
        updatedAt = now
      }

buildBooking ::
  MonadFlow m =>
  InitReq ->
  Id DM.Merchant ->
  Money ->
  Maybe Money ->
  Money ->
  DRB.BookingDetails ->
  DLoc.BookingLocation ->
  UTCTime ->
  m DRB.Booking
buildBooking req merchantId estimatedFare discount estimatedTotalFare bookingDetails fromLocation now = do
  id <- generateGUID
  return $
    DRB.Booking
      { id = Id id,
        status = DRB.NEW,
        providerId = merchantId,
        startTime = req.startTime,
        riderId = Nothing,
        fromLocation,
        bapId = req.bapId,
        bapUri = req.bapUri,
        estimatedFare = estimatedFare,
        discount = discount,
        estimatedTotalFare = estimatedTotalFare,
        vehicleVariant = req.vehicleVariant,
        reallocationsCount = 0,
        bookingDetails = bookingDetails,
        riderName = Nothing,
        createdAt = now,
        updatedAt = now
      }
