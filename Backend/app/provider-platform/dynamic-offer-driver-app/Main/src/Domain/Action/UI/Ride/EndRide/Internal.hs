{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
    getDistanceBetweenPoints,
    makeDailyDriverLeaderBoardKey,
    safeMod,
    makeWeeklyDriverLeaderBoardKey,
    getCurrentDate,
    getRidesAndDistancefromZscore,
    makeCachedDailyDriverLeaderBoardKey,
    makeCachedWeeklyDriverLeaderBoardKey,
    mkDriverFeeCalcJobFlagKey,
    getDriverFeeCalcJobFlagKey,
    getPlan,
    getDriverFeeBillNumberKey,
    mkDriverFeeBillNumberKey,
    mkDriverFee,
    setDriverFeeBillNumberKey,
    getDriverFeeCalcJobCache,
    setDriverFeeCalcJobCache,
  )
where

import qualified Data.Map as M
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationCharges as DCC
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.FareParameters as DFare
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import EulerHS.Prelude hiding (foldr, id, length, null)
import GHC.Float (double2Int)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (whenJust)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.DriverOnboarding
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import Storage.Queries.DriverPlan (findByDriverIdWithServiceName)
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications
import qualified Tools.PaymentNudge as PaymentNudge

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Id DP.Driver ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  DFare.FareParameters ->
  TransporterConfig ->
  m ()
endRideTransaction driverId booking ride mbFareParams mbRiderDetailsId newFareParams thresholdConfig = do
  QDI.updateOnRide (cast ride.driverId) False
  QRide.updateStatus ride.id Ride.COMPLETED
  QRB.updateStatus booking.id SRB.COMPLETED
  whenJust mbFareParams QFare.create
  QRide.updateAll ride.id ride
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  QDriverStats.updateIdleTime driverId
  QDriverStats.incrementTotalRidesAndTotalDist (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)

  clearCachedFarePolicyByEstOrQuoteId booking.quoteId

  when (thresholdConfig.subscription) $ do
    maxShards <- asks (.maxShards)
    let serviceName = YATRI_SUBSCRIPTION
    createDriverFee booking.providerId booking.merchantOperatingCityId driverId ride.fare newFareParams maxShards driverInfo booking serviceName

  triggerRideEndEvent RideEventData {ride = ride{status = Ride.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}

  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId

  sendReferralFCM ride booking.providerId mbRiderDetails booking.merchantOperatingCityId
  updateLeaderboardZScore booking.providerId booking.merchantOperatingCityId ride
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnRideCompletion {merchantId = booking.providerId, driverId = cast driverId, ride = ride}
  let customerCancellationDues = fromMaybe 0 newFareParams.customerCancellationDues
  when (thresholdConfig.canAddCancellationFee && customerCancellationDues > 0) $ do
    case mbRiderDetails of
      Just riderDetails -> do
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = cast driverId,
                  rideId = Just ride.id,
                  cancellationCharges = customerCancellationDues,
                  ..
                }
        calDisputeChances <-
          if thresholdConfig.cancellationFee == 0
            then do
              logWarning "Unable to calculate dispute chances used"
              return 0
            else do
              return $ round (customerCancellationDues / thresholdConfig.cancellationFee)
        QRD.updateDisputeChancesUsedAndCancellationDues riderDetails.id (max 0 (riderDetails.disputeChancesUsed - calDisputeChances)) 0 >> QCC.create cancellationCharges
      _ -> logWarning $ "Unable to update customer cancellation dues as RiderDetailsId is NULL with rideId " <> ride.id.getId

sendReferralFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  Ride.Ride ->
  Id Merchant ->
  Maybe RD.RiderDetails ->
  Id DMOC.MerchantOperatingCity ->
  m ()
sendReferralFCM ride merchantId mbRiderDetails merchantOpCityId = do
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
          Nothing -> True
  when shouldUpdateRideComplete $
    fork "REFERRAL_ACTIVATED FCM to Driver" $ do
      whenJust mbRiderDetails $ \riderDetails -> do
        QRD.updateHasTakenValidRide riderDetails.id
        case riderDetails.referredByDriver of
          Just referredDriverId -> do
            let referralMessage = "Congratulations!"
            let referralTitle = "Your referred customer has completed their first Namma Yatri ride"
            driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
            sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver.id driver.deviceToken
            logDebug "Driver Referral Coin Event"
            fork "DriverToCustomerReferralCoin Event : " $ DC.driverCoinsEvent driver.id merchantId merchantOpCityId (DCT.DriverToCustomerReferral ride.chargeableDistance)
          Nothing -> pure ()

updateLeaderboardZScore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Ride.Ride -> m ()
updateLeaderboardZScore merchantId merchantOpCityId ride = do
  fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ do
    nowUtc <- getCurrentTime
    let rideDate = getCurrentDate nowUtc
    driverZscore <- Hedis.zScore (makeDailyDriverLeaderBoardKey merchantId rideDate) $ ride.driverId.getId
    updateDriverDailyZscore ride rideDate driverZscore ride.chargeableDistance merchantId merchantOpCityId
    let (_, currDayIndex) = sundayStartWeek rideDate
    let weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
    let weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
    driverWeeklyZscore <- Hedis.zScore (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) $ ride.driverId.getId
    updateDriverWeeklyZscore ride rideDate weekStartDate weekEndDate driverWeeklyZscore ride.chargeableDistance merchantId merchantOpCityId

updateDriverDailyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> m ()
updateDriverDailyZscore ride rideDate driverZscore chargeableDistance merchantId merchantOpCityId = do
  mbdDailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId
  whenJust mbdDailyLeaderBoardConfig $ \dailyLeaderBoardConfig -> do
    when dailyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let lbExpiry = dailyLeaderBoardConfig.leaderBoardExpiry - secondsFromTimeOfDay localTime
      let currZscore =
            case driverZscore of
              Nothing -> dailyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 chargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore dailyLeaderBoardConfig.zScoreBase
                let currTotalRides = prevTotalRides + 1
                let currTotalDist = prevTotalDistance + fromMaybe 0 chargeableDistance
                currTotalRides * dailyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
      Hedis.zAddExp (makeDailyDriverLeaderBoardKey merchantId rideDate) ride.driverId.getId (fromIntegral currZscore) lbExpiry.getSeconds
      let limit = dailyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeDailyDriverLeaderBoardKey merchantId rideDate) 0 (limit -1)
      Hedis.setExp (makeCachedDailyDriverLeaderBoardKey merchantId rideDate) driversListWithScores' (dailyLeaderBoardConfig.leaderBoardExpiry.getSeconds * dailyLeaderBoardConfig.numberOfSets)

updateDriverWeeklyZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Day -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> m ()
updateDriverWeeklyZscore ride rideDate weekStartDate weekEndDate driverZscore rideChargeableDistance merchantId merchantOpCityId = do
  mbWeeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId
  whenJust mbWeeklyLeaderBoardConfig $ \weeklyLeaderBoardConfig -> do
    when weeklyLeaderBoardConfig.isEnabled $ do
      (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
      let (_, currDayIndex) = sundayStartWeek rideDate
      let lbExpiry = weeklyLeaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + (Seconds 86400 - secondsFromTimeOfDay localTime) -- Calculated as (total_Seconds_in_week - Week_Day_Index * 86400 + Seconds_Remaining_In_This_Week
      let currZscore =
            case driverZscore of
              Nothing -> weeklyLeaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
              Just zscore -> do
                let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore weeklyLeaderBoardConfig.zScoreBase
                let currTotalRides = prevTotalRides + 1
                let currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
                currTotalRides * weeklyLeaderBoardConfig.zScoreBase + getMeters currTotalDist
      Hedis.zAddExp (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) ride.driverId.getId (fromIntegral currZscore) (lbExpiry.getSeconds)
      let limit = weeklyLeaderBoardConfig.leaderBoardLengthLimit
      driversListWithScores' <- Hedis.zrevrangeWithscores (makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) 0 (limit -1)
      Hedis.setExp (makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate) driversListWithScores' (weeklyLeaderBoardConfig.leaderBoardExpiry.getSeconds * weeklyLeaderBoardConfig.numberOfSets)

makeCachedDailyDriverLeaderBoardKey :: Id Merchant -> Day -> Text
makeCachedDailyDriverLeaderBoardKey merchantId rideDate = "DDLBCK:" <> merchantId.getId <> ":" <> show rideDate

makeDailyDriverLeaderBoardKey :: Id Merchant -> Day -> Text
makeDailyDriverLeaderBoardKey merchantId rideDate =
  "DDLBK:" <> merchantId.getId <> ":" <> show rideDate

makeCachedWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> Text
makeCachedWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate =
  "DWLBCK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

makeWeeklyDriverLeaderBoardKey :: Id Merchant -> Day -> Day -> Text
makeWeeklyDriverLeaderBoardKey merchantId weekStartDate weekEndDate =
  "DWLBK:" <> merchantId.getId <> ":" <> show weekStartDate <> ":" <> show weekEndDate

getRidesAndDistancefromZscore :: Double -> Int -> (Int, Meters)
getRidesAndDistancefromZscore dzscore dailyZscoreBase =
  let (totalRides, totalDistance) = quotRem (double2Int dzscore) dailyZscoreBase
   in (totalRides, Meters totalDistance)

getCurrentDate :: UTCTime -> Day
getCurrentDate time =
  let currentDate = localDay $ utcToLocalTime timeZoneIST time
   in currentDate

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs

getDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  Meters ->
  m Meters
getDistanceBetweenPoints merchantId merchantOpCityId origin destination interpolatedPoints estimatedDistance = do
  -- somehow interpolated points pushed to redis in reversed order, so we need to reverse it back
  let pickedWaypoints = origin :| (pickWaypoints interpolatedPoints <> [destination])
  logTagInfo "endRide" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes merchantId merchantOpCityId $
      Maps.GetRoutesReq
        { waypoints = pickedWaypoints,
          mode = Just Maps.CAR,
          calcPoints = True
        }
  let mbShortestRouteDistance = (.distance) =<< getRouteInfoWithShortestDuration routeResponse
  -- Next error is impossible, because we never receive empty list from directions api
  --mbShortestRouteDistance & fromMaybeM (InvalidRequest "Couldn't calculate route distance")
  return $ fromMaybe estimatedDistance mbShortestRouteDistance

-- TODO reuse code from rider-app
getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration [] = Nothing
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteResult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteResult
  where
    comparator route1 route2 =
      if route1.duration < route2.duration
        then route1
        else route2

-- for distance api we can't pick more than 10 waypoints
pickWaypoints :: [a] -> [a]
pickWaypoints waypoints = do
  let step = length waypoints `div` 10
  take 10 $ foldr (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list) [] $ zip [1 ..] waypoints

safeMod :: Int -> Int -> Int
_ `safeMod` 0 = 0
a `safeMod` b = a `mod` b

createDriverFee ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Maybe Money ->
  DFare.FareParameters ->
  Int ->
  DI.DriverInformation ->
  SRB.Booking ->
  ServiceNames ->
  m ()
createDriverFee merchantId merchantOpCityId driverId rideFare newFareParams maxShards driverInfo booking serviceName = do
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  mbDriverPlan <- getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft
  let govtCharges = fromMaybe 0 newFareParams.govtCharges
  let (platformFee, cgst, sgst) = case newFareParams.fareParametersDetails of
        DFare.ProgressiveDetails _ -> (0, 0, 0)
        DFare.SlabDetails fpDetails -> (fromMaybe 0 fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst)
        DFare.RentalDetails _ -> (0, 0, 0)
  let totalDriverFee = fromIntegral govtCharges + platformFee + cgst + sgst
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  lastDriverFee <- QDF.findLatestFeeByDriverIdTypeAndServiceName driverId [DF.RECURRING_INVOICE, DF.RECURRING_EXECUTION_INVOICE] DF.ONGOING serviceName
  driverFee <- mkDriverFee serviceName now Nothing Nothing merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig (Just booking)
  let toUpdateOrCreateDriverfee = totalDriverFee > 0 || (totalDriverFee <= 0 && transporterConfig.isPlanMandatory && isJust mbDriverPlan)
  when (toUpdateOrCreateDriverfee && isEligibleForCharge transporterConfig freeTrialDaysLeft) $ do
    numRides <- case lastDriverFee of
      Just ldFee ->
        if now >= ldFee.startTime && now < ldFee.endTime
          then do
            QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst now True booking
            return (ldFee.numRides + 1)
          else do
            QDF.create driverFee
            return 1
      Nothing -> do
        QDF.create driverFee
        return 1
    plan <- getPlan mbDriverPlan serviceName merchantOpCityId
    fork "Sending switch plan nudge" $ PaymentNudge.sendSwitchPlanNudge transporterConfig driverInfo plan mbDriverPlan numRides serviceName
    scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now
  where
    isEligibleForCharge transporterConfig freeTrialDaysLeft =
      if DTC.isRideOtpBooking booking.tripCategory
        then transporterConfig.considerSpecialZoneRideChargesInFreeTrial || freeTrialDaysLeft <= 0
        else freeTrialDaysLeft <= 0

    getPlanAndPushToDefualtIfEligible :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> Int -> m (Maybe DriverPlan)
    getPlanAndPushToDefualtIfEligible transporterConfig freeTrialDaysLeft' = do
      mbDriverPlan' <- findByDriverIdWithServiceName (cast driverId) serviceName
      let planMandatory = transporterConfig.isPlanMandatory
          chargeSPZRides = transporterConfig.considerSpecialZoneRideChargesInFreeTrial
          isEligibleForDefaultPlanAfterFreeTrial = freeTrialDaysLeft' <= 0 && planMandatory && transporterConfig.allowDefaultPlanAllocation
          isEligibleForDefaultPlanBeforeFreeTrial = freeTrialDaysLeft' > 0 && chargeSPZRides && planMandatory
      if isNothing mbDriverPlan'
        then do
          case (DTC.isRideOtpBooking booking.tripCategory, isEligibleForDefaultPlanBeforeFreeTrial, isEligibleForDefaultPlanAfterFreeTrial) of
            (True, True, _) -> assignDefaultPlan
            (_, _, True) -> assignDefaultPlan
            _ -> return mbDriverPlan'
        else return mbDriverPlan'
    assignDefaultPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m (Maybe DriverPlan)
    assignDefaultPlan = do
      plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName
      case plans of
        (plan' : _) -> do
          newDriverPlan <- Plan.mkDriverPlan plan' (driverId, merchantId, merchantOpCityId)
          QDPlan.create newDriverPlan
          Plan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (Just DI.PENDING) Nothing
          QDI.updatPayerVpa Nothing (cast driverId)
          return $ Just newDriverPlan
        _ -> return Nothing

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Id MerchantOperatingCity -> Int -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId merchantOpCityId maxShards now = do
  void $
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> pure ()
      Just dfCalcTime -> do
        whenWithLockRedis (mkLockKeyForDriverFeeCalculation driverFee.startTime driverFee.endTime merchantOpCityId) 60 $ do
          isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName
          let dfCalculationJobTs = diffUTCTime (addUTCTime dfCalcTime driverFee.endTime) now
          case isDfCaclculationJobScheduled of
            ----- marker ---
            Nothing -> do
              createJobIn @_ @'CalculateDriverFees dfCalculationJobTs maxShards $
                CalculateDriverFeesJobData
                  { merchantId = merchantId,
                    merchantOperatingCityId = Just merchantOpCityId,
                    startTime = driverFee.startTime,
                    serviceName = Just (driverFee.serviceName),
                    scheduleNotification = Just True,
                    scheduleOverlay = Just True,
                    scheduleManualPaymentLink = Just True,
                    scheduleDriverFeeCalc = Just True,
                    createChildJobs = Just True,
                    endTime = driverFee.endTime
                  }
              setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName dfCalculationJobTs
              setDriverFeeBillNumberKey merchantOpCityId 1 36000 (driverFee.serviceName)
            _ -> pure ()

mkDriverFee ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  ServiceNames ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id Merchant ->
  Id DP.Driver ->
  Maybe Money ->
  Money ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  TransporterConfig ->
  Maybe SRB.Booking ->
  m DF.DriverFee
mkDriverFee serviceName now startTime' endTime' merchantId driverId rideFare govtCharges platformFee cgst sgst transporterConfig mbBooking = do
  id <- generateGUID
  let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig.driverPaymentCycleDuration startTime
      payBy = if isNothing transporterConfig.driverFeeCalculationTime then addUTCTime transporterConfig.driverPaymentCycleBuffer endTime else addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      platformFee_ = if isNothing transporterConfig.driverFeeCalculationTime then DF.PlatformFee platformFee cgst sgst else DF.PlatformFee 0 0 0
      govtCharges_ = if isNothing transporterConfig.driverFeeCalculationTime then govtCharges else 0
      isPlanMandatory = transporterConfig.isPlanMandatory
      totalFee = platformFee + cgst + sgst
      (specialZoneRideCount, specialZoneAmount) = specialZoneMetricsIntialization totalFee
      numRides = if serviceName == YATRI_SUBSCRIPTION then 1 else 0
  mbDriverPlan <- findByDriverIdWithServiceName (cast driverId) serviceName -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan serviceName transporterConfig.merchantOperatingCityId
  return $
    DF.DriverFee
      { status = DF.ONGOING,
        collectedBy = Nothing,
        collectedAt = Nothing,
        createdAt = now,
        updatedAt = now,
        platformFee = platformFee_,
        totalEarnings = fromMaybe 0 rideFare,
        feeType = case (plan, isPlanMandatory) of
          (Nothing, _) -> DF.RECURRING_INVOICE
          (Just plan_, True) -> if plan_.paymentMode == MANUAL then DF.RECURRING_INVOICE else DF.RECURRING_EXECUTION_INVOICE
          (Just _, False) -> DF.RECURRING_INVOICE,
        govtCharges = govtCharges_,
        offerId = Nothing,
        planOfferTitle = Nothing,
        autopayPaymentStage = Nothing,
        stageUpdatedAt = Nothing,
        billNumber = Nothing,
        schedulerTryCount = 0,
        feeWithoutDiscount = Nothing, -- Only for NY rn
        overlaySent = False,
        amountPaidByCoin = Nothing,
        planId = Nothing,
        planMode = Nothing,
        notificationRetryCount = 0,
        badDebtDeclarationDate = Nothing,
        badDebtRecoveryDate = Nothing,
        vehicleNumber = case mbDriverPlan <&> (.subscriptionServiceRelatedData) of
          Just (RentedVehicleNumber t) -> Just t
          _ -> Nothing,
        merchantOperatingCityId = transporterConfig.merchantOperatingCityId,
        startTime = fromMaybe startTime startTime',
        endTime = fromMaybe endTime endTime',
        ..
      }
  where
    specialZoneMetricsIntialization totalFee' = do
      let mbBookingCategory = mbBooking <&> (.tripCategory)
      if (DTC.isRideOtpBooking <$> mbBookingCategory) == Just True then (1, totalFee') else (0, 0)

getPlan ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe DriverPlan ->
  ServiceNames ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe Plan)
getPlan mbDriverPlan serviceName merchantOpCityId = do
  case mbDriverPlan of
    Just dp -> CQP.findByIdAndPaymentModeWithServiceName dp.planId dp.planType serviceName
    Nothing -> do
      plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName
      case plans of
        [] -> pure Nothing
        [pl] -> pure (Just pl)
        _ -> throwError $ InternalError "Multiple default plans found"

getDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName)

mkDriverFeeCalcJobCacheKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculation:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

mkDriverFeeCalcJobFlagKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculationFlag:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

getDriverFeeCalcJobFlagKey :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName)

setDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> NominalDiffTime -> m ()
setDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName expTime = do
  Hedis.setExp (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName) True (round $ expTime + 86399)
  Hedis.setExp (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName) False (round $ expTime + 86399)

mkDriverFeeBillNumberKey :: Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeBillNumberKey merchantOpCityId service = "DriverFeeCalulation:BillNumber:Counter" <> merchantOpCityId.getId <> ":service:" <> show service

getDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> ServiceNames -> m (Maybe Int)
getDriverFeeBillNumberKey merchantOpCityId serviceName = Hedis.get (mkDriverFeeBillNumberKey merchantOpCityId serviceName)

setDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> Int -> NominalDiffTime -> ServiceNames -> m ()
setDriverFeeBillNumberKey merchantOpCityId count expTime serviceName = Hedis.setExp (mkDriverFeeBillNumberKey merchantOpCityId serviceName) count (round expTime)

mkLockKeyForDriverFeeCalculation :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> Text
mkLockKeyForDriverFeeCalculation startTime endTime merchantOpCityId = "DriverFeeCalculation:Lock:MerchantId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime
