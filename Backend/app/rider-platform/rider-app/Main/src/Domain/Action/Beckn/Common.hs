{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.Common where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Time hiding (getCurrentTime)
import Domain.Action.UI.HotSpot
import qualified Domain.Types.BecknConfig as BecknConfig
import qualified Domain.Types.Booking as BT
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Client as DC
import qualified Domain.Types.ClientPersonInfo as DPCI
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.HotSpot
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Beam.Functions
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.MerchantConfig as SMC
import SharedLogic.Payment as SPayment
import qualified SharedLogic.ScheduledNotifications as SN
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CRRN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.ClientPersonInfo as QCP
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event
import Tools.Maps (LatLong)
import Tools.Metrics (HasBAPMetrics, incrementRideCreatedRequestCount)
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

data BookingDetails = BookingDetails
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    otp :: Text,
    isInitiatedByCronJob :: Bool
  }

data RideAssignedReq = RideAssignedReq
  { bookingDetails :: BookingDetails,
    transactionId :: Text,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    isFreeRide :: Bool,
    driverAccountId :: Maybe Payment.AccountId,
    previousRideEndPos :: Maybe LatLong
  }

data OnlinePaymentParameters = OnlinePaymentParameters
  { paymentMethodId :: Payment.PaymentMethodId,
    customerPaymentId :: Payment.CustomerId,
    driverAccountId :: Payment.AccountId,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    email :: Maybe Text
  }

data ValidatedRideAssignedReq = ValidatedRideAssignedReq
  { bookingDetails :: BookingDetails,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool,
    vehicleAge :: Maybe Months,
    onlinePaymentParameters :: Maybe OnlinePaymentParameters,
    previousRideEndPos :: Maybe LatLong,
    booking :: DRB.Booking,
    fareParams :: Maybe [DFareBreakup]
  }

data RideStartedReq = RideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal,
    rideStartTime :: Maybe UTCTime,
    driverArrivalTime :: Maybe UTCTime
  }

data ValidatedRideStartedReq = ValidatedRideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal,
    rideStartTime :: Maybe UTCTime,
    driverArrivalTime :: Maybe UTCTime,
    ride :: DRide.Ride,
    booking :: DRB.Booking
  }

data RideCompletedReq = RideCompletedReq
  { bookingDetails :: BookingDetails,
    fare :: Price,
    totalFare :: Price,
    fareBreakups :: [DFareBreakup],
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    tollConfidence :: Maybe Confidence,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal,
    rideEndTime :: Maybe UTCTime,
    paymentStatus :: Maybe DRB.PaymentStatus
  }

data ValidatedRideCompletedReq = ValidatedRideCompletedReq
  { bookingDetails :: BookingDetails,
    fare :: Price,
    totalFare :: Price,
    fareBreakups :: [DFareBreakup],
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    tollConfidence :: Maybe Confidence,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal,
    rideEndTime :: Maybe UTCTime,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    person :: DPerson.Person,
    paymentStatus :: Maybe DRB.PaymentStatus
  }

data ValidatedFarePaidReq = ValidatedFarePaidReq
  { booking :: DRB.Booking,
    paymentStatus :: DRB.PaymentStatus
  }

data BookingCancelledReq = BookingCancelledReq
  { bookingDetails :: Maybe BookingDetails,
    bppBookingId :: Id DRB.BPPBooking,
    cancellationSource :: DBCR.CancellationSource
  }

data ValidatedBookingCancelledReq = ValidatedBookingCancelledReq
  { bookingDetails :: Maybe BookingDetails,
    bppBookingId :: Id DRB.BPPBooking,
    cancellationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    mbRide :: Maybe DRide.Ride
  }

data BookingReallocationReq = BookingReallocationReq ----need to use in future
  { bookingDetails :: BookingDetails,
    reallocationSource :: DBCR.CancellationSource
  }

data ValidatedBookingReallocationReq = ValidatedBookingReallocationReq ----need to use in future
  { bookingDetails :: BookingDetails,
    reallocationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    ride :: DRide.Ride
  }

data DriverArrivedReq = DriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime
  }

data ValidatedDriverArrivedReq = ValidatedDriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime,
    ride :: DRide.Ride,
    booking :: DRB.Booking
  }

data DFareBreakup = DFareBreakup
  { amount :: Price,
    description :: Text
  }

rideAssignedReqHandler ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig, "version" ::: DeploymentVersion],
    HasField "storeRidesTimeLimit" r Int,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedRideAssignedReq ->
  m ()
rideAssignedReqHandler req = do
  let BookingDetails {..} = req.bookingDetails
  void $ QRB.updateBPPBookingId req.booking.id bppBookingId
  let booking = req.booking {DRB.bppBookingId = Just bppBookingId}
  mbMerchant <- CQM.findById booking.merchantId
  now <- getCurrentTime
  ride <- buildRide mbMerchant booking req.bookingDetails req.previousRideEndPos now

  whenJust req.onlinePaymentParameters $ \OnlinePaymentParameters {..} -> do
    -- handle error flow properly
    let createPaymentIntentReq =
          Payment.CreatePaymentIntentReq
            { amount = booking.estimatedFare.amount,
              applicationFeeAmount = maybe 0.0 (.amount) booking.estimatedApplicationFee,
              currency = booking.estimatedFare.currency,
              customer = customerPaymentId,
              paymentMethod = paymentMethodId,
              receiptEmail = email,
              driverAccountId
            }
    void $ SPayment.makePaymentIntent booking.merchantId merchantOperatingCityId booking.riderId ride createPaymentIntentReq

  triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
  let category = case booking.specialLocationTag of
        Just _ -> "specialLocation"
        Nothing -> "normal"
  incrementRideCreatedRequestCount booking.merchantId.getId booking.merchantOperatingCityId.getId category
  let fareParams = fromMaybe [] req.fareParams
  fareBreakups <- traverse (buildFareBreakupV2 req.booking.id.getId DFareBreakup.BOOKING) fareParams
  QFareBreakup.createMany fareBreakups
  QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
  QRide.createRide ride
  QPFS.clearCache booking.riderId
  unless isInitiatedByCronJob $ do
    Notify.notifyOnRideAssigned booking ride
    when req.isDriverBirthDay $ do
      Notify.notifyDriverBirthDay booking.riderId driverName
  withLongRetry $ CallBPP.callTrack booking ride

  notifyRideRelatedNotificationOnEvent booking ride now DRN.RIDE_ASSIGNED
  notifyRideRelatedNotificationOnEvent booking ride now DRN.PICKUP_TIME
  where
    buildRide :: (MonadFlow m, HasFlowEnv m r '["version" ::: DeploymentVersion]) => Maybe DMerchant.Merchant -> DRB.Booking -> BookingDetails -> Maybe LatLong -> UTCTime -> m DRide.Ride
    buildRide mbMerchant booking BookingDetails {..} previousRideEndPos now = do
      guid <- generateGUID
      shortId <- generateShortId
      deploymentVersion <- asks (.version)
      let fromLocation = booking.fromLocation
          toLocation = case booking.bookingDetails of
            DRB.OneWayDetails details -> Just details.toLocation
            DRB.RentalDetails _ -> Nothing
            DRB.DriverOfferDetails details -> Just details.toLocation
            DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
            DRB.InterCityDetails details -> Just details.toLocation
            DRB.AmbulanceDetails details -> Just details.toLocation
      let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditLocationAttemptsThreshold) mbMerchant
      let allowedEditPickupLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
      let onlinePayment = maybe False (.onlinePayment) mbMerchant
      return
        DRide.Ride
          { id = guid,
            bookingId = booking.id,
            merchantId = Just booking.merchantId,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            clientId = booking.clientId,
            status = DRide.NEW,
            trackingUrl = Nothing,
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            traveledDistance = Nothing,
            driverArrivalTime = Nothing,
            vehicleVariant = DVST.castServiceTierToVariant booking.vehicleServiceTierType, -- fix later
            vehicleServiceTierType = Just booking.vehicleServiceTierType,
            createdAt = now,
            updatedAt = now,
            rideStartTime = Nothing,
            rideEndTime = Nothing,
            rideRating = Nothing,
            safetyCheckStatus = Nothing,
            isFreeRide = Just req.isFreeRide,
            endOtp = Nothing,
            startOdometerReading = Nothing,
            endOdometerReading = Nothing,
            clientBundleVersion = booking.clientBundleVersion,
            clientSdkVersion = booking.clientSdkVersion,
            clientDevice = booking.clientDevice,
            clientConfigVersion = booking.clientConfigVersion,
            backendConfigVersion = booking.backendConfigVersion,
            backendAppVersion = Just deploymentVersion.getDeploymentVersion,
            driversPreviousRideDropLoc = previousRideEndPos,
            showDriversPreviousRideDropLoc = isJust previousRideEndPos,
            tollConfidence = Nothing,
            distanceUnit = booking.distanceUnit,
            driverAccountId = req.onlinePaymentParameters <&> (.driverAccountId),
            paymentDone = False,
            vehicleAge = req.vehicleAge,
            cancellationFeeIfCancelled = Nothing,
            ..
          }

    notifyRideRelatedNotificationOnEvent booking ride now timeDiffEvent = do
      rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEvent booking.merchantOperatingCityId timeDiffEvent
      forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking ride now)

rideStartedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  ValidatedRideStartedReq ->
  m ()
rideStartedReqHandler ValidatedRideStartedReq {..} = do
  let BookingDetails {..} = bookingDetails
  fork "ride start geohash frequencyUpdater" $ do
    case tripStartLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripStart
      Nothing -> return ()
  let updRideForStartReq =
        ride{status = DRide.INPROGRESS,
             rideStartTime,
             rideEndTime = Nothing,
             endOtp = endOtp_,
             driverArrivalTime,
             startOdometerReading
            }
  triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
  _ <- QRide.updateMultiple updRideForStartReq.id updRideForStartReq
  QPFS.clearCache booking.riderId
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEvent booking.merchantOperatingCityId DRN.START_TIME
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking updRideForStartReq (fromMaybe now rideStartTime))
  unless isInitiatedByCronJob $ do
    fork "notify emergency contacts" $ Notify.notifyRideStartToEmergencyContacts booking ride
    Notify.notifyOnRideStarted booking ride

rideCompletedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  ValidatedRideCompletedReq ->
  m ()
rideCompletedReqHandler ValidatedRideCompletedReq {..} = do
  let BookingDetails {..} = bookingDetails
  fork "ride end geohash frequencyUpdater" $ do
    case tripEndLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripEnd
      Nothing -> return ()
  fork "updating total rides count" $ SMC.updateTotalRidesCounters booking.riderId
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs
  mbAdvRide <- QRide.findLatestByDriverPhoneNumber ride.driverMobileNumber
  whenJust mbAdvRide $ do \advRide -> when (advRide.id /= ride.id) $ QRide.updateshowDriversPreviousRideDropLoc False advRide.id
  let distanceUnit = ride.distanceUnit
  let updRide =
        ride{status = DRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = convertHighPrecMetersToDistance distanceUnit <$> chargeableDistance,
             traveledDistance = convertHighPrecMetersToDistance distanceUnit <$> traveledDistance,
             tollConfidence,
             rideEndTime,
             endOdometerReading
            }
  breakups <- traverse (buildFareBreakup ride.id) fareBreakups
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  fork "update first ride info" $ do
    mbPersonFirstRideInfo <- QCP.findByPersonIdAndVehicleCategory booking.riderId $ Just (DVST.castServiceTierToCategory booking.vehicleServiceTierType)
    case mbPersonFirstRideInfo of
      Just personFirstRideInfo -> do
        QCP.updateHasTakenValidRideCount (personFirstRideInfo.rideCount + 1) booking.riderId $ Just (DVST.castServiceTierToCategory booking.vehicleServiceTierType)
      Nothing -> do
        totalCount <- B.runInReplica $ QRB.findCountByRideIdStatusAndVehicleServiceTierType booking.riderId BT.COMPLETED (getListOfServiceTireTypes $ DVST.castServiceTierToCategory booking.vehicleServiceTierType)
        personClientInfo <- buildPersonClientInfo booking.riderId booking.clientId booking.merchantOperatingCityId booking.merchantId (DVST.castServiceTierToCategory booking.vehicleServiceTierType) (totalCount + 1)
        QCP.create personClientInfo
        when (totalCount + 1 == 1) $ do
          Notify.notifyFirstRideEvent booking.riderId (DVST.castServiceTierToCategory booking.vehicleServiceTierType)
  triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = DRB.COMPLETED}}
  when shouldUpdateRideComplete $ void $ QP.updateHasTakenValidRide booking.riderId
  unless (booking.status == DRB.COMPLETED) $ void $ QRB.updateStatus booking.id DRB.COMPLETED
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRRN.findAllByMerchantOperatingCityIdAndTimeDiffEvent booking.merchantOperatingCityId DRN.END_TIME
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking updRide (fromMaybe now rideEndTime))
  when (isJust paymentStatus && booking.paymentStatus /= Just DRB.PAID) $ QRB.updatePaymentStatus booking.id (fromJust paymentStatus)
  whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
  _ <- QRide.updateMultiple updRide.id updRide
  _ <- QFareBreakup.createMany breakups
  QPFS.clearCache booking.riderId

  -- uncomment for update api test; booking.paymentMethodId should be present
  -- whenJust booking.paymentMethodId $ \paymentMethodId -> do
  --   merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  --   paymentMethod <-
  --     CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
  --       >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId.getId)
  --   let dUpdateReq = ACL.PaymentCompletedBuildReq
  --         { bppBookingId,
  --           bppRideId = ride.bppRideId,
  --           paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod,
  --           bppId = booking.providerId,
  --           bppUrl = booking.providerUrl,
  --           transactionId = booking.transactionId,
  --           merchant
  --         }
  --   becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  --   void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq
  unless isInitiatedByCronJob $
    Notify.notifyOnRideCompleted booking updRide
  where
    buildFareBreakup :: MonadFlow m => Id DRide.Ride -> DFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup rideId DFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            entityId = rideId.getId,
            entityType = DFareBreakup.RIDE,
            ..
          }

getListOfServiceTireTypes :: BecknConfig.VehicleCategory -> [DVST.VehicleServiceTierType]
getListOfServiceTireTypes BecknConfig.CAB = [DVST.SEDAN, DVST.SUV, DVST.HATCHBACK, DVST.TAXI, DVST.TAXI_PLUS, DVST.ECO, DVST.COMFY, DVST.PREMIUM, DVST.PREMIUM_SEDAN, DVST.BLACK, DVST.BLACK_XL, DVST.SUV_PLUS]
getListOfServiceTireTypes BecknConfig.AUTO_RICKSHAW = [DVST.AUTO_RICKSHAW]
getListOfServiceTireTypes BecknConfig.MOTORCYCLE = [DVST.BIKE]
getListOfServiceTireTypes BecknConfig.AMBULANCE = [DVST.AMBULANCE_TAXI, DVST.AMBULANCE_TAXI_OXY, DVST.AMBULANCE_AC, DVST.AMBULANCE_AC_OXY, DVST.AMBULANCE_VENTILATOR]
getListOfServiceTireTypes BecknConfig.METRO = []

buildFareBreakupV2 :: MonadFlow m => Text -> DFareBreakup.FareBreakupEntityType -> DFareBreakup -> m DFareBreakup.FareBreakup
buildFareBreakupV2 entityId entityType DFareBreakup {..} = do
  guid <- generateGUID
  pure
    DFareBreakup.FareBreakup
      { id = guid,
        entityId,
        entityType,
        ..
      }

farePaidReqHandler :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => ValidatedFarePaidReq -> m ()
farePaidReqHandler req = void $ QRB.updatePaymentStatus req.booking.id req.paymentStatus

driverArrivedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedDriverArrivedReq ->
  m ()
driverArrivedReqHandler ValidatedDriverArrivedReq {..} = do
  unless (isJust ride.driverArrivalTime) $ do
    void $ QRide.updateDriverArrival ride.id arrivalTime
    QPFS.clearCache booking.riderId

bookingCancelledReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedBookingCancelledReq ->
  m ()
bookingCancelledReqHandler ValidatedBookingCancelledReq {..} = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason:-" <> show cancellationSource)
  cancellationTransaction booking mbRide cancellationSource Nothing

cancellationTransaction ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  DRB.Booking ->
  Maybe DRide.Ride ->
  DBCR.CancellationSource ->
  Maybe PriceAPIEntity ->
  m ()
cancellationTransaction booking mbRide cancellationSource cancellationFee = do
  bookingCancellationReason <- mkBookingCancellationReason booking (mbRide <&> (.id)) cancellationSource
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  fork "incrementing fraud counters" $ do
    case mbRide of
      Just ride -> do
        case cancellationSource of
          DBCR.ByUser -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
          DBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
          _ -> pure ()
        triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
      Nothing -> do
        logDebug "No ride found for the booking."
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs Nothing
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = DRB.CANCELLED}}
  QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == DRB.CANCELLED) $ void $ QRB.updateStatus booking.id DRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == DRide.CANCELLED) $ void $ QRide.updateStatus ride.id DRide.CANCELLED
  fork "Cancellation Settlement" $ do
    whenJust cancellationFee $ \fee -> do
      riderConfig <- QRiderConfig.findByMerchantOperatingCityId booking.merchantOperatingCityId >>= fromMaybeM (InternalError "RiderConfig not found")
      merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
      person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
      merchantCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      case (riderConfig.settleCancellationFeeBeforeNextRide, mbRide, person.mobileCountryCode) of
        (Just True, Just ride, Just countryCode) -> do
          SPayment.makeCancellationPayment booking.merchantId booking.merchantOperatingCityId booking.riderId ride fee
          --TODO: We can move this to stripe confirmation of payment
          void $
            CallBPPInternal.customerCancellationDuesSync
              (merchant.driverOfferApiKey)
              (merchant.driverOfferBaseUrl)
              (merchant.driverOfferMerchantId)
              mobileNumber
              countryCode
              (Just fee.amount)
              cancellationFee
              Nothing
              True
              (merchantCity.city)
        _ -> pure ()
  unless (cancellationSource == DBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  QPFS.clearCache booking.riderId
  -- notify customer
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
  Notify.notifyOnBookingCancelled booking cancellationSource bppDetails mbRide

mkBookingCancellationReason ::
  (MonadFlow m) =>
  DRB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  m DBCR.BookingCancellationReason
mkBookingCancellationReason booking mbRideId cancellationSource = do
  now <- getCurrentTime
  return $
    DBCR.BookingCancellationReason
      { bookingId = booking.id,
        merchantId = Just booking.merchantId,
        distanceUnit = booking.distanceUnit,
        rideId = mbRideId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        createdAt = now,
        updatedAt = now
      }

validateRideAssignedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideAssignedReq ->
  m ValidatedRideAssignedReq
validateRideAssignedReq RideAssignedReq {..} = do
  booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingDoesNotExist $ "transactionId:-" <> transactionId)
  mbMerchant <- CQM.findById booking.merchantId
  let onlinePayment = maybe False (.onlinePayment) mbMerchant
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  onlinePaymentParameters <-
    if onlinePayment
      then do
        person <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
        customerPaymentId <- person.customerPaymentId & fromMaybeM (CustomerPaymentIdNotFound booking.riderId.getId)
        paymentMethodId <- booking.paymentMethodId & fromMaybeM (PaymentMethodIdNotFound booking.id.getId)
        driverAccountId_ <- driverAccountId & fromMaybeM (DriverAccountIdNotFound booking.id.getId)
        let merchantOperatingCityId = person.merchantOperatingCityId
        email <- mapM decrypt person.email
        return $ Just OnlinePaymentParameters {driverAccountId = driverAccountId_, ..}
      else return Nothing
  return $ ValidatedRideAssignedReq {fareParams = Nothing, ..}
  where
    isAssignable booking = booking.status `elem` [DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.NEW]

validateRideStartedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideStartedReq ->
  m ValidatedRideStartedReq
validateRideStartedReq RideStartedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == DRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  return $ ValidatedRideStartedReq {..}

validateDriverArrivedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  DriverArrivedReq ->
  m ValidatedDriverArrivedReq
validateDriverArrivedReq DriverArrivedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus ("The ride has already started." <> Text.pack (show ride.status))
  return $ ValidatedDriverArrivedReq {..}
  where
    isValidRideStatus status = status == DRide.NEW

validateRideCompletedReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  RideCompletedReq ->
  m (Either ValidatedRideCompletedReq ValidatedFarePaidReq)
validateRideCompletedReq RideCompletedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- QRB.findByBPPBookingId bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bookingDetails.bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  let bookingCanBeCompleted = booking.status == DRB.TRIP_ASSIGNED
      rideCanBeCompleted = ride.status == DRide.INPROGRESS
      bookingAlreadyCompleted = booking.status == DRB.COMPLETED
      rideAlreadyCompleted = ride.status == DRide.COMPLETED
  if bookingAlreadyCompleted && rideAlreadyCompleted
    then validateFarePaidReq booking
    else do
      unless (isInitiatedByCronJob || bookingCanBeCompleted || (bookingAlreadyCompleted && rideCanBeCompleted)) $
        throwError (BookingInvalidStatus $ show booking.status)
      unless (isInitiatedByCronJob || rideCanBeCompleted || (rideAlreadyCompleted && bookingCanBeCompleted)) $
        throwError (RideInvalidStatus $ show ride.status)
      person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
      return . Left $ ValidatedRideCompletedReq {..}
  where
    validateFarePaidReq booking = do
      when (booking.paymentStatus == Just DRB.PAID) $ do
        throwError . InvalidRequest $ "payment_status is already PAID for bookingId:-" <> show booking.id.getId
      when (paymentStatus /= Just DRB.PAID) $ do
        throwError . InvalidRequest $ "Invalid payment status change:-" <> show paymentStatus <> " for bookingId:-" <> show booking.id.getId <> ", which is already completed."
      return . Right $ ValidatedFarePaidReq {booking, paymentStatus = fromJust paymentStatus} -- fromJust is safe here because of above check.

validateBookingCancelledReq ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  BookingCancelledReq ->
  m ValidatedBookingCancelledReq
validateBookingCancelledReq BookingCancelledReq {..} = do
  let isInitiatedByCronJob = maybe False (.isInitiatedByCronJob) bookingDetails
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [DRide.INPROGRESS, DRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == DRB.CANCELLED
  unless (isInitiatedByCronJob || isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedBookingCancelledReq {..}
  where
    isBookingCancellable booking =
      booking.status `elem` [DRB.NEW, DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.TRIP_ASSIGNED]

buildPersonClientInfo :: MonadFlow m => Id DPerson.Person -> Maybe (Id DC.Client) -> Id DMOC.MerchantOperatingCity -> Id DMerchant.Merchant -> BecknConfig.VehicleCategory -> Int -> m DPCI.ClientPersonInfo
buildPersonClientInfo personId clientId cityId merchantId vehicleCategory rideCount = do
  now <- getCurrentTime
  id <- generateGUID
  return
    DPCI.ClientPersonInfo
      { id = id,
        personId = personId,
        clientId = clientId,
        merchantOperatingCityId = cityId,
        merchantId = merchantId,
        vehicleCategory = Just vehicleCategory,
        rideCount = rideCount,
        createdAt = now,
        updatedAt = now
      }
