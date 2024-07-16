{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module SharedLogic.Allocator where

import Data.Singletons.TH
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantMessage
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Overlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as Plan
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Types.Common (Meters, Seconds)
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler

data AllocatorJobType
  = SendSearchRequestToDriver
  | UnblockDriver
  | SendPDNNotificationToDriver
  | MandateExecution
  | CalculateDriverFees
  | OrderAndNotificationStatusUpdate
  | SendOverlay
  | BadDebtCalculation
  | SendManualPaymentLink
  | RetryDocumentVerification
  | ScheduledRideNotificationsToDriver
  | DriverReferralPayout
  | ScheduledRideAssignedOnUpdate
  | CheckExotelStatusDoFallback
  deriving (Generic, FromDhall, Eq, Ord, Show, Read, FromJSON, ToJSON)

genSingletons [''AllocatorJobType]
showSingInstance ''AllocatorJobType

instance JobProcessor AllocatorJobType where
  restoreAnyJobInfo :: Sing (e :: AllocatorJobType) -> Text -> Maybe (AnyJobInfo AllocatorJobType)
  restoreAnyJobInfo SSendSearchRequestToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendSearchRequestToDriver jobData
  restoreAnyJobInfo SUnblockDriver jobData = AnyJobInfo <$> restoreJobInfo SUnblockDriver jobData
  restoreAnyJobInfo SSendPDNNotificationToDriver jobData = AnyJobInfo <$> restoreJobInfo SSendPDNNotificationToDriver jobData
  restoreAnyJobInfo SMandateExecution jobData = AnyJobInfo <$> restoreJobInfo SMandateExecution jobData
  restoreAnyJobInfo SCalculateDriverFees jobData = AnyJobInfo <$> restoreJobInfo SCalculateDriverFees jobData
  restoreAnyJobInfo SOrderAndNotificationStatusUpdate jobData = AnyJobInfo <$> restoreJobInfo SOrderAndNotificationStatusUpdate jobData
  restoreAnyJobInfo SSendOverlay jobData = AnyJobInfo <$> restoreJobInfo SSendOverlay jobData
  restoreAnyJobInfo SBadDebtCalculation jobData = AnyJobInfo <$> restoreJobInfo SBadDebtCalculation jobData
  restoreAnyJobInfo SSendManualPaymentLink jobData = AnyJobInfo <$> restoreJobInfo SSendManualPaymentLink jobData
  restoreAnyJobInfo SRetryDocumentVerification jobData = AnyJobInfo <$> restoreJobInfo SRetryDocumentVerification jobData
  restoreAnyJobInfo SScheduledRideNotificationsToDriver jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideNotificationsToDriver jobData
  restoreAnyJobInfo SDriverReferralPayout jobData = AnyJobInfo <$> restoreJobInfo SDriverReferralPayout jobData
  restoreAnyJobInfo SScheduledRideAssignedOnUpdate jobData = AnyJobInfo <$> restoreJobInfo SScheduledRideAssignedOnUpdate jobData
  restoreAnyJobInfo SCheckExotelStatusDoFallback jobData = AnyJobInfo <$> restoreJobInfo SCheckExotelStatusDoFallback jobData

data SendSearchRequestToDriverJobData = SendSearchRequestToDriverJobData
  { searchTryId :: Id DST.SearchTry,
    estimatedRideDistance :: Maybe Meters
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendSearchRequestToDriver

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

newtype UnblockDriverRequestJobData = UnblockDriverRequestJobData
  { driverId :: Id DP.Driver
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'UnblockDriver

type instance JobContent 'UnblockDriver = UnblockDriverRequestJobData

type instance JobContent 'SendSearchRequestToDriver = SendSearchRequestToDriverJobData

data SendPDNNotificationToDriverJobData = SendPDNNotificationToDriverJobData
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    retryCount :: Maybe Int,
    serviceName :: Maybe Plan.ServiceNames
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendPDNNotificationToDriver

type instance JobContent 'SendPDNNotificationToDriver = SendPDNNotificationToDriverJobData

data MandateExecutionInfo = MandateExecutionInfo
  { startTime :: UTCTime,
    endTime :: UTCTime,
    merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    serviceName :: Maybe Plan.ServiceNames
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'MandateExecution

type instance JobContent 'MandateExecution = MandateExecutionInfo

data CalculateDriverFeesJobData = CalculateDriverFeesJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    startTime :: UTCTime,
    endTime :: UTCTime,
    serviceName :: Maybe Plan.ServiceNames,
    scheduleNotification :: Maybe Bool,
    scheduleOverlay :: Maybe Bool,
    scheduleManualPaymentLink :: Maybe Bool,
    scheduleDriverFeeCalc :: Maybe Bool,
    createChildJobs :: Maybe Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'CalculateDriverFees

type instance JobContent 'CalculateDriverFees = CalculateDriverFeesJobData

data RetryDocumentVerificationJobData = RetryDocumentVerificationJobData
  { requestId :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'RetryDocumentVerification

type instance JobContent 'RetryDocumentVerification = RetryDocumentVerificationJobData

data OrderAndNotificationStatusUpdateJobData = OrderAndNotificationStatusUpdateJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'OrderAndNotificationStatusUpdate

type instance JobContent 'OrderAndNotificationStatusUpdate = OrderAndNotificationStatusUpdateJobData

data SendOverlayJobData = SendOverlayJobData
  { merchantId :: Id DM.Merchant,
    overlayKey :: Text,
    udf1 :: Maybe Text,
    condition :: OverlayCondition,
    rescheduleInterval :: Maybe Seconds,
    scheduledTime :: TimeOfDay,
    freeTrialDays :: Int,
    timeDiffFromUtc :: Seconds,
    driverPaymentCycleDuration :: NominalDiffTime,
    driverPaymentCycleStartTime :: NominalDiffTime,
    driverFeeOverlaySendingTimeLimitInDays :: Int,
    overlayBatchSize :: Int,
    serviceName :: Maybe Plan.ServiceNames,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendOverlay

type instance JobContent 'SendOverlay = SendOverlayJobData

data BadDebtCalculationJobData = BadDebtCalculationJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'BadDebtCalculation

type instance JobContent 'BadDebtCalculation = BadDebtCalculationJobData

data SendManualPaymentLinkJobData = SendManualPaymentLinkJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    serviceName :: Plan.ServiceNames,
    startTime :: UTCTime,
    endTime :: UTCTime,
    channel :: MediaChannel
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'SendManualPaymentLink

type instance JobContent 'SendManualPaymentLink = SendManualPaymentLinkJobData

data ScheduledRideNotificationsToDriverJobData = ScheduledRideNotificationsToDriverJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    timeDiffEvent :: DRN.TimeDiffEvent,
    bookingStatus :: DB.BookingStatus,
    notificationType :: DRN.NotificationType,
    notificationKey :: Text,
    onlyIfOffline :: Bool,
    bookingId :: Id DB.Booking,
    driverId :: Id DP.Person
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRideNotificationsToDriver

type instance JobContent 'ScheduledRideNotificationsToDriver = ScheduledRideNotificationsToDriverJobData

data DriverReferralPayoutJobData = DriverReferralPayoutJobData
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    toScheduleNextPayout :: Bool,
    statusForRetry :: DS.PayoutStatus
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

instance JobInfoProcessor 'DriverReferralPayout

type instance JobContent 'DriverReferralPayout = DriverReferralPayoutJobData

data ScheduledRideAssignedOnUpdateJobData = ScheduledRideAssignedOnUpdateJobData
  { bookingId :: Id DB.Booking,
    rideId :: Id SRide.Ride,
    driverId :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON)

instance JobInfoProcessor 'ScheduledRideAssignedOnUpdate

type instance JobContent 'ScheduledRideAssignedOnUpdate = ScheduledRideAssignedOnUpdateJobData

data CheckExotelStatusDoFallbackJobData = CheckExotelStatusDoFallbackJobData
  { ride :: DRide.Ride,
    booking :: DB.Booking
  }
  deriving (Generic, FromJSON, ToJSON)

instance JobInfoProcessor 'CheckExotelStatusDoFallback

type instance JobContent 'CheckExotelStatusDoFallback = CheckExotelStatusDoFallbackJobData
