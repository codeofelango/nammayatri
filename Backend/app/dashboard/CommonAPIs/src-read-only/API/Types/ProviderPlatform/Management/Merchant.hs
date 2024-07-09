{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Merchant where

import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.Value
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data AllowedTripDistanceBoundsAPIEntity = AllowedTripDistanceBoundsAPIEntity
  { maxAllowedTripDistance :: Kernel.Types.Common.Meters,
    maxAllowedTripDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    minAllowedTripDistance :: Kernel.Types.Common.Meters,
    minAllowedTripDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data BatchSplitByPickupDistance = BatchSplitByPickupDistance {batchSplitDelay :: Kernel.Types.Common.Seconds, batchSplitSize :: Kernel.Prelude.Int} deriving (Generic, ToJSON, FromJSON, ToSchema)

data BatchSplitByPickupDistanceOnRide = BatchSplitByPickupDistanceOnRide {batchSplitDelay :: Kernel.Types.Common.Seconds, batchSplitSize :: Kernel.Prelude.Int}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CongestionChargeMultiplierAPIEntity
  = BaseFareAndExtraDistanceFare Kernel.Types.Common.Centesimal
  | ExtraDistanceFare Kernel.Types.Common.Centesimal
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data CreateFPDriverExtraFeeReq = CreateFPDriverExtraFeeReq
  { defaultStepFee :: Kernel.Types.Common.Money,
    defaultStepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    maxFee :: Kernel.Types.Common.Money,
    maxFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    minFee :: Kernel.Types.Common.Money,
    minFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    stepFee :: Kernel.Types.Common.Money,
    stepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DocumentType = RC | DL deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DocumentVerificationConfigCreateReq = DocumentVerificationConfigCreateReq
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    defaultStepFee :: Kernel.Types.Common.Money,
    defaultStepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    doStrictVerifcation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isDefaultEnabledOnManualVerification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isImageValidationRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxFee :: Kernel.Types.Common.Money,
    maxFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    maxRetryCount :: Kernel.Prelude.Int,
    minFee :: Kernel.Types.Common.Money,
    minFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    rcNumberPrefix :: Kernel.Prelude.Text,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    stepFee :: Kernel.Types.Common.Money,
    stepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    vehicleClassCheckType :: API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DocumentVerificationConfigItem = DocumentVerificationConfigItem
  { checkExpiry :: Kernel.Prelude.Bool,
    checkExtraction :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    documentType :: API.Types.ProviderPlatform.Management.Merchant.DocumentType,
    maxRetryCount :: Kernel.Prelude.Int,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleClassCheckType :: API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type DocumentVerificationConfigRes = [API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigItem]

data DocumentVerificationConfigUpdateReq = DocumentVerificationConfigUpdateReq
  { checkExpiry :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    checkExtraction :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    maxRetryCount :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    rcNumberPrefix :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Text),
    rcNumberPrefixList :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [Kernel.Prelude.Text]),
    supportedVehicleClasses :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.SupportedVehicleClasses,
    vehicleClassCheckType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverIntelligentPoolConfigRes = DriverIntelligentPoolConfigRes
  { acceptanceRatioWeightage :: Kernel.Prelude.Int,
    acceptanceRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    availabilityTimeWeightage :: Kernel.Prelude.Int,
    availabilityTimeWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationAndRideFrequencyRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    defaultDriverSpeed :: Kernel.Prelude.Double,
    driverSpeedWeightage :: Kernel.Prelude.Int,
    intelligentPoolPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    locationUpdateSampleTime :: Kernel.Types.Common.Minutes,
    minLocationUpdates :: Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPool :: Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    speedNormalizer :: Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
  { acceptanceRatioWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    acceptanceRatioWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    availabilityTimeWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    availabilityTimeWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationAndRideFrequencyRatioWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    defaultDriverSpeed :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double),
    driverSpeedWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    intelligentPoolPercentage :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    locationUpdateSampleTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Minutes),
    minLocationUpdates :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    minQuotesToQualifyForIntelligentPool :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    minQuotesToQualifyForIntelligentPoolWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    speedNormalizer :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverPoolConfigCreateReq = DriverPoolConfigCreateReq
  { actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    batchSizeOnRide :: Kernel.Prelude.Int,
    currentRideTripCategoryValidForForwardBatching :: [Kernel.Prelude.Text],
    distanceBasedBatchSplit :: [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistance],
    driverBatchSize :: Kernel.Prelude.Int,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    driverToDestinationDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    enableForwardBatching :: Kernel.Prelude.Bool,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Int,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    maxRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    minRadiusOfSearch :: Kernel.Types.Common.Meters,
    minRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    onRideBatchSplitConfig :: [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistanceOnRide],
    onRideRadiusConfig :: [API.Types.ProviderPlatform.Management.Merchant.OnRideRadiusConfig],
    poolSortingType :: API.Types.ProviderPlatform.Management.Merchant.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    radiusShrinkValueForDriversOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    radiusStepSize :: Kernel.Types.Common.Meters,
    radiusStepSizeWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    singleBatchProcessTime :: Kernel.Types.Common.Seconds
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverPoolConfigItem = DriverPoolConfigItem
  { actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    createdAt :: Kernel.Prelude.UTCTime,
    driverBatchSize :: Kernel.Prelude.Int,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    driverToDestinationDistanceThresholdWithUnit :: Kernel.Types.Common.Distance,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Int,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    maxRadiusOfSearchWithUnit :: Kernel.Types.Common.Distance,
    minRadiusOfSearch :: Kernel.Types.Common.Meters,
    minRadiusOfSearchWithUnit :: Kernel.Types.Common.Distance,
    poolSortingType :: API.Types.ProviderPlatform.Management.Merchant.PoolSortingType,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    radiusShrinkValueForDriversOnRideWithUnit :: Kernel.Types.Common.Distance,
    radiusStepSize :: Kernel.Types.Common.Meters,
    radiusStepSizeWithUnit :: Kernel.Types.Common.Distance,
    singleBatchProcessTime :: Kernel.Types.Common.Seconds,
    tripDistance :: Kernel.Types.Common.Meters,
    tripDistanceWithUnit :: Kernel.Types.Common.Distance,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type DriverPoolConfigRes = [API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigItem]

data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
  { actualDistanceThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Meters),
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Meters),
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Distance),
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Distance),
    distanceBasedBatchSplit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistance]),
    driverBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Seconds),
    driverQuoteLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverRequestCountLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxDriverQuotesRequired :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxNumberOfBatches :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxParallelSearchRequests :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxRadiusOfSearch :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    maxRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    minRadiusOfSearch :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    minRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    poolSortingType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue API.Types.ProviderPlatform.Management.Merchant.PoolSortingType),
    radiusStepSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    radiusStepSizeWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    singleBatchProcessTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data JobName = BadDebtCalculationTrigger | DriverFeeCalculationTrigger | SendManualPaymentLinkTrigger | ReferralPayoutTrigger deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MerchantCommonConfigRes = MerchantCommonConfigRes
  { actualRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    actualRideDistanceDiffThresholdWithUnit :: Kernel.Types.Common.Distance,
    approxRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    approxRideDistanceDiffThresholdWithUnit :: Kernel.Types.Common.Distance,
    checkImageExtractionForDashboard :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    defaultPopupDelay :: Kernel.Types.Common.Seconds,
    dropLocThreshold :: Kernel.Types.Common.Meters,
    dropLocThresholdWithUnit :: Kernel.Types.Common.Distance,
    includeDriverCurrentlyOnRide :: Kernel.Prelude.Bool,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    minLocationAccuracy :: Kernel.Prelude.Double,
    minRidesForCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    onboardingRetryTimeInHours :: Kernel.Prelude.Int,
    onboardingTryLimit :: Kernel.Prelude.Int,
    pickupLocThreshold :: Kernel.Types.Common.Meters,
    pickupLocThresholdWithUnit :: Kernel.Types.Common.Distance,
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    rideTimeEstimatedThreshold :: Kernel.Types.Common.Seconds,
    searchRepeatLimit :: Kernel.Prelude.Int,
    thresholdCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    upwardsRecomputeBuffer :: Kernel.Types.Common.HighPrecMeters,
    upwardsRecomputeBufferWithUnit :: Kernel.Types.Common.Distance
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
  { checkImageExtractionForDashboard :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    defaultPopupDelay :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    driverAutoPayExecutionTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverAutoPayNotificationTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverFeeCalculationTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    driverFeeCalculatorBatchGap :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    driverFeeCalculatorBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    driverFeeMandateExecutionBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverFeeMandateNotificationBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverPaymentCycleBuffer :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentCycleDuration :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentCycleStartTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentReminderInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    dropLocThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    dropLocThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    mandateExecutionRescheduleInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    mandateNotificationRescheduleInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    mediaFileSizeUpperLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    mediaFileUrlPattern :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Text),
    minRidesForCancellationScore :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    onboardingRetryTimeInHours :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    onboardingTryLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    orderAndNotificationStatusCheckTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    orderAndNotificationStatusCheckTimeLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    pickupLocThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    pickupLocThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Seconds),
    rideTimeEstimatedThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    searchRepeatLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    snapToRoadConfidenceThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double),
    thresholdCancellationScore :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    timeDiffFromUtc :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    useWithSnapToRoadFallback :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantUpdateReq = MerchantUpdateReq
  { description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantUpdateRes = MerchantUpdateRes
  { contactNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    status :: API.Types.ProviderPlatform.Management.Merchant.Status
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data NightShiftBounds = NightShiftBounds {nightShiftEnd :: Kernel.Prelude.TimeOfDay, nightShiftStart :: Kernel.Prelude.TimeOfDay} deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data NightShiftChargeAPIEntity
  = ProgressiveNightShiftCharge Kernel.Prelude.Float
  | ConstantNightShiftCharge Kernel.Types.Common.Money
  | ConstantNightShiftChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data OnRideRadiusConfig = OnRideRadiusConfig {batchNumber :: Kernel.Prelude.Int, onRideRadius :: Kernel.Types.Common.Meters, onRideRadiusWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PoolSortingType = Intelligent | Random deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data SchedulerTriggerReq = SchedulerTriggerReq
  { jobData :: Kernel.Prelude.Text,
    jobName :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.JobName,
    scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data UpdateFPPerExtraKmRateReq = UpdateFPPerExtraKmRateReq {perExtraKmRate :: Kernel.Types.Common.HighPrecMoney, perExtraKmRateWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { allowedTripDistanceBounds :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.AllowedTripDistanceBoundsAPIEntity,
    baseDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    baseDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    baseFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    congestionChargeMultiplier :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.CongestionChargeMultiplierAPIEntity,
    deadKmFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    deadKmFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    freeWaitingTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    nightShiftBounds :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.NightShiftBounds,
    nightShiftCharge :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.NightShiftChargeAPIEntity,
    perMinuteRideExtraTimeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perMinuteRideExtraTimeChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    serviceChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    waitingCharge :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.WaitingChargeAPIEntity,
    waitingChargeInfo :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.WaitingChargeInfoAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateOnboardingVehicleVariantMappingReq = UpdateOnboardingVehicleVariantMappingReq {file :: Kernel.Prelude.FilePath} deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpsertFarePolicyReq = UpsertFarePolicyReq {file :: Kernel.Prelude.FilePath} deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpsertFarePolicyResp = UpsertFarePolicyResp {success :: Kernel.Prelude.Text, unprocessedFarePolicies :: [Kernel.Prelude.Text]} deriving (Generic, ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType = Infix | Prefix | Suffix deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data WaitingChargeAPIEntity
  = PerMinuteWaitingCharge Kernel.Types.Common.HighPrecMoney
  | ConstantWaitingCharge Kernel.Types.Common.Money
  | PerMinuteWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  | ConstantWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data WaitingChargeInfoAPIEntity = WaitingChargeInfoAPIEntity {freeWaitingTime :: Kernel.Types.Common.Minutes, waitingCharge :: API.Types.ProviderPlatform.Management.Merchant.WaitingChargeAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type API = ("merchant" :> (PostMerchantUpdate :<|> PostMerchantServiceConfigMapsUpdate :<|> GetMerchantConfigCommon :<|> PostMerchantConfigCommonUpdate :<|> GetMerchantConfigDriverPool :<|> PostMerchantConfigDriverPoolUpdate :<|> PostMerchantConfigDriverPoolCreate :<|> GetMerchantConfigDriverIntelligentPool :<|> PostMerchantConfigDriverIntelligentPoolUpdate :<|> GetMerchantConfigOnboardingDocument :<|> PostMerchantConfigOnboardingDocumentUpdate :<|> PostMerchantConfigOnboardingDocumentCreate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> PostMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> PostMerchantConfigFarePolicyUpdate :<|> PostMerchantSchedulerTrigger))

type PostMerchantUpdate =
  ( "update" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq
      :> Post
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes
  )

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody ('[JSON]) Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigCommon = ("config" :> "common" :> Get ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)

type PostMerchantConfigCommonUpdate =
  ( "config" :> "common" :> "update" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigDriverPool =
  ( "config" :> "driverPool" :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> QueryParam
           "tripDistance"
           Kernel.Types.Common.Meters
      :> QueryParam "tripDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes
  )

type PostMerchantConfigDriverPoolUpdate =
  ( "config" :> "driverPool" :> "update" :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> QueryParam
           "tripCategory"
           Kernel.Prelude.Text
      :> QueryParam "tripDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "vehicleVariant"
           Dashboard.Common.Variant
      :> MandatoryQueryParam
           "area"
           Lib.Types.SpecialLocation.Area
      :> MandatoryQueryParam
           "tripDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigDriverPoolCreate =
  ( "config" :> "driverPool" :> "create" :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> QueryParam
           "tripCategory"
           Kernel.Prelude.Text
      :> QueryParam "tripDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "vehiclevariant"
           Dashboard.Common.Variant
      :> MandatoryQueryParam
           "area"
           Lib.Types.SpecialLocation.Area
      :> MandatoryQueryParam
           "tripDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigDriverIntelligentPool = ("config" :> "driverIntelligentPool" :> Get ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)

type PostMerchantConfigDriverIntelligentPoolUpdate =
  ( "config" :> "driverIntelligentPool" :> "update"
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigOnboardingDocument =
  ( "config" :> "onboardingDocument" :> QueryParam "documentType" API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> QueryParam
           "vehicleCategory"
           Dashboard.Common.Category
      :> Get ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes
  )

type PostMerchantConfigOnboardingDocumentUpdate =
  ( "config" :> "onboardingDocument" :> "update" :> MandatoryQueryParam "category" Dashboard.Common.Category
      :> MandatoryQueryParam
           "documentType"
           API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigOnboardingDocumentCreate =
  ( "config" :> "onboardingDocument" :> "create" :> MandatoryQueryParam "category" Dashboard.Common.Category
      :> MandatoryQueryParam
           "documentType"
           API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate =
  ( "config" :> "farePolicy"
      :> Capture
           "farePolicyId"
           (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> "driverExtraFeeBounds"
      :> "create"
      :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> QueryParam
           "startDistanceValue"
           Kernel.Types.Common.HighPrecDistance
      :> MandatoryQueryParam
           "startDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate =
  ( "config" :> "farePolicy"
      :> Capture
           "farePolicyId"
           (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> "driverExtraFeeBounds"
      :> "update"
      :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> QueryParam
           "startDistanceValue"
           Kernel.Types.Common.HighPrecDistance
      :> MandatoryQueryParam
           "startDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyPerExtraKmRateUpdate =
  ( "config" :> "farePolicy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> Capture
           "startDistance"
           Kernel.Types.Common.Meters
      :> "perExtraKmRate"
      :> "update"
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyUpdate =
  ( "config" :> "farePolicy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy) :> "update"
      :> ReqBody
           ('[JSON])
           API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSchedulerTrigger =
  ( "scheduler" :> "trigger" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: (API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes),
    postMerchantServiceConfigMapsUpdate :: (Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getMerchantConfigCommon :: (EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes),
    postMerchantConfigCommonUpdate :: (API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getMerchantConfigDriverPool :: (Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Meters) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes),
    postMerchantConfigDriverPoolUpdate :: (Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Dashboard.Common.Variant) -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigDriverPoolCreate :: (Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Dashboard.Common.Variant) -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getMerchantConfigDriverIntelligentPool :: (EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes),
    postMerchantConfigDriverIntelligentPoolUpdate :: (API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getMerchantConfigOnboardingDocument :: (Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Merchant.DocumentType) -> Kernel.Prelude.Maybe (Dashboard.Common.Category) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes),
    postMerchantConfigOnboardingDocumentUpdate :: (Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigOnboardingDocumentCreate :: (Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: (Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: (Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigFarePolicyPerExtraKmRateUpdate :: (Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantConfigFarePolicyUpdate :: (Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postMerchantSchedulerTrigger :: (API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> postMerchantServiceConfigMapsUpdate :<|> getMerchantConfigCommon :<|> postMerchantConfigCommonUpdate :<|> getMerchantConfigDriverPool :<|> postMerchantConfigDriverPoolUpdate :<|> postMerchantConfigDriverPoolCreate :<|> getMerchantConfigDriverIntelligentPool :<|> postMerchantConfigDriverIntelligentPoolUpdate :<|> getMerchantConfigOnboardingDocument :<|> postMerchantConfigOnboardingDocumentUpdate :<|> postMerchantConfigOnboardingDocumentCreate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> postMerchantConfigFarePolicyUpdate :<|> postMerchantSchedulerTrigger = merchantClient
