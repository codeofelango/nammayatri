{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Merchant
  ( module Dashboard.ProviderPlatform.Merchant,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Merchant as Reexport
-- import qualified Dashboard.Common as Common
import Dashboard.Common.Merchant as Reexport
import Data.Aeson
-- import Data.OpenApi hiding (description, name, password, url)
import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.APISuccess
import Kernel.Types.Common
-- import Kernel.Types.Id
import Kernel.Types.Predicate
import qualified Kernel.Types.SlidingWindowCounters as SWC
-- import Kernel.Types.Value
import qualified Kernel.Utils.Predicates as P
-- import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
-- import qualified Lib.Types.SpecialLocation as SL
import Servant

---------------------------------------------------------
-- merchant update --------------------------------------

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateTReq
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name,
      whenJust exoPhones $ \phones -> do
        sequenceA_
          [ validateField "exoPhones" phones $ UniqueField @"primaryPhone",
            validateField "exoPhones" phones $ UniqueField @"backupPhone"
          ],
      whenJust exoPhones $ \phones -> for_ phones $ \exophoneReq -> do
        validateObject "exoPhones" exophoneReq validateExophoneReq,
      whenJust fcmConfig $ \cfg -> validateObject "fcmConfig" cfg validateFCMConfigUpdateReq
    ]

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }

---------------------------------------------------------
-- merchant common config -------------------------------

-- type MerchantCommonConfigAPI =
--   "config"
--     :> "common"
--     :> Get '[JSON] MerchantCommonConfigRes

-- data MerchantCommonConfigRes = MerchantCommonConfigRes
--   { pickupLocThreshold :: Meters,
--     dropLocThreshold :: Meters,
--     pickupLocThresholdWithUnit :: Distance,
--     dropLocThresholdWithUnit :: Distance,
--     rideTimeEstimatedThreshold :: Seconds,
--     includeDriverCurrentlyOnRide :: Bool,
--     defaultPopupDelay :: Seconds,
--     popupDelayToAddAsPenalty :: Maybe Seconds,
--     thresholdCancellationScore :: Maybe Int,
--     minRidesForCancellationScore :: Maybe Int,
--     mediaFileUrlPattern :: Text,
--     mediaFileSizeUpperLimit :: Int,
--     onboardingTryLimit :: Int,
--     onboardingRetryTimeInHours :: Int,
--     checkImageExtractionForDashboard :: Bool,
--     searchRepeatLimit :: Int,
--     actualRideDistanceDiffThreshold :: HighPrecMeters,
--     upwardsRecomputeBuffer :: HighPrecMeters,
--     approxRideDistanceDiffThreshold :: HighPrecMeters,
--     actualRideDistanceDiffThresholdWithUnit :: Distance,
--     upwardsRecomputeBufferWithUnit :: Distance,
--     approxRideDistanceDiffThresholdWithUnit :: Distance,
--     minLocationAccuracy :: Double,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant common config update ------------------------

-- type MerchantCommonConfigUpdateAPI =
--   "config"
--     :> "common"
--     :> "update"
--     :> ReqBody '[JSON] MerchantCommonConfigUpdateReq
--     :> Post '[JSON] APISuccess

-- data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
--   { pickupLocThreshold :: Maybe (MandatoryValue Meters),
--     dropLocThreshold :: Maybe (MandatoryValue Meters),
--     pickupLocThresholdWithUnit :: Maybe (MandatoryValue Distance),
--     dropLocThresholdWithUnit :: Maybe (MandatoryValue Distance),
--     rideTimeEstimatedThreshold :: Maybe (MandatoryValue Seconds),
--     defaultPopupDelay :: Maybe (MandatoryValue Seconds),
--     popupDelayToAddAsPenalty :: Maybe (OptionalValue Seconds),
--     thresholdCancellationScore :: Maybe (OptionalValue Int),
--     minRidesForCancellationScore :: Maybe (OptionalValue Int),
--     mediaFileUrlPattern :: Maybe (MandatoryValue Text),
--     mediaFileSizeUpperLimit :: Maybe (MandatoryValue Int),
--     onboardingTryLimit :: Maybe (MandatoryValue Int),
--     onboardingRetryTimeInHours :: Maybe (MandatoryValue Int),
--     checkImageExtractionForDashboard :: Maybe (MandatoryValue Bool),
--     searchRepeatLimit :: Maybe (MandatoryValue Int),
--     driverPaymentCycleBuffer :: Maybe (MandatoryValue NominalDiffTime), -- TODO : Add in validation if reqd
--     driverPaymentCycleDuration :: Maybe (MandatoryValue NominalDiffTime),
--     driverPaymentCycleStartTime :: Maybe (MandatoryValue NominalDiffTime),
--     driverPaymentReminderInterval :: Maybe (MandatoryValue NominalDiffTime),
--     timeDiffFromUtc :: Maybe (MandatoryValue Seconds),
--     driverAutoPayNotificationTime :: Maybe (MandatoryValue NominalDiffTime),
--     driverAutoPayExecutionTime :: Maybe (MandatoryValue NominalDiffTime),
--     driverFeeMandateNotificationBatchSize :: Maybe (MandatoryValue Int),
--     driverFeeMandateExecutionBatchSize :: Maybe (MandatoryValue Int),
--     mandateNotificationRescheduleInterval :: Maybe (MandatoryValue NominalDiffTime),
--     mandateExecutionRescheduleInterval :: Maybe (MandatoryValue NominalDiffTime),
--     driverFeeCalculationTime :: Maybe (OptionalValue NominalDiffTime),
--     driverFeeCalculatorBatchSize :: Maybe (OptionalValue Int),
--     driverFeeCalculatorBatchGap :: Maybe (OptionalValue NominalDiffTime),
--     orderAndNotificationStatusCheckTime :: Maybe (OptionalValue NominalDiffTime),
--     orderAndNotificationStatusCheckTimeLimit :: Maybe (OptionalValue NominalDiffTime),
--     snapToRoadConfidenceThreshold :: Maybe (MandatoryValue Double),
--     useWithSnapToRoadFallback :: Maybe (MandatoryValue Bool)
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantCommonConfigUpdateReq where
  hideSecrets = identity

validateMerchantCommonConfigUpdateReq :: Validate MerchantCommonConfigUpdateReq
validateMerchantCommonConfigUpdateReq MerchantCommonConfigUpdateReq {..} =
  sequenceA_
    [ validateField "pickupLocThreshold" pickupLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "dropLocThreshold" dropLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "pickupLocThresholdWithUnit" pickupLocThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "dropLocThresholdWithUnit" dropLocThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "defaultPopupDelay" defaultPopupDelay $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "popupDelayToAddAsPenalty" popupDelayToAddAsPenalty $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "thresholdCancellationScore" thresholdCancellationScore $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "mediaFileUrlPattern" mediaFileUrlPattern $ InMaybe $ InValue $ MinLength 1,
      validateField "mediaFileSizeUpperLimit" mediaFileSizeUpperLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "minRidesForCancellationScore" minRidesForCancellationScore $ InMaybe $ InValue $ Min @Int 0,
      validateField "onboardingTryLimit" onboardingTryLimit $ InMaybe $ InValue $ Min @Int 0,
      validateField "onboardingRetryTimeInHours" onboardingRetryTimeInHours $ InMaybe $ InValue $ Min @Int 0,
      validateField "searchRepeatLimit" searchRepeatLimit $ InMaybe $ InValue $ Min @Int 0
      -- validateField "driverPaymentCycleDuration" driverPaymentCycleDuration $ InMaybe $ InValue $ Min @NominalDiffTime 86400,
      -- validateField "driverPaymentCycleStartTime" driverPaymentCycleStartTime $ InMaybe $ InValue $ InRange @NominalDiffTime 0 86399,
      -- validateField "timeDiffFromUtc" timeDiffFromUtc $ InMaybe $ InValue $ InRange @Seconds maxWestWardTimeDiff maxEastWardTimeDiff, --Set -12 hrs to 14 hrs as the max time diff from UTC is -12 Hrs to 14 Hrs
      -- validateField "driverPaymentCycleBuffer" driverPaymentCycleBuffer $ InMaybe $ InValue $ Max @NominalDiffTime (cycleBufferTimeLimit driverPaymentCycleDuration),
      -- validateField "driverPaymentReminderInterval" driverPaymentReminderInterval $ InMaybe $ InValue $ Max @NominalDiffTime (reminderIntervalLimit driverPaymentCycleBuffer)
    ]

-- where
--   maxWestWardTimeDiff = -12 * 3600
--   maxEastWardTimeDiff = 14 * 3600
--   cycleBufferTimeLimit mbMValue =
--     let mValue = fromMaybe (MandatoryValue 86400) mbMValue
--      in (mValue.value - 8 * 3600)
--   reminderIntervalLimit mbMValue =
--     let mValue = fromMaybe (MandatoryValue 14400) mbMValue
--      in (mValue.value * 0.5)

---------------------------------------------------------
-- merchant driver pool config  -------------------------

-- type DriverPoolConfigAPI =
--   "config"
--     :> "driverPool"
--     :> QueryParam "tripDistance" Meters
--     :> QueryParam "tripDistanceValue" HighPrecDistance
--     :> QueryParam "distanceUnit" DistanceUnit
--     :> Get '[JSON] DriverPoolConfigRes

-- data DriverPoolConfigItem = DriverPoolConfigItem
--   { minRadiusOfSearch :: Meters,
--     maxRadiusOfSearch :: Meters,
--     radiusStepSize :: Meters,
--     minRadiusOfSearchWithUnit :: Distance,
--     maxRadiusOfSearchWithUnit :: Distance,
--     radiusStepSizeWithUnit :: Distance,
--     driverPositionInfoExpiry :: Maybe Seconds,
--     actualDistanceThreshold :: Maybe Meters,
--     actualDistanceThresholdOnRide :: Maybe Meters,
--     actualDistanceThresholdWithUnit :: Maybe Distance,
--     actualDistanceThresholdOnRideWithUnit :: Maybe Distance,
--     maxDriverQuotesRequired :: Int,
--     driverQuoteLimit :: Int,
--     driverRequestCountLimit :: Int,
--     driverBatchSize :: Int,
--     maxNumberOfBatches :: Int,
--     maxParallelSearchRequests :: Int,
--     maxParallelSearchRequestsOnRide :: Int,
--     poolSortingType :: PoolSortingType,
--     singleBatchProcessTime :: Seconds,
--     tripDistance :: Meters,
--     radiusShrinkValueForDriversOnRide :: Meters,
--     driverToDestinationDistanceThreshold :: Meters,
--     tripDistanceWithUnit :: Distance,
--     radiusShrinkValueForDriversOnRideWithUnit :: Distance,
--     driverToDestinationDistanceThresholdWithUnit :: Distance,
--     driverToDestinationDuration :: Seconds,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data PoolSortingType = Intelligent | Random
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver pool config update -------------------

-- type DriverPoolConfigUpdateAPI =
--   "config"
--     :> "driverPool"
--     :> "update"
--     :> MandatoryQueryParam "tripDistance" Meters
--     :> QueryParam "tripDistanceValue" HighPrecDistance
--     :> QueryParam "distanceUnit" DistanceUnit
--     :> MandatoryQueryParam "area" SL.Area
--     :> QueryParam "vehicleVariant" Variant
--     :> QueryParam "tripCategory" Text
--     :> ReqBody '[JSON] DriverPoolConfigUpdateReq
--     :> Post '[JSON] APISuccess

-- data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
--   { minRadiusOfSearch :: Maybe (MandatoryValue Meters),
--     maxRadiusOfSearch :: Maybe (MandatoryValue Meters),
--     radiusStepSize :: Maybe (MandatoryValue Meters),
--     minRadiusOfSearchWithUnit :: Maybe (MandatoryValue Distance),
--     maxRadiusOfSearchWithUnit :: Maybe (MandatoryValue Distance),
--     radiusStepSizeWithUnit :: Maybe (MandatoryValue Distance),
--     driverPositionInfoExpiry :: Maybe (OptionalValue Seconds),
--     actualDistanceThreshold :: Maybe (OptionalValue Meters),
--     actualDistanceThresholdOnRide :: Maybe (OptionalValue Meters),
--     actualDistanceThresholdWithUnit :: Maybe (OptionalValue Distance),
--     actualDistanceThresholdOnRideWithUnit :: Maybe (OptionalValue Distance),
--     maxDriverQuotesRequired :: Maybe (MandatoryValue Int),
--     driverQuoteLimit :: Maybe (MandatoryValue Int),
--     driverRequestCountLimit :: Maybe (MandatoryValue Int),
--     driverBatchSize :: Maybe (MandatoryValue Int),
--     maxNumberOfBatches :: Maybe (MandatoryValue Int),
--     maxParallelSearchRequests :: Maybe (MandatoryValue Int),
--     maxParallelSearchRequestsOnRide :: Maybe (MandatoryValue Int),
--     poolSortingType :: Maybe (MandatoryValue PoolSortingType),
--     singleBatchProcessTime :: Maybe (MandatoryValue Seconds),
--     distanceBasedBatchSplit :: Maybe (MandatoryValue [BatchSplitByPickupDistance])
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverPoolConfigUpdateReq :: Validate DriverPoolConfigUpdateReq
validateDriverPoolConfigUpdateReq DriverPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ InMaybe $ InValue $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ InMaybe $ InValue $ Min @Meters (maybe 1 (.value) minRadiusOfSearch),
      validateField "radiusStepSize" radiusStepSize $ InMaybe $ InValue $ Min @Meters 1,
      validateField "minRadiusOfSearchWithUnit" minRadiusOfSearchWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 1 Meter),
      validateField "maxRadiusOfSearchWithUnit" maxRadiusOfSearchWithUnit $ InMaybe $ InValue $ Min @Distance (maybe (Distance 1 Meter) (.value) minRadiusOfSearchWithUnit),
      validateField "radiusStepSizeWithUnit" radiusStepSizeWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 1 Meter),
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ InValue $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "actualDistanceThresholdOnRide" actualDistanceThresholdOnRide $ InMaybe $ InValue $ Min @Meters 0,
      validateField "actualDistanceThresholdWithUnit" actualDistanceThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "actualDistanceThresholdOnRideWithUnit" actualDistanceThresholdOnRideWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequestsOnRide" maxParallelSearchRequestsOnRide $ InMaybe $ InValue $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ InMaybe $ InValue $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver pool config create -------------------

-- type DriverPoolConfigCreateAPI =
--   "config"
--     :> "driverPool"
--     :> "create"
--     :> MandatoryQueryParam "tripDistance" Meters
--     :> QueryParam "tripDistanceValue" HighPrecDistance
--     :> QueryParam "distanceUnit" DistanceUnit
--     :> MandatoryQueryParam "area" SL.Area
--     :> QueryParam "vehiclevariant" Variant
--     :> QueryParam "tripCategory" Text
--     :> ReqBody '[JSON] DriverPoolConfigCreateReq
--     :> Post '[JSON] APISuccess

-- data BatchSplitByPickupDistance = BatchSplitByPickupDistance
--   { batchSplitSize :: Int,
--     batchSplitDelay :: Seconds
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data BatchSplitByPickupDistanceOnRide = BatchSplitByPickupDistanceOnRide
--   { batchSplitSize :: Int,
--     batchSplitDelay :: Seconds
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data OnRideRadiusConfig = OnRideRadiusConfig
--   { onRideRadius :: Meters,
--     onRideRadiusWithUnit :: Maybe Distance,
--     batchNumber :: Int
--   }
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data DriverPoolConfigCreateReq = DriverPoolConfigCreateReq
--   { minRadiusOfSearch :: Meters,
--     maxRadiusOfSearch :: Meters,
--     radiusStepSize :: Meters,
--     minRadiusOfSearchWithUnit :: Maybe Distance,
--     maxRadiusOfSearchWithUnit :: Maybe Distance,
--     radiusStepSizeWithUnit :: Maybe Distance,
--     driverPositionInfoExpiry :: Maybe Seconds,
--     actualDistanceThreshold :: Maybe Meters,
--     actualDistanceThresholdOnRide :: Maybe Meters,
--     onRideBatchSplitConfig :: [BatchSplitByPickupDistanceOnRide],
--     onRideRadiusConfig :: [OnRideRadiusConfig],
--     enableForwardBatching :: Bool,
--     currentRideTripCategoryValidForForwardBatching :: [Text],
--     batchSizeOnRide :: Int,
--     actualDistanceThresholdWithUnit :: Maybe Distance,
--     actualDistanceThresholdOnRideWithUnit :: Maybe Distance,
--     maxDriverQuotesRequired :: Int,
--     driverQuoteLimit :: Int,
--     driverRequestCountLimit :: Int,
--     driverBatchSize :: Int,
--     maxNumberOfBatches :: Int,
--     maxParallelSearchRequests :: Int,
--     maxParallelSearchRequestsOnRide :: Int,
--     poolSortingType :: PoolSortingType,
--     distanceBasedBatchSplit :: [BatchSplitByPickupDistance],
--     singleBatchProcessTime :: Seconds,
--     radiusShrinkValueForDriversOnRide :: Meters,
--     driverToDestinationDistanceThreshold :: Meters,
--     radiusShrinkValueForDriversOnRideWithUnit :: Maybe Distance,
--     driverToDestinationDistanceThresholdWithUnit :: Maybe Distance,
--     driverToDestinationDuration :: Seconds
--   }
-- deriving stock (Show, Generic)
-- deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigCreateReq where
  hideSecrets = identity

validateDriverPoolConfigCreateReq :: Validate DriverPoolConfigCreateReq
validateDriverPoolConfigCreateReq DriverPoolConfigCreateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ Min @Meters minRadiusOfSearch,
      validateField "radiusStepSize" radiusStepSize $ Min @Meters 1,
      validateField "minRadiusOfSearchWithUnit" minRadiusOfSearchWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "maxRadiusOfSearchWithUnit" maxRadiusOfSearchWithUnit $ InMaybe $ Min @Distance $ fromMaybe (convertMetersToDistance Meter minRadiusOfSearch) minRadiusOfSearchWithUnit,
      validateField "radiusStepSizeWithUnit" radiusStepSizeWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ Min @Meters 0,
      validateField "actualDistanceThresholdOnRide" actualDistanceThresholdOnRide $ InMaybe $ Min @Meters 0,
      validateField "actualDistanceThresholdWithUnit" actualDistanceThresholdWithUnit $ InMaybe $ Min @Distance (Distance 0 Meter),
      validateField "actualDistanceThresholdOnRideWithUnit" actualDistanceThresholdOnRideWithUnit $ InMaybe $ Min @Distance (Distance 0 Meter),
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ Min @Int 1,
      validateField "maxParallelSearchRequestsOnRide" maxParallelSearchRequestsOnRide $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ Min @Seconds 1,
      validateField "radiusShrinkValueForDriversOnRide" radiusShrinkValueForDriversOnRide $ Min @Meters 1,
      validateField "driverToDestinationDistanceThreshold" driverToDestinationDistanceThreshold $ Min @Meters 1,
      validateField "radiusShrinkValueForDriversOnRideWithUnit" radiusShrinkValueForDriversOnRideWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverToDestinationDistanceThresholdWithUnit" driverToDestinationDistanceThresholdWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverToDestinationDuration" driverToDestinationDuration $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver intelligent pool config --------------

-- type DriverIntelligentPoolConfigAPI =
--   "config"
--     :> "driverIntelligentPool"
--     :> Get '[JSON] DriverIntelligentPoolConfigRes

-- data DriverIntelligentPoolConfigRes = DriverIntelligentPoolConfigRes
--   { availabilityTimeWeightage :: Int,
--     availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
--     acceptanceRatioWeightage :: Int,
--     acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
--     cancellationRatioWeightage :: Int,
--     cancellationAndRideFrequencyRatioWindowOption :: SWC.SlidingWindowOptions,
--     minQuotesToQualifyForIntelligentPool :: Int,
--     minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions,
--     intelligentPoolPercentage :: Maybe Int,
--     speedNormalizer :: Double,
--     driverSpeedWeightage :: Int,
--     minLocationUpdates :: Int,
--     locationUpdateSampleTime :: Minutes,
--     defaultDriverSpeed :: Double,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver intelligent pool config update -------

-- type DriverIntelligentPoolConfigUpdateAPI =
--   "config"
--     :> "driverIntelligentPool"
--     :> "update"
--     :> ReqBody '[JSON] DriverIntelligentPoolConfigUpdateReq
--     :> Post '[JSON] APISuccess

-- data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
--   { availabilityTimeWeightage :: Maybe (MandatoryValue Int),
--     availabilityTimeWindowOption :: Maybe SWC.SlidingWindowOptions, -- value wrapper make no sense for lists and objects
--     acceptanceRatioWeightage :: Maybe (MandatoryValue Int),
--     acceptanceRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
--     cancellationRatioWeightage :: Maybe (MandatoryValue Int),
--     cancellationAndRideFrequencyRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
--     minQuotesToQualifyForIntelligentPool :: Maybe (MandatoryValue Int),
--     minQuotesToQualifyForIntelligentPoolWindowOption :: Maybe SWC.SlidingWindowOptions,
--     intelligentPoolPercentage :: Maybe (OptionalValue Int),
--     speedNormalizer :: Maybe (MandatoryValue Double),
--     driverSpeedWeightage :: Maybe (MandatoryValue Int),
--     minLocationUpdates :: Maybe (MandatoryValue Int),
--     locationUpdateSampleTime :: Maybe (MandatoryValue Minutes),
--     defaultDriverSpeed :: Maybe (MandatoryValue Double)
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverIntelligentPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverIntelligentPoolConfigUpdateReq :: Validate DriverIntelligentPoolConfigUpdateReq
validateDriverIntelligentPoolConfigUpdateReq DriverIntelligentPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "availabilityTimeWeightage" availabilityTimeWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust availabilityTimeWindowOption $ \obj ->
        validateObject "availabilityTimeWindowOption" obj validateSlidingWindowOptions,
      validateField "acceptanceRatioWeightage" acceptanceRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust acceptanceRatioWindowOption $ \obj ->
        validateObject "acceptanceRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "cancellationRatioWeightage" cancellationRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust cancellationAndRideFrequencyRatioWindowOption $ \obj ->
        validateObject "cancellationAndRideFrequencyRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "minQuotesToQualifyForIntelligentPool" minQuotesToQualifyForIntelligentPool $ InMaybe $ InValue $ Min @Int 1,
      whenJust minQuotesToQualifyForIntelligentPoolWindowOption $ \obj ->
        validateObject "minQuotesToQualifyForIntelligentPoolWindowOption" obj validateSlidingWindowOptions,
      validateField "intelligentPoolPercentage" intelligentPoolPercentage $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "speedNormalizer" speedNormalizer $ InMaybe $ InValue $ Min @Double 0.0,
      validateField "driverSpeedWeightage" driverSpeedWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      validateField "minLocationUpdates" minLocationUpdates $ InMaybe $ InValue $ Min @Int 0,
      validateField "locationUpdateSampleTime" locationUpdateSampleTime $ InMaybe $ InValue $ Min @Minutes 0,
      validateField "defaultDriverSpeed" defaultDriverSpeed $ InMaybe $ InValue $ Min @Double 0.0
    ]

validateSlidingWindowOptions :: Validate SWC.SlidingWindowOptions
validateSlidingWindowOptions SWC.SlidingWindowOptions {..} =
  validateField "period" period $ Min @Integer 0

---------------------------------------------------------
-- merchant onboarding document config update -----------

-- type DocumentVerificationConfigAPI =
--   "config"
--     :> "onboardingDocument"
--     :> QueryParam "documentType" DocumentType
--     :> QueryParam "vehicleCategory" Category
--     :> Get '[JSON] DocumentVerificationConfigRes

---------------------------------------------------------
-- merchant onboarding document config update -----------

-- type DocumentVerificationConfigUpdateAPI =
--   "config"
--     :> "onboardingDocument"
--     :> "update"
--     :> MandatoryQueryParam "documentType" DocumentType
--     :> MandatoryQueryParam "category" Category
--     :> ReqBody '[JSON] DocumentVerificationConfigUpdateReq
--     :> Post '[JSON] APISuccess

-- data DocumentVerificationConfigUpdateReq = DocumentVerificationConfigUpdateReq
--   { checkExtraction :: Maybe (MandatoryValue Bool),
--     checkExpiry :: Maybe (MandatoryValue Bool),
--     supportedVehicleClasses :: Maybe SupportedVehicleClasses, -- value wrapper make no sense for lists and objects
--     rcNumberPrefix :: Maybe (MandatoryValue Text),
--     rcNumberPrefixList :: Maybe (MandatoryValue [Text]),
--     maxRetryCount :: Maybe (MandatoryValue Int),
--     vehicleClassCheckType :: Maybe (MandatoryValue VehicleClassCheckType)
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DocumentVerificationConfigUpdateReq where
  hideSecrets = identity

---------------------------------------------------------
-- merchant onboarding document config create -----------

-- type DocumentVerificationConfigCreateAPI =
--   "config"
--     :> "onboardingDocument"
--     :> "create"
--     :> MandatoryQueryParam "documentType" DocumentType
--     :> MandatoryQueryParam "category" Category
--     :> ReqBody '[JSON] DocumentVerificationConfigCreateReq
--     :> Post '[JSON] APISuccess

-- data DocumentVerificationConfigCreateReq = DocumentVerificationConfigCreateReq
--   { checkExtraction :: Bool,
--     checkExpiry :: Bool,
--     supportedVehicleClasses :: SupportedVehicleClasses,
--     rcNumberPrefix :: Text,
--     rcNumberPrefixList :: Maybe [Text],
--     maxRetryCount :: Int,
--     vehicleClassCheckType :: VehicleClassCheckType,
--     isDefaultEnabledOnManualVerification :: Maybe Bool,
--     isImageValidationRequired :: Maybe Bool,
--     doStrictVerifcation :: Maybe Bool
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DocumentVerificationConfigCreateReq where
  hideSecrets = identity

---------------------------------------------------------
-- Create Fare Policy -----------------------------------

-- type CreateFPDriverExtraFee =
--   "config"
--     :> "farePolicy"
--     :> Capture "farePolicyId" (Id Common.FarePolicy)
--     :> "driverExtraFeeBounds"
--     :> "create"
--     :> MandatoryQueryParam "startDistance" Meters
--     :> QueryParam "startDistanceValue" HighPrecDistance
--     :> QueryParam "distanceUnit" DistanceUnit
--     :> ReqBody '[JSON] CreateFPDriverExtraFeeReq
--     :> Post '[JSON] APISuccess

-- data CreateFPDriverExtraFeeReq = CreateFPDriverExtraFeeReq
--   { minFee :: Money,
--     maxFee :: Money,
--     stepFee :: Money,
--     defaultStepFee :: Money,
--     minFeeWithCurrency :: Maybe PriceAPIEntity,
--     maxFeeWithCurrency :: Maybe PriceAPIEntity,
--     stepFeeWithCurrency :: Maybe PriceAPIEntity,
--     defaultStepFeeWithCurrency :: Maybe PriceAPIEntity
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateFPDriverExtraFeeReq where
  hideSecrets = identity

---------------------------------------------------------
-- Update Fare Policy -----------------------------------

-- type UpdateFPDriverExtraFee =
--   "config"
--     :> "farePolicy"
--     :> Capture "farePolicyId" (Id Common.FarePolicy)
--     :> "driverExtraFeeBounds"
--     :> "update"
--     :> MandatoryQueryParam "startDistance" Meters
--     :> QueryParam "startDistanceValue" HighPrecDistance
--     :> QueryParam "distanceUnit" DistanceUnit
--     :> ReqBody '[JSON] CreateFPDriverExtraFeeReq
--     :> Post '[JSON] APISuccess

-- type UpdateFPPerExtraKmRate =
--   "config"
--     :> "farePolicy"
--     :> Capture "farePolicyId" (Id Common.FarePolicy)
--     :> Capture "startDistance" Meters -- FIXME distance units?
--     :> "perExtraKmRate"
--     :> "update"
--     :> ReqBody '[JSON] UpdateFPPerExtraKmRateReq
--     :> Post '[JSON] APISuccess

-- data UpdateFPPerExtraKmRateReq = UpdateFPPerExtraKmRateReq
--   { perExtraKmRate :: HighPrecMoney,
--     perExtraKmRateWithCurrency :: Maybe PriceAPIEntity
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateFPPerExtraKmRateReq where
  hideSecrets = identity

-- type UpdateFarePolicy =
--   "config"
--     :> "farePolicy"
--     :> Capture "farePolicyId" (Id Common.FarePolicy)
--     :> "update"
--     :> ReqBody '[JSON] UpdateFarePolicyReq
--     :> Post '[JSON] APISuccess

-- data UpdateFarePolicyReq = UpdateFarePolicyReq
--   { serviceCharge :: Maybe Money,
--     serviceChargeWithCurrency :: Maybe PriceAPIEntity,
--     nightShiftBounds :: Maybe NightShiftBounds,
--     allowedTripDistanceBounds :: Maybe AllowedTripDistanceBoundsAPIEntity,
--     govtCharges :: Maybe Double,
--     perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
--     tollCharges :: Maybe HighPrecMoney,
--     perMinuteRideExtraTimeChargeWithCurrency :: Maybe PriceAPIEntity,
--     congestionChargeMultiplier :: Maybe CongestionChargeMultiplierAPIEntity,
--     description :: Maybe Text,
--     baseDistance :: Maybe Meters,
--     baseDistanceWithUnit :: Maybe Distance,
--     baseFare :: Maybe Money,
--     deadKmFare :: Maybe Money,
--     baseFareWithCurrency :: Maybe PriceAPIEntity,
--     deadKmFareWithCurrency :: Maybe PriceAPIEntity,
--     waitingCharge :: Maybe WaitingChargeAPIEntity,
--     waitingChargeInfo :: Maybe WaitingChargeInfoAPIEntity,
--     freeWaitingTime :: Maybe Minutes,
--     nightShiftCharge :: Maybe NightShiftChargeAPIEntity
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateFarePolicyReq where
  hideSecrets = identity

-- data WaitingChargeInfoAPIEntity = WaitingChargeInfoAPIEntity
--   { freeWaitingTime :: Minutes,
--     waitingCharge :: WaitingChargeAPIEntity
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data WaitingChargeAPIEntity
--   = PerMinuteWaitingCharge HighPrecMoney
--   | ConstantWaitingCharge Money
--   | PerMinuteWaitingChargeWithCurrency PriceAPIEntity
--   | ConstantWaitingChargeWithCurrency PriceAPIEntity
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data CongestionChargeMultiplierAPIEntity
--   = BaseFareAndExtraDistanceFare Centesimal
--   | ExtraDistanceFare Centesimal
--   deriving stock (Show, Eq, Read, Ord, Generic)
--   deriving anyclass (FromJSON, ToJSON, ToSchema)

-- data NightShiftChargeAPIEntity
--   = ProgressiveNightShiftCharge Float
--   | ConstantNightShiftCharge Money
--   | ConstantNightShiftChargeWithCurrency PriceAPIEntity
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data NightShiftBounds = NightShiftBounds
--   { nightShiftStart :: TimeOfDay,
--     nightShiftEnd :: TimeOfDay
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- data AllowedTripDistanceBoundsAPIEntity = AllowedTripDistanceBoundsAPIEntity
--   { maxAllowedTripDistance :: Meters,
--     minAllowedTripDistance :: Meters,
--     maxAllowedTripDistanceWithUnit :: Maybe Distance,
--     minAllowedTripDistanceWithUnit :: Maybe Distance
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

--- Upsert fare policy using csv file ----
type UpsertFarePolicyAPI =
  "config"
    :> "farePolicy"
    :> "upsert"
    :> MultipartForm Tmp UpsertFarePolicyReq
    :> Post '[JSON] UpsertFarePolicyResp

-- data UpsertFarePolicyResp = UpsertFarePolicyResp
--   { unprocessedFarePolicies :: [Text],
--     success :: Text
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- newtype UpsertFarePolicyReq = UpsertFarePolicyReq
--   { file :: FilePath
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UpsertFarePolicyReq where
  fromMultipart form = do
    UpsertFarePolicyReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertFarePolicyReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

instance HideSecrets UpsertFarePolicyReq where
  hideSecrets = identity

---- generic trigger for schedulers ----

-- type SchedulerTriggerAPI =
--   "scheduler"
--     :> "trigger"
--     :> ReqBody '[JSON] SchedulerTriggerReq
--     :> Post '[JSON] APISuccess

-- data SchedulerTriggerReq = SchedulerTriggerReq
--   { scheduledAt :: Maybe UTCTime,
--     jobName :: Maybe JobName,
--     jobData :: Text
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data JobName = BadDebtCalculationTrigger | DriverFeeCalculationTrigger | SendManualPaymentLinkTrigger | ReferralPayoutTrigger
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets SchedulerTriggerReq where
  hideSecrets = identity

--
-- Update Onboarding Vehicle Variant Mapping
--
type UpdateOnboardingVehicleVariantMappingAPI =
  "updateOnboardingVehicleVariantMapping"
    :> MultipartForm Tmp UpdateOnboardingVehicleVariantMappingReq
    :> Post '[JSON] APISuccess

-- newtype UpdateOnboardingVehicleVariantMappingReq = UpdateOnboardingVehicleVariantMappingReq
--   { file :: FilePath
--   }
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp UpdateOnboardingVehicleVariantMappingReq where
  fromMultipart form = do
    UpdateOnboardingVehicleVariantMappingReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpdateOnboardingVehicleVariantMappingReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

instance HideSecrets UpdateOnboardingVehicleVariantMappingReq where
  hideSecrets = identity
