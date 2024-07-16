{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderConfig (module Domain.Types.RiderConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RiderConfig as ReExport
import qualified Domain.Types.Extra.RiderConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Email.Types
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderConfig = RiderConfig
  { appUrl :: Kernel.Prelude.Text,
    autoUnblockSafetyCenterAfterDays :: Kernel.Prelude.Int,
    bookingSyncStatusCallSecondsDiffThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    collectAutoCompleteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    collectMMIRouteData :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    distanceWeightage :: Kernel.Prelude.Int,
    emailOtpConfig :: Kernel.Prelude.Maybe Email.Types.EmailOTPConfig,
    enableEmergencyContactAddedMessage :: Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: Kernel.Prelude.Bool,
    enableSupportForSafety :: Kernel.Prelude.Bool,
    exotelAppIdMapping :: Kernel.Prelude.Maybe Domain.Types.Extra.RiderConfig.ExotelMapping,
    exotelStatusCheckScheduler :: Kernel.Prelude.Int,
    hardLimitForSafetyJobs :: Kernel.Prelude.Int,
    incidentReportSupport :: Kernel.Prelude.Bool,
    isAvoidToll :: Kernel.Prelude.Bool,
    ivrTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    kaptureConfig :: IssueManagement.Common.KaptureConfig,
    kaptureQueue :: Kernel.Prelude.Text,
    localPoliceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    placeNameCacheExpiryDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    policeTriggerDelay :: Kernel.Prelude.NominalDiffTime,
    safetyCheckEndTime :: Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: Kernel.Types.Common.Seconds,
    specialZoneRadius :: Kernel.Prelude.Int,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: Kernel.Prelude.Text,
    videoFileSizeUpperLimit :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)
