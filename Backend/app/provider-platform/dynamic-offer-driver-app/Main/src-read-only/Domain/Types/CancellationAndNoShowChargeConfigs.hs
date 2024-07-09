{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationAndNoShowChargeConfigs where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common
import qualified Tools.Beam.UtilsTH

data CancellationAndNoShowChargeConfigs = CancellationAndNoShowChargeConfigs
  { currency :: Kernel.Utils.Common.Currency,
    freeCancellationTimeSeconds :: Kernel.Types.Common.Seconds,
    id :: Kernel.Types.Id.Id Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs,
    maxCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    maxWaitingTimeAtPickupSeconds :: Kernel.Types.Common.Seconds,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMetreCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMinuteCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    percentageOfRideFareToBeCharged :: Kernel.Types.Common.HighPrecMoney,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
