{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CancellationAndNoShowChargeConfigs where

import qualified Database.Beam as B
import qualified Domain.Types.ServiceTierType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data CancellationAndNoShowChargeConfigsT f = CancellationAndNoShowChargeConfigsT
  { currency :: B.C f Kernel.Utils.Common.Currency,
    freeCancellationTimeSeconds :: B.C f Kernel.Types.Common.Seconds,
    id :: B.C f Kernel.Prelude.Text,
    maxCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    maxWaitingTimeAtPickupSeconds :: B.C f Kernel.Types.Common.Seconds,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    perMetreCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    perMinuteCancellationCharge :: B.C f Kernel.Types.Common.HighPrecMoney,
    percentageOfRideFareToBeCharged :: B.C f Kernel.Types.Common.HighPrecMoney,
    vehicleServiceTierType :: B.C f Domain.Types.ServiceTierType.ServiceTierType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CancellationAndNoShowChargeConfigsT where
  data PrimaryKey CancellationAndNoShowChargeConfigsT f = CancellationAndNoShowChargeConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CancellationAndNoShowChargeConfigsId . id

type CancellationAndNoShowChargeConfigs = CancellationAndNoShowChargeConfigsT Identity

$(enableKVPG ''CancellationAndNoShowChargeConfigsT ['id] [])

$(mkTableInstances ''CancellationAndNoShowChargeConfigsT "cancellation_and_now_show_charge_configs")
