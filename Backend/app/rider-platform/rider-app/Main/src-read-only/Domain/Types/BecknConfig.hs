{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BecknConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BecknConfig = BecknConfig
  { bapIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    buyerFinderFee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    collectedBy :: Domain.Types.BecknConfig.PaymentCollectedBy,
    confirmBufferTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    confirmTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    domain :: Kernel.Prelude.Text,
    gatewayUrl :: Servant.Client.Core.BaseUrl,
    id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    initTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ratingTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    registryUrl :: Servant.Client.Core.BaseUrl,
    searchTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementWindow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    statusTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    subscriberId :: Kernel.Prelude.Text,
    subscriberUrl :: Servant.Client.Core.BaseUrl,
    trackTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    uniqueKeyId :: Kernel.Prelude.Text,
    vehicleCategory :: Domain.Types.BecknConfig.VehicleCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PaymentCollectedBy = BAP | BPP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleCategory = CAB | AUTO_RICKSHAW | METRO | MOTORCYCLE | AMBULANCE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PaymentCollectedBy)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleCategory)
