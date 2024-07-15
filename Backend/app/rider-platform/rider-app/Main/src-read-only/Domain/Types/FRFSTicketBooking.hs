{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketBooking where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketBooking = FRFSTicketBooking
  { _type :: Domain.Types.FRFSQuote.FRFSQuoteType,
    bppBankAccountNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppBankCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppItemId :: Kernel.Prelude.Text,
    bppOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bppSubscriberId :: Kernel.Prelude.Text,
    bppSubscriberUrl :: Kernel.Prelude.Text,
    cancellationCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    customerCancelled :: Kernel.Prelude.Bool,
    discountedTickets :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatedPrice :: Kernel.Types.Common.Price,
    eventDiscountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    fromStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    isBookingCancellable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    partnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization),
    partnerOrgTransactionId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrgTransaction),
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    price :: Kernel.Types.Common.Price,
    providerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    quantity :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    stationsJson :: Kernel.Prelude.Text,
    status :: Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    toStationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Domain.Types.Station.FRFSVehicleType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data FRFSTicketBookingStatus = NEW | APPROVED | PAYMENT_PENDING | CONFIRMING | FAILED | CONFIRMED | CANCELLED | COUNTER_CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FRFSTicketBookingStatus)
