{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSTicketBooking where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSTicketBookingT f = FRFSTicketBookingT
  { _type :: B.C f Domain.Types.FRFSQuote.FRFSQuoteType,
    bppBankAccountNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppBankCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppItemId :: B.C f Kernel.Prelude.Text,
    bppOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bppSubscriberId :: B.C f Kernel.Prelude.Text,
    bppSubscriberUrl :: B.C f Kernel.Prelude.Text,
    cancellationCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    customerCancelled :: B.C f Kernel.Prelude.Bool,
    discountedTickets :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    estimatedPrice :: B.C f Kernel.Types.Common.HighPrecMoney,
    eventDiscountAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    finalPrice :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    fromStationId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isBookingCancellable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    partnerOrgTransactionId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payerVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentTxnId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    providerDescription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerId :: B.C f Kernel.Prelude.Text,
    providerName :: B.C f Kernel.Prelude.Text,
    quantity :: B.C f Kernel.Prelude.Int,
    quoteId :: B.C f Kernel.Prelude.Text,
    refundAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    riderId :: B.C f Kernel.Prelude.Text,
    searchId :: B.C f Kernel.Prelude.Text,
    stationsJson :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus,
    toStationId :: B.C f Kernel.Prelude.Text,
    validTill :: B.C f Kernel.Prelude.UTCTime,
    vehicleType :: B.C f Domain.Types.Station.FRFSVehicleType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSTicketBookingT where
  data PrimaryKey FRFSTicketBookingT f = FRFSTicketBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSTicketBookingId . id

type FRFSTicketBooking = FRFSTicketBookingT Identity

$(enableKVPG ''FRFSTicketBookingT ['id] [['bppOrderId], ['quoteId], ['riderId], ['searchId]])

$(mkTableInstances ''FRFSTicketBookingT "frfs_ticket_booking")
