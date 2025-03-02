{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBooking where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBooking.FRFSTicketBooking] -> m ())
createMany = traverse_ create

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByRiderId limit offset riderId = do findAllWithOptionsKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)] (Se.Desc Beam.createdAt) limit offset

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> m [Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByBppOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByBppOrderId bppOrderId = do findOneWithKV [Se.Is Beam.bppOrderId $ Se.Eq bppOrderId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByQuoteId quoteId = do findOneWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

findBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findBySearchId searchId = do findOneWithKV [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

updateBPPOrderIdAndStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateBPPOrderIdAndStatusById bppOrderId status id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.bppOrderId bppOrderId, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateBppBankDetailsById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateBppBankDetailsById bppBankAccountNumber bppBankCode id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.bppBankAccountNumber bppBankAccountNumber, Se.Set Beam.bppBankCode bppBankCode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCustomerCancelledByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateCustomerCancelledByBookingId customerCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.customerCancelled customerCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFinalPriceById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateFinalPriceById finalPrice id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsBookingCancellableByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateIsBookingCancellableByBookingId isBookingCancellable id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isBookingCancellable isBookingCancellable, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePriceById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updatePriceById price id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price), Se.Set Beam.price ((.amount) price), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRefundCancellationChargesAndIsCancellableByBookingId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateRefundCancellationChargesAndIsCancellableByBookingId refundAmount cancellationCharges isBookingCancellable id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusValidTillAndPaymentTxnById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateStatusValidTillAndPaymentTxnById status validTill paymentTxnId id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.validTill validTill, Se.Set Beam.paymentTxnId paymentTxnId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateValidTillById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateValidTillById validTill id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.validTill validTill, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.bppBankAccountNumber bppBankAccountNumber,
      Se.Set Beam.bppBankCode bppBankCode,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppOrderId bppOrderId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.customerCancelled customerCancelled,
      Se.Set Beam.discountedTickets discountedTickets,
      Se.Set Beam.estimatedPrice ((.amount) estimatedPrice),
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice),
      Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.paymentTxnId paymentTxnId,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.quantity quantity,
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.status status,
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  fromTType' (Beam.FRFSTicketBookingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBooking.FRFSTicketBooking
          { _type = _type,
            bppBankAccountNumber = bppBankAccountNumber,
            bppBankCode = bppBankCode,
            bppItemId = bppItemId,
            bppOrderId = bppOrderId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            cancellationCharges = cancellationCharges,
            customerCancelled = customerCancelled,
            discountedTickets = discountedTickets,
            estimatedPrice = Kernel.Types.Common.mkPrice currency estimatedPrice,
            eventDiscountAmount = eventDiscountAmount,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            isBookingCancellable = isBookingCancellable,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            paymentTxnId = paymentTxnId,
            price = Kernel.Types.Common.mkPrice currency price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            quoteId = Kernel.Types.Id.Id quoteId,
            refundAmount = refundAmount,
            riderId = Kernel.Types.Id.Id riderId,
            searchId = Kernel.Types.Id.Id searchId,
            stationsJson = stationsJson,
            status = status,
            toStationId = Kernel.Types.Id.Id toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  toTType' (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
    Beam.FRFSTicketBookingT
      { Beam._type = _type,
        Beam.bppBankAccountNumber = bppBankAccountNumber,
        Beam.bppBankCode = bppBankCode,
        Beam.bppItemId = bppItemId,
        Beam.bppOrderId = bppOrderId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.cancellationCharges = cancellationCharges,
        Beam.customerCancelled = customerCancelled,
        Beam.discountedTickets = discountedTickets,
        Beam.estimatedPrice = (.amount) estimatedPrice,
        Beam.eventDiscountAmount = eventDiscountAmount,
        Beam.finalPrice = Kernel.Prelude.fmap (.amount) finalPrice,
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBookingCancellable = isBookingCancellable,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.paymentTxnId = paymentTxnId,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.refundAmount = refundAmount,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.stationsJson = stationsJson,
        Beam.status = status,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
