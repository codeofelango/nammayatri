module Storage.Queries.TicketBookingExtra where

import qualified Database.Beam as B
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBooking as DTB
import qualified Domain.Types.TicketPlace as DTP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import Storage.Beam.TicketBooking as BeamR
import Storage.Queries.OrphanInstances.TicketBooking ()

-- Extra code goes here --
findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTB.TicketBooking] ->
  m [DTB.TicketBooking]
findByIds ticketBookingIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ getId <$> ticketBookingIds]

findAllTicketBookingList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id DTP.TicketPlace ->
  Maybe Text ->
  Maybe DTB.BookingStatus ->
  m [DTB.TicketBooking]
findAllTicketBookingList merchantOperatingCityId mbLimit mbOffset mbFrom mbTo ticketPlaceId mbTicketBookingShortId mbTicketBookingStatus = do
  let limitVal = fromMaybe 5 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getMasterBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.filter_'
                ( \ticketBooking ->
                    ( maybe (B.sqlBool_ $ B.val_ True) (\ticketBookingStatus -> ticketBooking.status B.==?. B.val_ ticketBookingStatus) mbTicketBookingStatus
                    )
                      B.&&?. (ticketBooking.merchantOperatingCityId B.==?. B.val_ (getId merchantOperatingCityId))
                      B.&&?. (ticketBooking.ticketPlaceId B.==?. B.val_ (getId ticketPlaceId))
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\ticketBookingShortId -> ticketBooking.shortId B.==?. B.val_ ticketBookingShortId) mbTicketBookingShortId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ ticketBooking.createdAt B.>=. B.val_ defaultFrom) mbFrom
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ ticketBooking.createdAt B.<=. B.val_ defaultTo) mbTo
                )
                (B.all_ (BeamCommon.ticketBooking BeamCommon.atlasDB))
  case res of
    Right res' -> catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
