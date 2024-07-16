module Storage.Queries.TicketBookingServiceExtra where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import qualified Domain.Types.BusinessHour as DTBH
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBookingService as DTBS
import qualified Domain.Types.TicketService as DTS
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.TicketBookingService as BeamR
import Storage.Queries.OrphanInstances.TicketBookingService ()

-- Extra code goes here --

findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.Id DTBS.TicketBookingService] ->
  m [DTBS.TicketBookingService]
findByIds ticketBookingServiceIds = do
  findAllWithKV [Se.Is BeamR.id $ Se.In $ Id.getId <$> ticketBookingServiceIds]

findByShortIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id.ShortId DTBS.TicketBookingService] ->
  m [DTBS.TicketBookingService]
findByShortIds ticketBookingServiceShortIds = do
  findAllWithKV [Se.Is BeamR.shortId $ Se.In $ Id.getShortId <$> ticketBookingServiceShortIds]

findByVisitDateAndStatusAndServiceIdAndBtype ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Data.Time.Calendar.Day -> DTBS.ServiceStatus -> Id.Id DTS.TicketService -> DTBH.BusinessHourType -> Maybe Kernel.Prelude.UTCTime -> m [DTBS.TicketBookingService])
findByVisitDateAndStatusAndServiceIdAndBtype visitDate status (Id.Id ticketServiceId) btype expiryDate = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamR.visitDate $ Se.Eq visitDate,
          Se.Is BeamR.status $ Se.Eq status,
          Se.Is BeamR.ticketServiceId $ Se.Eq ticketServiceId,
          Se.Is BeamR.btype $ Se.Eq btype,
          Se.Is BeamR.expiryDate $ Se.GreaterThanOrEq expiryDate
        ]
    ]

findAllTicketBookingServiceList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id.Id DTBS.TicketBookingService ->
  Maybe Text ->
  Maybe DTBS.ServiceStatus ->
  m [DTBS.TicketBookingService]
findAllTicketBookingServiceList merchantOperatingCityId mbLimit mbOffset mbFrom mbTo ticketServiceId mbShortId mbStatus = do
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
                ( \tickekBookingService ->
                    ( maybe (B.sqlBool_ $ B.val_ True) (\status -> tickekBookingService.status B.==?. B.val_ status) mbStatus
                    )
                      B.&&?. (tickekBookingService.merchantOperatingCityId B.==?. B.val_ (Id.getId merchantOperatingCityId))
                      B.&&?. (tickekBookingService.ticketServiceId B.==?. B.val_ (Id.getId ticketServiceId))
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\ticketBookingShortId -> tickekBookingService.shortId B.==?. B.val_ ticketBookingShortId) mbShortId
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultFrom -> B.sqlBool_ $ tickekBookingService.createdAt B.>=. B.val_ defaultFrom) mbFrom
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultTo -> B.sqlBool_ $ tickekBookingService.createdAt B.<=. B.val_ defaultTo) mbTo
                )
                (B.all_ (BeamCommon.tickekBookingService BeamCommon.atlasDB))
  case res of
    Right res' -> catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
