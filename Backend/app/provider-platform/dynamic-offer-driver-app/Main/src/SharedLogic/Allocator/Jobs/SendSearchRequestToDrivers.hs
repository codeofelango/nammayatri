{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import Domain.Types.Booking (BookingStatus (..))
import Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.SearchTry (SearchTry)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool hiding (getDriverPoolConfig)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.SearchTry as SST
import Storage.Cac.DriverPoolConfig (getDriverPoolConfig)
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types
import Utils.Common.Cac.KeyNameConstants

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    MonadFlow m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  let searchTryId = jobData.searchTryId
  searchTry <- B.runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  searchReq <- B.runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleServiceTier searchTry.tripCategory (fromMaybe SL.Default searchReq.area) jobData.estimatedRideDistance (Just (TransactionId (Id searchReq.transactionId)))
  goHomeCfg <- CGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId)))
  tripQuoteDetails <- do
    let estimateIds = if length searchTry.estimateIds == 0 then [searchTry.estimateId] else searchTry.estimateIds
    estimateIds `forM` \estimateId -> do
      if DTC.isDynamicOfferTrip searchTry.tripCategory
        then do
          estimate <- B.runInReplica $ QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
          let mbDriverExtraFeeBounds = ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
              driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
              driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          SST.buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId
        else do
          quote <- B.runInReplica $ QQuote.findById (Id estimateId) >>= fromMaybeM (QuoteNotFound estimateId)
          let mbDriverExtraFeeBounds = ((,) <$> searchReq.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> quote.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
              driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> quote.farePolicy)
              driverParkingCharge = join $ (.parkingCharge) <$> quote.farePolicy
          SST.buildTripQuoteDetail searchReq quote.tripCategory quote.vehicleServiceTier quote.vehicleServiceTierName (quote.estimatedFare + fromMaybe 0 searchTry.customerExtraFee) Nothing (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge quote.id.getId
  let driverSearchBatchInput =
        DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq,
            tripQuoteDetails,
            customerExtraFee = searchTry.customerExtraFee,
            messageId = searchTry.messageId,
            isRepeatSearch = False,
            customerPhoneNum = Nothing
          }
  (res, _, _) <- sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg
  return res

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DriverPoolConfig ->
  SearchTry ->
  DriverSearchBatchInput m ->
  GoHomeConfig ->
  m (ExecutionResult, PoolType, Maybe Seconds)
sendSearchRequestToDrivers' driverPoolConfig searchTry driverSearchBatchInput goHomeCfg = do
  -- In case of static offer flow we will have booking created before driver ride request is sent
  mbBooking <- if DTC.isDynamicOfferTrip searchTry.tripCategory then pure Nothing else QRB.findByQuoteId searchTry.estimateId
  handler (handle mbBooking) goHomeCfg
  where
    handle mbBooking =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchTry.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchTry.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig driverSearchBatchInput.searchReq searchTry driverSearchBatchInput.tripQuoteDetails,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers driverSearchBatchInput.tripQuoteDetails driverSearchBatchInput.searchReq searchTry driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          setBatchDurationLock = I.setBatchDurationLock searchTry.id driverPoolConfig.singleBatchProcessTime,
          createRescheduleTime = I.createRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter driverSearchBatchInput.merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter driverSearchBatchInput.merchant.name,
                putTaskDuration = Metrics.putTaskDuration driverSearchBatchInput.merchant.name
              },
          isSearchTryValid = I.isSearchTryValid searchTry.id,
          initiateDriverSearchBatch = SST.initiateDriverSearchBatch driverSearchBatchInput,
          isScheduledBooking = searchTry.isScheduled,
          customerPhoneNum = driverSearchBatchInput.customerPhoneNum,
          cancelSearchTry = I.cancelSearchTry searchTry.id,
          isBookingValid = do
            case mbBooking of
              Just booking -> booking.status `notElem` [COMPLETED, CANCELLED]
              Nothing -> True,
          cancelBookingIfApplies = do
            whenJust mbBooking $ \booking -> do
              SBooking.cancelBooking booking Nothing driverSearchBatchInput.merchant
        }
