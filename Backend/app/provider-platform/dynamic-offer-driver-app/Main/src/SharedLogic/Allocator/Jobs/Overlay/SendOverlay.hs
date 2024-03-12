{-# OPTIONS_GHC -Wno-type-defaults #-}

module SharedLogic.Allocator.Jobs.Overlay.SendOverlay where

import Data.List (nub)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverInformation as DTDI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.Overlay as DOverlay
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import EulerHS.Prelude hiding (id)
import Kernel.External.Types
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.Allocator as SAllocator
import qualified SharedLogic.DriverFee as SLDriverFee
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as TN

sendOverlayToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    ServiceFlow m r,
    Esq.Transactionable m,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'SAllocator.SendOverlay ->
  m ExecutionResult
sendOverlayToDriver (Job {id, jobInfo}) = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      serviceName = fromMaybe DPlan.YATRI_SUBSCRIPTION jobData.serviceName
      mbMerchantOperatingCityId = jobData.merchantOperatingCityId
      jobId = id
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOperatingCityId merchant Nothing
  driverIds <- nub <$> getBatchedDriverIds merchantId merchantOpCityId jobId jobData.condition jobData.freeTrialDays jobData.timeDiffFromUtc jobData.driverPaymentCycleDuration jobData.driverPaymentCycleStartTime jobData.overlayBatchSize serviceName
  logInfo $ "The Job " <> jobId.getId <> " is scheduled for following driverIds " <> show driverIds
  driverIdsLength <- getSendOverlaySchedulerDriverIdsLength merchantOpCityId jobId

  if driverIdsLength > 0 || (invoiceOverlayCondition jobData.condition && not (null driverIds))
    then do
      mapM_ (sendOverlayAccordingToCondition jobData serviceName) driverIds
      ReSchedule . addUTCTime 1 <$> getCurrentTime
    else do
      unless (null driverIds) $ mapM_ (sendOverlayAccordingToCondition jobData serviceName) driverIds
      case jobData.rescheduleInterval of
        Just interval -> do
          lastScheduledTime <- getLastScheduledJobTime merchantOpCityId jobId jobData.scheduledTime jobData.timeDiffFromUtc
          let newScheduledTime = addUTCTime (fromIntegral interval) lastScheduledTime
          setLastScheduledJobTime merchantOpCityId jobId newScheduledTime
          pure (ReSchedule newScheduledTime)
        Nothing -> return Complete
  where
    invoiceOverlayCondition condition = condition == DOverlay.InvoiceGenerated DPlan.MANUAL || condition == DOverlay.InvoiceGenerated DPlan.AUTOPAY
    sendOverlayAccordingToCondition jobDataInfo serviceName driverId = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      case jobDataInfo.condition of
        DOverlay.PaymentOverdueGreaterThan limit -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when (manualDues > fromIntegral limit) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues
        DOverlay.PaymentOverdueBetween rLimit lLimit -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when ((fromIntegral rLimit <= manualDues) && (manualDues <= fromIntegral lLimit)) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues
        DOverlay.InvoiceGenerated _ -> do
          manualDues <- getAllManualDuesWithoutFilter driverId serviceName
          sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues
        DOverlay.FreeTrialDaysLeft _ -> do
          driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
          currUdf1 <- getCurrentAutoPayStatusUDF driverInfo
          when (currUdf1 == jobDataInfo.udf1) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 0
        DOverlay.BlockedDrivers -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when (manualDues > 0) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues
        _ -> pure ()

    getCurrentAutoPayStatusUDF driverInfo = do
      case driverInfo.autoPayStatus of
        Nothing -> return $ Just "PLAN_NOT_SELECTED"
        Just DTDI.ACTIVE -> return $ Just ""
        Just DTDI.PENDING -> return $ Just ""
        _ -> return $ Just "AUTOPAY_NOT_SET"

    getAllManualDuesWithoutFilter driverId serviceName = do
      pendingDriverFees <- QDF.findDriverFeeByTypeStatusAndServiceName driverId [DDF.RECURRING_INVOICE] [DDF.PAYMENT_OVERDUE] serviceName
      return $ sum $ map (\dueInvoice -> SLDriverFee.roundToHalf (fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

    getManualDues driverId timeDiffFromUtc driverFeeOverlaySendingTimeLimitInDays serviceName = do
      windowEndTime <- getLocalCurrentTime timeDiffFromUtc
      let windowStartTime = addUTCTime (-1 * fromIntegral driverFeeOverlaySendingTimeLimitInDays * 86400) (UTCTime (utctDay windowEndTime) (secondsToDiffTime 0))
      pendingDriverFees <- QDF.findDriverFeeByTypeStatusAndServiceName driverId [DDF.RECURRING_INVOICE] [DDF.PAYMENT_OVERDUE] serviceName
      let filteredDriverFees = filter (\driverFee -> driverFee.startTime >= windowStartTime) pendingDriverFees
      return $
        if null filteredDriverFees
          then 0
          else sum $ map (\dueInvoice -> SLDriverFee.roundToHalf (fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

sendOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Text -> Maybe Text -> HighPrecMoney -> m ()
sendOverlay driver overlayKey udf1 amount = do
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdf driver.merchantOperatingCityId overlayKey (fromMaybe ENGLISH driver.language) udf1
  whenJust mOverlay $ \overlay -> do
    let okButtonText = T.replace (templateText "dueAmount") (show amount) <$> overlay.okButtonText
    let description = T.replace (templateText "dueAmount") (show amount) <$> overlay.description
    fork ("sending overlay to driver with driverId " <> (show driver.id)) $ do
      TN.sendOverlay driver.merchantOperatingCityId driver.id driver.deviceToken $ TN.mkOverlayReq overlay description okButtonText overlay.cancelButtonText overlay.endPoint

getSendOverlaySchedulerDriverIdsLength :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> m Integer
getSendOverlaySchedulerDriverIdsLength merchantOpCityId jobId = Hedis.lLen $ makeSendOverlaySchedulerDriverIdsKey merchantOpCityId jobId

getFirstNSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> Integer -> m [Id DP.Person]
getFirstNSendOverlaySchedulerDriverIds merchantOpCityId jobId num = Hedis.lRange (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId jobId) 0 (num -1)

deleteNSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> Integer -> m ()
deleteNSendOverlaySchedulerDriverIds merchantOpCityId jobId num = Hedis.lTrim (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId jobId) num (-1)

addSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> NonEmpty (Id DP.Person) -> m ()
addSendOverlaySchedulerDriverIds merchantOpCityId jobId = Hedis.rPush (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId jobId)

makeSendOverlaySchedulerDriverIdsKey :: Id DMOC.MerchantOperatingCity -> Id AnyJob -> Text
makeSendOverlaySchedulerDriverIdsKey merchantOpCityId jobId = "SendOverlayScheduler:merchantOpCityId-" <> merchantOpCityId.getId <> ":jobId-" <> jobId.getId

getLastScheduledJobTime :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> TimeOfDay -> Seconds -> m UTCTime
getLastScheduledJobTime merchantOpCityId jobId scheduledTime timeDiffFromUtc = do
  Hedis.get (makeLastScheduledTimeJobKey merchantOpCityId jobId) >>= \case
    Nothing -> do
      now <- getLocalCurrentTime timeDiffFromUtc
      let lastScheduledTime = addUTCTime (fromIntegral $ -1 * timeDiffFromUtc) (UTCTime (utctDay now) (timeOfDayToTime scheduledTime))
      setLastScheduledJobTime merchantOpCityId jobId lastScheduledTime
      pure lastScheduledTime
    Just lastScheduledTime -> pure lastScheduledTime

setLastScheduledJobTime :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Id AnyJob -> UTCTime -> m ()
setLastScheduledJobTime merchantOpCityId jobId = Hedis.set (makeLastScheduledTimeJobKey merchantOpCityId jobId)

makeLastScheduledTimeJobKey :: Id DMOC.MerchantOperatingCity -> Id AnyJob -> Text
makeLastScheduledTimeJobKey merchantOpCityId jobId = "SendOverlayScheduler:lastScheduledTime:merchantOpCityId-" <> merchantOpCityId.getId <> ":jobId" <> jobId.getId

getBatchedDriverIds ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id AnyJob ->
  DOverlay.OverlayCondition ->
  Int ->
  Seconds ->
  NominalDiffTime ->
  NominalDiffTime ->
  Int ->
  DPlan.ServiceNames ->
  m [Id DP.Person]
getBatchedDriverIds merchantId merchantOperatingCityId jobId condition freeTrialDays timeDiffFromUtc driverPaymentCycleDuration driverPaymentCycleStartTime overlayBatchSize serviceName = do
  case condition of
    DOverlay.InvoiceGenerated paymentMode -> do
      now <- getLocalCurrentTime timeDiffFromUtc
      let potentialStartToday = addUTCTime driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
          startTime = addUTCTime (-1 * driverPaymentCycleDuration) potentialStartToday
          endTime = addUTCTime 120 potentialStartToday
      driverFees <- QDF.findWindowsAndServiceNameWithFeeTypeAndLimitAndServiceName merchantId merchantOperatingCityId startTime endTime (getFeeType paymentMode) overlayBatchSize serviceName
      let driverIds = driverFees <&> (.driverId)
      void $ QDF.updateDriverFeeOverlayScheduledByServiceName driverIds True startTime endTime serviceName
      return driverIds
    _ -> do
      -- except for the invoice generated condition, for rest of the conditions the all the required driverIds will be fetched one time and will be cached
      driverIdsLength <- getSendOverlaySchedulerDriverIdsLength merchantOperatingCityId jobId
      when (driverIdsLength < 1) do
        driverIds <- case condition of
          DOverlay.PaymentOverdueGreaterThan _ -> QDI.fetchAllDriversWithPaymentPending merchantOperatingCityId <&> (<&> (.driverId))
          DOverlay.PaymentOverdueBetween _ _ -> QDI.fetchAllDriversWithPaymentPending merchantOperatingCityId <&> (<&> (.driverId))
          DOverlay.BlockedDrivers -> QDI.fetchAllBlockedDriversWithSubscribedFalse merchantOperatingCityId <&> (<&> (.driverId))
          DOverlay.FreeTrialDaysLeft numOfDays -> do
            now <- getCurrentTime
            let startTime = addUTCTime (-1 * fromIntegral timeDiffFromUtc) $ addUTCTime (-1 * 86400 * fromIntegral (freeTrialDays - (numOfDays - 1))) (UTCTime (utctDay now) (secondsToDiffTime 0))
                endTime = addUTCTime 86400 startTime
            QDI.findAllByEnabledAtInWindow merchantOperatingCityId (Just startTime) (Just endTime) <&> (<&> (.driverId))
          _ -> return []
        whenJust (nonEmpty driverIds) $ addSendOverlaySchedulerDriverIds merchantOperatingCityId jobId
      batchedDriverIds <- getFirstNSendOverlaySchedulerDriverIds merchantOperatingCityId jobId $ fromIntegral overlayBatchSize
      deleteNSendOverlaySchedulerDriverIds merchantOperatingCityId jobId $ fromIntegral overlayBatchSize
      return batchedDriverIds
  where
    getFeeType paymentMode = case paymentMode of
      DPlan.AUTOPAY -> DDF.RECURRING_EXECUTION_INVOICE
      DPlan.MANUAL -> DDF.RECURRING_INVOICE

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"
