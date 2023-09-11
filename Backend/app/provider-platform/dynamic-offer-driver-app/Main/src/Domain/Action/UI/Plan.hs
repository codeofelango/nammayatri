{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Plan where

import Data.OpenApi (ToSchema (..))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (MandateStatus)
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as P
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (Language (ENGLISH))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as SOrder
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.MerchantConfig as QTC
import qualified Storage.CachedQueries.Plan as QPD
import qualified Storage.CachedQueries.PlanTranslation as CQPTD
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as DI
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications
import Tools.Payment as Payment
import Tools.SMS as Sms hiding (Success)

---------------------------------------------------------------------------------------------------------
--------------------------------------- Request & Response Types ----------------------------------------
---------------------------------------------------------------------------------------------------------

data PlanListAPIRes = PlanListAPIRes
  { list :: [PlanEntity],
    subscriptionStartTime :: UTCTime,
    isLocalized :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanEntity = PlanEntity
  { id :: Text,
    name :: Text,
    description :: Text,
    planFareBreakup :: [PlanFareBreakup],
    freeRideCount :: Int,
    frequency :: Text,
    offers :: [OfferEntity],
    paymentMode :: PaymentMode,
    totalPlanCreditLimit :: Money,
    currentDues :: Money
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanFareBreakup = PlanFareBreakup
  { component :: Text,
    amount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CurrentPlanRes = CurrentPlanRes
  { currentPlanDetails :: Maybe PlanEntity,
    mandateDetails :: Maybe MandateDetailsEntity,
    orderId :: Maybe (Id DOrder.PaymentOrder),
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    subscribed :: Bool,
    planRegistrationDate :: Maybe UTCTime,
    latestAutopayPaymentDate :: Maybe UTCTime,
    latestManualPaymentDate :: Maybe UTCTime,
    isLocalized :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanSubscribeRes = PlanSubscribeRes
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MandateDetailsEntity = MandateDetails
  { status :: MandateStatus,
    startDate :: UTCTime,
    endDate :: UTCTime,
    mandateId :: Text,
    payerVpa :: Maybe Text,
    frequency :: Text,
    maxAmount :: Money,
    autopaySetupDate :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

---------------------------------------------------------------------------------------------------------
--------------------------------------------- Controllers -----------------------------------------------
---------------------------------------------------------------------------------------------------------

-- This API is for listing all the AUTO PAY plans
planList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> Flow PlanListAPIRes
planList (driverId, merchantId) _mbLimit _mbOffset = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  mDriverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId
  plans <- QPD.findByMerchantIdAndPaymentMode merchantId AUTOPAY
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  now <- getCurrentTime
  let mandateSetupDate = fromMaybe now ((.mandateSetupDate) =<< mDriverPlan)
  plansList <-
    mapM
      ( \plan' ->
          if driverInfo.autoPayStatus == Just DI.ACTIVE
            then do convertPlanToPlanEntity driverId mandateSetupDate plan'
            else do convertPlanToPlanEntity driverId now plan'
      )
      plans
  return $
    PlanListAPIRes
      { list = plansList,
        subscriptionStartTime = transporterConfig.subscriptionStartTime,
        isLocalized = Just True
      }

-- This API is for listing current driver plan
currentPlan :: (Id SP.Person, Id DM.Merchant) -> Flow CurrentPlanRes
currentPlan (driverId, _merchantId) = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  mDriverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId
  mPlan <- maybe (pure Nothing) (\p -> QPD.findByIdAndPaymentMode p.planId (getDriverPaymentMode driverInfo.autoPayStatus)) mDriverPlan
  mandateDetailsEntity <- mkMandateDetailEntity (join (mDriverPlan <&> (.mandateId)))

  latestManualPayment <- QDF.findLatestByFeeTypeAndStatus DF.RECURRING_INVOICE [DF.CLEARED, DF.COLLECTED_CASH] driverId
  latestAutopayPayment <- QDF.findLatestByFeeTypeAndStatus DF.RECURRING_EXECUTION_INVOICE [DF.CLEARED] driverId

  now <- getCurrentTime
  let mbMandateSetupDate = mDriverPlan >>= (.mandateSetupDate)
  let mandateSetupDate = maybe now (\date -> if checkIFActiveStatus driverInfo.autoPayStatus then date else now) mbMandateSetupDate
  currentPlanEntity <- maybe (pure Nothing) (convertPlanToPlanEntity driverId mandateSetupDate >=> (pure . Just)) mPlan

  orderId <-
    if driverInfo.autoPayStatus == Just DI.PENDING
      then do
        mbOrder <- SOrder.findLatestByPersonId driverId.getId
        return (mbOrder <&> (.id))
      else return Nothing

  return $
    CurrentPlanRes
      { currentPlanDetails = currentPlanEntity,
        mandateDetails = mandateDetailsEntity,
        autoPayStatus = driverInfo.autoPayStatus,
        subscribed = driverInfo.subscribed,
        orderId,
        latestManualPaymentDate = latestManualPayment <&> (.updatedAt),
        latestAutopayPaymentDate = latestAutopayPayment <&> (.updatedAt),
        planRegistrationDate = mDriverPlan <&> (.createdAt),
        isLocalized = Just True
      }
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

    checkIFActiveStatus (Just DI.ACTIVE) = True
    checkIFActiveStatus _ = False

-- This API is to create a mandate order if the driver has not subscribed to Mandate even once or has Cancelled Mandate from PSP App.
planSubscribe :: Id Plan -> Bool -> (Id SP.Person, Id DM.Merchant) -> Flow PlanSubscribeRes
planSubscribe planId isDashboard (driverId, merchantId) = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus `elem` [Nothing, Just DI.CANCELLED_PSP, Just DI.PAUSED_PSP, Just DI.PENDING]) $ throwError InvalidAutoPayStatus
  plan <- QPD.findByIdAndPaymentMode planId MANUAL >>= fromMaybeM (PlanNotFound planId.getId)
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId

  when (driverInfo.autoPayStatus == Just DI.PAUSED_PSP) $ do
    let mbMandateId = (.mandateId) =<< driverPlan
    whenJust mbMandateId $ \mandateId -> do
      fork "Cancelling paused Mandate" $ do
        void $ Payment.mandateRevoke merchantId (Payment.MandateRevokeReq {mandateId = mandateId.getId})

  unless (driverInfo.autoPayStatus == Just DI.PENDING) $ DI.updateAutoPayStatus (Just DI.PENDING) (cast driverId)
  when (isNothing driverPlan) $ do
    newDriverPlan <- mkDriverPlan plan
    QDPlan.create newDriverPlan
  when (isJust driverPlan) $ do
    unless (driverInfo.autoPayStatus == Just DI.PENDING && maybe False (\dp -> dp.planId == planId) driverPlan) $ QDF.updateRegisterationFeeStatusByDriverId DF.INACTIVE driverId
    QDPlan.updatePlanIdByDriverId driverId planId
  (createOrderResp, orderId) <- createMandateInvoiceAndOrder driverId merchantId plan
  when isDashboard $ do
    let mbPaymentLink = createOrderResp.payment_links
    whenJust mbPaymentLink $ \paymentLinks -> do
      let webPaymentLink = show paymentLinks.web
      smsCfg <- asks (.smsCfg)
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      message <-
        MessageBuilder.buildSendPaymentLink merchantId $
          MessageBuilder.BuildSendPaymentLinkReq
            { paymentLink = webPaymentLink,
              amount = show createOrderResp.sdk_payload.payload.amount
            }
      Sms.sendSMS merchantId (Sms.SendSMSReq message phoneNumber smsCfg.sender)
        >>= Sms.checkSmsResult
  return $
    PlanSubscribeRes
      { orderId = orderId,
        orderResp = createOrderResp
      }
  where
    mkDriverPlan plan = do
      now <- getCurrentTime
      return $
        DriverPlan
          { driverId = cast driverId,
            planId = plan.id,
            planType = plan.paymentMode,
            mandateId = Nothing,
            createdAt = now,
            updatedAt = now,
            mandateSetupDate = Nothing,
            ..
          }

-- This API is to switch between plans of current Payment Method Preference.
planSelect :: Id Plan -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSelect planId (driverId, _) = do
  void $ B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  void $ QPD.findByIdAndPaymentMode planId (getDriverPaymentMode driverInfo.autoPayStatus) >>= fromMaybeM (PlanNotFound planId.getId)
  QDPlan.updatePlanIdByDriverId driverId planId
  return Success
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL

-- This API is to make Mandate Inactive and switch to Manual plan type from Autopay.
planSuspend :: Bool -> (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planSuspend isDashboard (driverId, _merchantId) = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.ACTIVE) $ throwError InvalidAutoPayStatus
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.mandateProcessingLockKey mandate.id.getId) 60 $ do
    QM.updateStatus mandate.id DM.INACTIVE
    QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) MANUAL
    DI.updateAutoPayStatus (Just DI.SUSPENDED) (cast driverId)
  when isDashboard $ notifyPaymentModeManualOnSuspend _merchantId driverId driver.deviceToken
  return Success

-- This API is to make Mandate Active and switch to Autopay plan type. If an only if an Auto Pay plan was paused/cancelled by driver from App.
planResume :: (Id SP.Person, Id DM.Merchant) -> Flow APISuccess
planResume (driverId, _merchantId) = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  unless (driverInfo.autoPayStatus == Just DI.SUSPENDED) $ throwError InvalidAutoPayStatus
  driverPlan <- B.runInReplica $ QDPlan.findByDriverId driverId >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  mandate <- validateInActiveMandateExists driverId driverPlan
  Redis.whenWithLockRedis (DF.mandateProcessingLockKey mandate.id.getId) 60 $ do
    QM.updateStatus mandate.id DM.ACTIVE
    QDPlan.updateMandateSetupDateByDriverId (cast driverPlan.driverId)
    QDPlan.updatePaymentModeByDriverId (cast driverPlan.driverId) AUTOPAY
    DI.updateAutoPayStatus (Just DI.ACTIVE) (cast driverId)
  return Success

---------------------------------------------------------------------------------------------------------
------------------------------------------ Helper Functions ---------------------------------------------
---------------------------------------------------------------------------------------------------------

validateActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ ActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- B.runInReplica $ QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.ACTIVE) $ throwError (ActiveMandateDoNotExist driverId.getId)
      return mandate

validateInActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateInActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ InActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.INACTIVE) $ throwError (InActiveMandateDoNotExist driverId.getId)
      return mandate

createMandateInvoiceAndOrder :: Id SP.Person -> Id DM.Merchant -> Plan -> Flow (Payment.CreateOrderResp, Id DOrder.PaymentOrder)
createMandateInvoiceAndOrder driverId merchantId plan = do
  driverPendingAndDuesFees <- QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  driverRegisterationFee <- QDF.findLatestRegisterationFeeByDriverId (cast driverId)
  transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  now <- getCurrentTime
  let currentDues = sum $ map (\dueInvoice -> fromIntegral dueInvoice.govtCharges + fromIntegral dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) driverPendingAndDuesFees
  case driverRegisterationFee of
    Just registerFee -> do
      invoices <- QINV.findByDriverFeeIdAndActiveStatus registerFee.id
      case invoices of
        [] -> createOrderForDriverFee driverPendingAndDuesFees registerFee currentDues now transporterConfig.mandateValidity
        (inv : resActiveInvoices) -> do
          -- ideally resActiveInvoices should be null in case they are there make them inactive
          mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) resActiveInvoices
          if inv.maxMandateAmount == Just plan.maxAmount
            then SPayment.createOrder (driverId, merchantId) (registerFee : driverPendingAndDuesFees) (Just $ mandateOrder currentDues now transporterConfig.mandateValidity) (Just (inv.id, inv.invoiceShortId))
            else do
              QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE inv.id
              createOrderForDriverFee driverPendingAndDuesFees registerFee currentDues now transporterConfig.mandateValidity
    Nothing -> do
      driverFee <- mkDriverFee
      QDF.create driverFee
      createOrderForDriverFee driverPendingAndDuesFees driverFee currentDues now transporterConfig.mandateValidity
  where
    mandateOrder currentDues now mandateValidity =
      SPayment.MandateOrder
        { maxAmount = max plan.maxAmount currentDues,
          _type = Payment.REQUIRED,
          frequency = Payment.ASPRESENTED,
          mandateStartDate = T.pack $ show $ utcTimeToPOSIXSeconds now,
          mandateEndDate = T.pack $ show $ utcTimeToPOSIXSeconds $ addUTCTime (secondsToNominalDiffTime (fromIntegral (60 * 60 * 24 * 365 * mandateValidity))) now
        }
    createOrderForDriverFee driverPendingAndDuesFees driverFee currentDues now mandateValidity = do
      if not (null driverPendingAndDuesFees)
        then SPayment.createOrder (driverId, merchantId) (driverFee : driverPendingAndDuesFees) (Just $ mandateOrder currentDues now mandateValidity) Nothing
        else do
          SPayment.createOrder (driverId, merchantId) [driverFee] (Just $ mandateOrder currentDues now mandateValidity) Nothing
    mkDriverFee = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DF.DriverFee
          { id = id,
            merchantId = merchantId,
            payBy = now,
            status = DF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DF.PlatformFee (round plan.registrationAmount) 0.0 0.0,
            totalEarnings = 0,
            feeType = DF.MANDATE_REGISTRATION,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            collectedBy = Nothing,
            driverId = cast driverId
          }

convertPlanToPlanEntity :: Id SP.Person -> UTCTime -> Plan -> Flow PlanEntity
convertPlanToPlanEntity driverId applicationDate plan@Plan {..} = do
  dueInvoices <- B.runInReplica $ QDF.findAllPendingAndDueDriverFeeByDriverId driverId
  offers <- Payment.offerList merchantId =<< makeOfferReq applicationDate
  let planFareBreakup = mkPlanFareBreakup offers.offerResp
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbtranslation <- CQPTD.findByPlanIdAndLanguage plan.id (fromMaybe ENGLISH driver.language)
  let translatedName = maybe plan.name (.name) mbtranslation
  let translatedDescription = maybe plan.description (.description) mbtranslation
  planBaseFrequcency <- case planBaseAmount of
    PERRIDE_BASE _ -> return "PER_RIDE"
    DAILY_BASE _ -> return "DAILY"
    WEEKLY_BASE _ -> return "WEEKLY"
    MONTHLY_BASE _ -> return "MONTHLY"
  return
    PlanEntity
      { id = plan.id.getId,
        offers = makeOfferEntity <$> offers.offerResp,
        frequency = planBaseFrequcency,
        name = translatedName,
        description = translatedDescription,
        currentDues = round . sum $ map (\dueInvoice -> fromIntegral dueInvoice.govtCharges + fromIntegral dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst) dueInvoices,
        totalPlanCreditLimit = round maxCreditLimit,
        ..
      }
  where
    makeOfferEntity offer =
      OfferEntity
        { title = offer.offerDescription.title,
          description = offer.offerDescription.description,
          tnc = offer.offerDescription.tnc
        }
    makeOfferReq date = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = plan.maxAmount, currency = Payment.INR}
          customerReq = Payment.OfferCustomer {customerId = driverId.getId, email = driver.email, mobile = Nothing}
      transporterConfig <- QTC.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
      return
        Payment.OfferListReq
          { order = offerOrder,
            customer = Just customerReq,
            planId = plan.id.getId,
            registrationDate = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) date
          }
    mkPlanFareBreakup offers = do
      let baseAmount = case plan.planBaseAmount of
            PERRIDE_BASE amount -> amount
            DAILY_BASE amount -> amount
            WEEKLY_BASE amount -> amount
            MONTHLY_BASE amount -> amount
          (discountAmount, finalOrderAmount) =
            if null offers
              then (0.0, baseAmount)
              else do
                let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers
                (bestOffer.discountAmount, bestOffer.finalOrderAmount)
      [ PlanFareBreakup {component = "INITIAL_BASE_FEE", amount = baseAmount},
        PlanFareBreakup {component = "REGISTRATION_FEE", amount = plan.registrationAmount},
        PlanFareBreakup {component = "MAX_FEE_LIMIT", amount = plan.maxAmount},
        PlanFareBreakup {component = "DISCOUNTED_FEE", amount = discountAmount},
        PlanFareBreakup {component = "FINAL_FEE", amount = finalOrderAmount}
        ]

mkMandateDetailEntity :: Maybe (Id DM.Mandate) -> Flow (Maybe MandateDetailsEntity)
mkMandateDetailEntity mandateId = do
  case mandateId of
    Just id -> do
      mandate <- B.runInReplica $ QM.findById id >>= fromMaybeM (MandateNotFound id.getId)
      return $
        Just
          MandateDetails
            { status = mandate.status,
              startDate = mandate.startDate,
              endDate = mandate.endDate,
              mandateId = mandate.id.getId,
              payerVpa = mandate.payerVpa,
              frequency = "Aspresented",
              maxAmount = round mandate.maxAmount,
              autopaySetupDate = mandate.createdAt
            }
    Nothing -> return Nothing
