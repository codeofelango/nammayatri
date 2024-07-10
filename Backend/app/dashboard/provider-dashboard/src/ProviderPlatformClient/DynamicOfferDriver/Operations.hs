{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.DynamicOfferDriver.Operations
  ( callDriverOfferBPPOperations,
  )
where

import "dynamic-offer-driver-app" API.Dashboard.Management as BPP
import qualified API.Dashboard.Management.Subscription as MSubscription
import qualified API.Types.ProviderPlatform.Management.Merchant as MerchantDSL
import qualified API.Types.ProviderPlatform.Management.Revenue as RevenueDSL
import qualified API.Types.ProviderPlatform.Management.Ride as RideDSL
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.ProviderPlatform.Driver as Driver
import qualified Dashboard.ProviderPlatform.Driver.Coin as Coins
import qualified Dashboard.ProviderPlatform.Driver.Registration as Registration
import qualified Dashboard.ProviderPlatform.DriverReferral as DriverReferral
-- import qualified Dashboard.ProviderPlatform.Merchant as Merchant
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Dashboard.ProviderPlatform.Message as Message
import qualified Dashboard.ProviderPlatform.Ride as Ride
import qualified Data.ByteString.Lazy as LBS
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Driver as DDriver
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Overlay as Overlay
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Driver as ADriver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Payment as APayment
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Plan as Subscription
import qualified "dynamic-offer-driver-app" Domain.Types.Invoice as INV
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "dynamic-offer-driver-app" Domain.Types.Plan as DPlan
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import IssueManagement.Common
import qualified IssueManagement.Common.Dashboard.Issue as Issue
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueReport
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
-- import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client

-- TODO remove DSL suffix after move all apis to DSL
data DriverOperationAPIs = DriverOperationAPIs
  { subscription :: SubscriptionAPIs,
    rides :: RidesAPIs,
    overlay :: OverlayAPIs,
    message :: MessageAPIs,
    merchant :: MerchantAPIs,
    issue :: IssueAPIs,
    drivers :: DriversAPIs,
    bookings :: BookingsAPIs,
    merchantDSL :: MerchantDSL.MerchantAPIs,
    revenueDSL :: RevenueDSL.RevenueAPIs,
    rideDSL :: RideDSL.RideAPIs
  }

data GoHomeAPIs = GoHomeAPIs
  { getDriverHomeLocation :: Id Driver.Driver -> Euler.EulerClient Driver.GetHomeLocationsRes,
    updateDriverHomeLocation :: Id Driver.Driver -> Driver.UpdateDriverHomeLocationReq -> Euler.EulerClient APISuccess,
    incrementDriverGoToCount :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    getDriverGoHomeInfo :: Id Driver.Driver -> Euler.EulerClient Driver.CachedGoHomeRequestInfoRes
  }

data DriverReferralAPIs = DriverReferralAPIs
  { updateReferralLinkPassword :: DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Euler.EulerClient APISuccess,
    linkDriverReferralCode :: (LBS.ByteString, DriverReferral.ReferralLinkReq) -> Euler.EulerClient DriverReferral.LinkReport
  }

data DriverCommonAPIs = DriverCommonAPIs
  { driverDocumentsInfo :: Euler.EulerClient Driver.DriverDocumentsInfoRes,
    driverAadhaarInfo :: Id Driver.Driver -> Euler.EulerClient Driver.DriverAadhaarInfoRes,
    driverAadhaarInfoByPhone :: Text -> Euler.EulerClient Driver.DriverAadhaarInfoByPhoneReq,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverListRes,
    driverActivity :: Euler.EulerClient Driver.DriverActivityRes,
    disableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    updateACUsageRestriction :: Id Driver.Driver -> Driver.UpdateACUsageRestrictionReq -> Euler.EulerClient APISuccess,
    blockDriverWithReason :: Id Driver.Driver -> Text -> Driver.BlockDriverWithReasonReq -> Euler.EulerClient APISuccess,
    blockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    blockReasonList :: Euler.EulerClient [Driver.BlockReason],
    unblockDriver :: Id Driver.Driver -> Text -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Driver.DriverIds -> Euler.EulerClient Driver.DriverLocationRes,
    deleteDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkDL :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkAadhaar :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Driver.Driver -> Driver.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    updateByPhoneNumber :: Text -> Driver.UpdateDriverDataReq -> Euler.EulerClient APISuccess,
    updateDriverName :: Id Driver.Driver -> Driver.UpdateDriverNameReq -> Euler.EulerClient APISuccess,
    deleteRC :: Id Driver.Driver -> Driver.DeleteRCReq -> Euler.EulerClient APISuccess,
    clearOnRideStuckDrivers :: Maybe Int -> Euler.EulerClient Driver.ClearOnRideStuckDriversRes,
    sendDummyRideRequestToDriverViaDashboard :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    changeOperatingCity :: Id Driver.Driver -> Driver.ChangeOperatingCityReq -> Euler.EulerClient APISuccess,
    getOperatingCity :: Maybe Text -> Maybe Text -> Maybe (Id Ride.Ride) -> Euler.EulerClient Driver.GetOperatingCityResp,
    setServiceChargeEligibleFlagInDriverPlan :: Id Common.Driver -> Driver.PauseOrResumeServiceChargesReq -> Euler.EulerClient APISuccess,
    updateRCInvalidStatus :: Id Common.Driver -> Driver.UpdateRCInvalidStatusReq -> Euler.EulerClient APISuccess,
    updateVehicleVariant :: Id Common.Driver -> Driver.UpdateVehicleVariantReq -> Euler.EulerClient APISuccess,
    bulkReviewRCVariant :: [Driver.ReviewRCVariantReq] -> Euler.EulerClient [Driver.ReviewRCVariantRes],
    updateDriverTag :: Id Driver.Driver -> Driver.UpdateDriverTagReq -> Euler.EulerClient APISuccess
  }

data DriverRegistrationAPIs = DriverRegistrationAPIs
  { documentsList :: Id Driver.Driver -> Euler.EulerClient Registration.DocumentsListResponse,
    getDocument :: Id Driver.Image -> Euler.EulerClient Registration.GetDocumentResponse,
    uploadDocument :: Id Driver.Driver -> Registration.UploadDocumentReq -> Euler.EulerClient Registration.UploadDocumentResp,
    registerDL :: Id Driver.Driver -> Registration.RegisterDLReq -> Euler.EulerClient APISuccess,
    registerRC :: Id Driver.Driver -> Registration.RegisterRCReq -> Euler.EulerClient APISuccess,
    generateAadhaarOtp :: Id Driver.Driver -> Registration.GenerateAadhaarOtpReq -> Euler.EulerClient Registration.GenerateAadhaarOtpRes,
    verifyAadhaarOtp :: Id Driver.Driver -> Registration.VerifyAadhaarOtpReq -> Euler.EulerClient Registration.VerifyAadhaarOtpRes,
    underReviewDriversList :: Maybe Int -> Maybe Int -> Euler.EulerClient Registration.UnderReviewDriversListResponse,
    driverDocumentInfo :: Id Driver.Driver -> Euler.EulerClient [Registration.DriverDocument],
    updateDocument :: Registration.UpdateDocumentRequest -> Euler.EulerClient APISuccess
  }

data DriversAPIs = DriversAPIs
  { goHome :: GoHomeAPIs,
    driverReferral :: DriverReferralAPIs,
    driverRegistration :: DriverRegistrationAPIs,
    driverCommon :: DriverCommonAPIs,
    driverCoins :: CoinAPIs
  }

data RidesAPIs = RidesAPIs
  { multipleRideEnd :: Ride.MultipleRideEndReq -> Euler.EulerClient Ride.MultipleRideEndResp,
    multipleRideCancel :: Ride.MultipleRideCancelReq -> Euler.EulerClient Ride.MultipleRideCancelResp,
    rideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.RideInfoRes,
    rideSync :: Id Ride.Ride -> Euler.EulerClient Ride.RideSyncRes,
    multipleRideSync :: Ride.MultipleRideSyncReq -> Euler.EulerClient Ride.MultipleRideSyncRes,
    rideRoute :: Id Ride.Ride -> Euler.EulerClient Ride.RideRouteRes,
    ticketRideList :: Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Ride.TicketRideListRes
  }

data BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes,
    multipleBookingSync :: Booking.MultipleBookingSyncReq -> Euler.EulerClient Booking.MultipleBookingSyncResp
  }

data MerchantAPIs = MerchantAPIs
  { --merchantCommonConfig :: Euler.EulerClient Merchant.MerchantCommonConfigRes,
    -- merchantCommonConfigUpdate :: Merchant.MerchantCommonConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- driverPoolConfig :: Maybe Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Euler.EulerClient Merchant.DriverPoolConfigRes,
    -- driverPoolConfigUpdate :: Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> SL.Area -> Maybe Common.Variant -> Maybe Text -> Merchant.DriverPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- driverPoolConfigCreate :: Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> SL.Area -> Maybe Common.Variant -> Maybe Text -> Merchant.DriverPoolConfigCreateReq -> Euler.EulerClient APISuccess,
    -- driverIntelligentPoolConfig :: Euler.EulerClient Merchant.DriverIntelligentPoolConfigRes,
    -- driverIntelligentPoolConfigUpdate :: Merchant.DriverIntelligentPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- documentVerificationConfig :: Maybe Merchant.DocumentType -> Maybe Common.Category -> Euler.EulerClient Merchant.DocumentVerificationConfigRes,
    -- documentVerificationConfigUpdate :: Merchant.DocumentType -> Common.Category -> Merchant.DocumentVerificationConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- documentVerificationConfigCreate :: Merchant.DocumentType -> Common.Category -> Merchant.DocumentVerificationConfigCreateReq -> Euler.EulerClient APISuccess,
    -- serviceUsageConfig :: Euler.EulerClient Merchant.ServiceUsageConfigRes,
    -- mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- verificationServiceConfigUpdate :: Merchant.VerificationServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    -- createFPDriverExtraFee :: Id Common.FarePolicy -> Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Merchant.CreateFPDriverExtraFeeReq -> Euler.EulerClient APISuccess,
    -- updateFPDriverExtraFee :: Id Common.FarePolicy -> Meters -> Maybe HighPrecDistance -> Maybe DistanceUnit -> Merchant.CreateFPDriverExtraFeeReq -> Euler.EulerClient APISuccess,
    -- updateFPPerExtraKmRate :: Id Common.FarePolicy -> Meters -> Merchant.UpdateFPPerExtraKmRateReq -> Euler.EulerClient APISuccess,
    -- updateFarePolicy :: Id Common.FarePolicy -> Merchant.UpdateFarePolicyReq -> Euler.EulerClient APISuccess,
    -- upsertFarePolicy :: (LBS.ByteString, Merchant.UpsertFarePolicyReq) -> Euler.EulerClient Merchant.UpsertFarePolicyResp,
    -- createMerchantOperatingCity :: Merchant.CreateMerchantOperatingCityReqT -> Euler.EulerClient Merchant.CreateMerchantOperatingCityRes,
    -- schedulerTrigger :: Merchant.SchedulerTriggerReq -> Euler.EulerClient APISuccess,
    -- updateOnboardingVehicleVariantMapping :: (LBS.ByteString, Common.UpdateOnboardingVehicleVariantMappingReq) -> Euler.EulerClient APISuccess,
    upsertSpecialLocation :: Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> Euler.EulerClient APISuccess,
    deleteSpecialLocation :: Id SL.SpecialLocation -> Euler.EulerClient APISuccess,
    upsertSpecialLocationGate :: Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Euler.EulerClient APISuccess,
    deleteSpecialLocationGate :: Id SL.SpecialLocation -> Text -> Euler.EulerClient APISuccess
  }

data MessageAPIs = MessageAPIs
  { uploadFile :: (LBS.ByteString, Message.UploadFileRequest) -> Euler.EulerClient Message.UploadFileResponse,
    addLinkAsMedia :: Message.AddLinkAsMedia -> Euler.EulerClient Message.UploadFileResponse,
    addMessage :: Message.AddMessageRequest -> Euler.EulerClient Message.AddMessageResponse,
    sendMessage :: (LBS.ByteString, Message.SendMessageRequest) -> Euler.EulerClient APISuccess,
    messageList :: Maybe Int -> Maybe Int -> Euler.EulerClient Message.MessageListResponse,
    messageInfo :: Id Message.Message -> Euler.EulerClient Message.MessageInfoResponse,
    messageDeliveryInfo :: Id Message.Message -> Euler.EulerClient Message.MessageDeliveryInfoResponse,
    messageReceiverList :: Id Message.Message -> Maybe Text -> Maybe Message.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> Euler.EulerClient Message.MessageReceiverListResponse
  }

data OverlayAPIs = OverlayAPIs
  { createOverlay :: Overlay.CreateOverlayReq -> Euler.EulerClient APISuccess,
    deleteOverlay :: Overlay.DeleteOverlayReq -> Euler.EulerClient APISuccess,
    listOverlay :: Euler.EulerClient Overlay.ListOverlayResp,
    overlayInfo :: Text -> Maybe Text -> Euler.EulerClient Overlay.OverlayInfoResp,
    scheduleOverlay :: Overlay.ScheduleOverlay -> Euler.EulerClient APISuccess
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueInfoV2 :: Maybe (Id IssueReport) -> Maybe (ShortId IssueReport) -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text,
    ticketStatusCallBack :: Issue.TicketStatusCallBackReq -> Euler.EulerClient APISuccess
  }

data SubscriptionAPIs = SubscriptionAPIs
  { planListV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient Subscription.PlanListAPIRes,
    planSelectV2 :: Id Driver.Driver -> Id DPlan.Plan -> DPlan.ServiceNames -> Euler.EulerClient APISuccess,
    planSuspendV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient APISuccess,
    planSubscribeV2 :: Id Driver.Driver -> Id DPlan.Plan -> DPlan.ServiceNames -> MSubscription.PlanSubscribeReq -> Euler.EulerClient Subscription.PlanSubscribeRes,
    currentPlanV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Euler.EulerClient Subscription.CurrentPlanRes,
    paymentStatus :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient APayment.PaymentStatusResp,
    getPaymentHistoryV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Euler.EulerClient ADriver.HistoryEntityV2,
    getPaymentHistoryEntityDetailsV2 :: Id Driver.Driver -> DPlan.ServiceNames -> Id INV.Invoice -> Euler.EulerClient ADriver.HistoryEntryDetailsEntityV2,
    updateSubscriptionDriverFeeAndInvoice :: Id Driver.Driver -> Driver.ServiceNames -> Driver.SubscriptionDriverFeesAndInvoicesToUpdate -> Euler.EulerClient Driver.SubscriptionDriverFeesAndInvoicesToUpdate,
    sendMessageToDriverViaDashboard :: Id Driver.Driver -> Text -> DDriver.SendSmsReq -> Euler.EulerClient APISuccess,
    planList :: Id Driver.Driver -> Euler.EulerClient Subscription.PlanListAPIRes,
    planSelect :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient APISuccess,
    planSuspend :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    planSubscribe :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient Subscription.PlanSubscribeRes,
    currentPlan :: Id Driver.Driver -> Euler.EulerClient Subscription.CurrentPlanRes,
    getPaymentHistory :: Id Driver.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Euler.EulerClient ADriver.HistoryEntityV2,
    getPaymentHistoryEntityDetails :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient ADriver.HistoryEntryDetailsEntityV2
  }

data CoinAPIs = CoinAPIs
  { driverCoinBulkUpload :: Coins.BulkUploadCoinsReq -> Euler.EulerClient APISuccess,
    driverCoinBulkUploadV2 :: Coins.BulkUploadCoinsReqV2 -> Euler.EulerClient APISuccess,
    driverCoinsHistory :: Id Driver.Driver -> Maybe Integer -> Maybe Integer -> Euler.EulerClient Coins.CoinHistoryRes
  }

mkDriverOperationAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> DriverOperationAPIs
mkDriverOperationAPIs merchantId city token = do
  let driverReferral = DriverReferralAPIs {..}
  let driverRegistration = DriverRegistrationAPIs {..}
  let driverCommon = DriverCommonAPIs {..}
  let goHome = GoHomeAPIs {..}
  let driverCoins = CoinAPIs {..}
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  let subscription = SubscriptionAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let message = MessageAPIs {..}
  let issue = IssueAPIs {..}
  let overlay = OverlayAPIs {..}

  let merchantDSL = MerchantDSL.mkMerchantAPIs merchantClientDSL
  let revenueDSL = RevenueDSL.mkRevenueAPIs revenueClientDSL
  let rideDSL = RideDSL.mkRideAPIs rideClientDSL
  DriverOperationAPIs {..}
  where
    subscriptionClient
      :<|> ridesClient
      :<|> overlayClient
      :<|> messageClient
      :<|> merchantClient
      :<|> issueClient
      :<|> driversClient
      :<|> bookingsClient
      :<|> merchantClientDSL
      :<|> revenueClientDSL
      :<|> rideClientDSL =
        clientWithMerchantAndCity (Proxy :: Proxy BPP.API) merchantId city token

    planListV2
      :<|> planSelectV2
      :<|> planSuspendV2
      :<|> planSubscribeV2
      :<|> currentPlanV2
      :<|> planList
      :<|> planSelect
      :<|> planSuspend
      :<|> planSubscribe
      :<|> currentPlan
      :<|> paymentStatus
      :<|> getPaymentHistoryV2
      :<|> getPaymentHistoryEntityDetailsV2
      :<|> getPaymentHistory
      :<|> getPaymentHistoryEntityDetails
      :<|> updateSubscriptionDriverFeeAndInvoice
      :<|> sendMessageToDriverViaDashboard = subscriptionClient

    goHomeClient
      :<|> referralClient
      :<|> driverRegistrationClient
      :<|> driverCommonClient
      :<|> driverCoinsClient = driversClient

    driverDocumentsInfo
      :<|> driverAadhaarInfo
      :<|> driverAadhaarInfoByPhone
      :<|> listDrivers
      :<|> driverActivity
      :<|> disableDriver
      :<|> updateACUsageRestriction
      :<|> blockDriverWithReason
      :<|> blockDriver
      :<|> blockReasonList
      :<|> unblockDriver
      :<|> driverLocation
      :<|> deleteDriver
      :<|> unlinkDL
      :<|> unlinkAadhaar
      :<|> updatePhoneNumber
      :<|> updateByPhoneNumber
      :<|> updateDriverName
      :<|> deleteRC
      :<|> clearOnRideStuckDrivers
      :<|> sendDummyRideRequestToDriverViaDashboard
      :<|> changeOperatingCity
      :<|> getOperatingCity
      :<|> setServiceChargeEligibleFlagInDriverPlan
      :<|> updateRCInvalidStatus
      :<|> updateVehicleVariant
      :<|> bulkReviewRCVariant
      :<|> updateDriverTag = driverCommonClient

    updateReferralLinkPassword
      :<|> linkDriverReferralCode = referralClient

    getDriverHomeLocation
      :<|> updateDriverHomeLocation
      :<|> incrementDriverGoToCount
      :<|> getDriverGoHomeInfo = goHomeClient

    documentsList
      :<|> getDocument
      :<|> uploadDocument
      :<|> registerDL
      :<|> registerRC
      :<|> generateAadhaarOtp
      :<|> verifyAadhaarOtp
      :<|> underReviewDriversList
      :<|> driverDocumentInfo
      :<|> updateDocument = driverRegistrationClient

    multipleRideEnd
      :<|> multipleRideCancel
      :<|> rideInfo
      :<|> rideSync
      :<|> multipleRideSync
      :<|> rideRoute
      :<|> ticketRideList = ridesClient

    stuckBookingsCancel
      :<|> multipleBookingSync = bookingsClient

    -- merchantCommonConfig
    --   :<|> merchantCommonConfigUpdate
    --   :<|> driverPoolConfig
    --   :<|> driverPoolConfigUpdate
    --   :<|> driverPoolConfigCreate
    --   :<|> driverIntelligentPoolConfig
    --   :<|> driverIntelligentPoolConfigUpdate
    --   :<|> documentVerificationConfig
    --   :<|> documentVerificationConfigUpdate
    --   :<|> documentVerificationConfigCreate
    -- serviceUsageConfig
    --   :<|> mapsServiceUsageConfigUpdate
    --   :<|> smsServiceConfigUpdate
    --   :<|> smsServiceUsageConfigUpdate
    --   :<|> verificationServiceConfigUpdate
    -- :<|> createFPDriverExtraFee
    -- :<|> updateFPDriverExtraFee
    -- :<|> updateFPPerExtraKmRate
    -- :<|> updateFarePolicy
    -- :<|> upsertFarePolicy
    -- createMerchantOperatingCity
    -- :<|> schedulerTrigger
    -- :<|> updateOnboardingVehicleVariantMapping
    upsertSpecialLocation
      :<|> deleteSpecialLocation
      :<|> upsertSpecialLocationGate
      :<|> deleteSpecialLocationGate = merchantClient

    uploadFile
      :<|> addLinkAsMedia
      :<|> addMessage
      :<|> sendMessage
      :<|> messageList
      :<|> messageInfo
      :<|> messageDeliveryInfo
      :<|> messageReceiverList = messageClient

    createOverlay
      :<|> deleteOverlay
      :<|> listOverlay
      :<|> overlayInfo
      :<|> scheduleOverlay = overlayClient

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueInfoV2
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia
      :<|> ticketStatusCallBack = issueClient

    driverCoinBulkUpload
      :<|> driverCoinBulkUploadV2
      :<|> driverCoinsHistory = driverCoinsClient

callDriverOfferBPPOperations ::
  forall m r b c.
  CallServerAPI' DriverOperationAPIs m r b c =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (DriverOperationAPIs -> b) ->
  c
callDriverOfferBPPOperations merchantId city = callServerAPI @_ @m @r DRIVER_OFFER_BPP_MANAGEMENT (mkDriverOperationAPIs merchantId city) "callDriverOfferBPPOperations"
