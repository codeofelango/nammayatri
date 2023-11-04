{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ProviderPlatformClient.DynamicOfferDriver
  ( callDriverOfferBPP,
    callDynamicOfferDriverAppExotelApi,
    callDynamicOfferDriverAppFleetApi,
  )
where

import "dynamic-offer-driver-app" API.Dashboard as BPP
import qualified Dashboard.Common.Booking as Booking
import qualified Dashboard.Common.Exotel as Exotel
import qualified Dashboard.ProviderPlatform.Driver as Driver
import qualified Dashboard.ProviderPlatform.Driver.Registration as Registration
import qualified Dashboard.ProviderPlatform.DriverReferral as DriverReferral
import qualified Dashboard.ProviderPlatform.Merchant as Merchant
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Dashboard.ProviderPlatform.Message as Message
import qualified Dashboard.ProviderPlatform.Revenue as Revenue
import qualified Dashboard.ProviderPlatform.Ride as Ride
import qualified Dashboard.ProviderPlatform.Volunteer as Volunteer
import qualified Data.ByteString.Lazy as LBS
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Driver as DDriver
import qualified "dynamic-offer-driver-app" Domain.Action.Dashboard.Fleet.Registration as Fleet
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
import Kernel.Utils.Common
import Servant
import Tools.Auth.Merchant (CheckedShortId)
import Tools.Client
import "lib-dashboard" Tools.Metrics

data DriverOfferAPIs = DriverOfferAPIs
  { drivers :: DriversAPIs,
    rides :: RidesAPIs,
    bookings :: BookingsAPIs,
    merchant :: MerchantAPIs,
    message :: MessageAPIs,
    volunteer :: VolunteerAPIs,
    overlay :: OverlayAPIs,
    driverReferral :: DriverReferralAPIs,
    driverRegistration :: DriverRegistrationAPIs,
    issue :: IssueAPIs,
    revenue :: RevenueAPIs,
    subscription :: SubscriptionAPIs
  }

data DriversAPIs = DriversAPIs
  { driverDocumentsInfo :: Euler.EulerClient Driver.DriverDocumentsInfoRes,
    driverAadhaarInfo :: Id Driver.Driver -> Euler.EulerClient Driver.DriverAadhaarInfoRes,
    driverAadhaarInfoByPhone :: Text -> Euler.EulerClient Driver.DriverAadhaarInfoByPhoneReq,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> Euler.EulerClient Driver.DriverListRes,
    getDriverDue :: Maybe Text -> Text -> Euler.EulerClient [Driver.DriverOutstandingBalanceResp],
    driverActivity :: Euler.EulerClient Driver.DriverActivityRes,
    enableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    disableDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    blockDriverWithReason :: Id Driver.Driver -> Driver.BlockDriverWithReasonReq -> Euler.EulerClient APISuccess,
    blockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    blockReasonList :: Euler.EulerClient [Driver.BlockReason],
    collectCash :: Id Driver.Driver -> Text -> Euler.EulerClient APISuccess,
    exemptCash :: Id Driver.Driver -> Text -> Euler.EulerClient APISuccess,
    unblockDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    driverLocation :: Maybe Int -> Maybe Int -> Driver.DriverIds -> Euler.EulerClient Driver.DriverLocationRes,
    driverInfo :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Bool -> Euler.EulerClient Driver.DriverInfoRes,
    deleteDriver :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkVehicle :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkDL :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    unlinkAadhaar :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    endRCAssociation :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    updatePhoneNumber :: Id Driver.Driver -> Driver.UpdatePhoneNumberReq -> Euler.EulerClient APISuccess,
    updateByPhoneNumber :: Text -> Driver.UpdateDriverDataReq -> Euler.EulerClient APISuccess,
    addVehicle :: Id Driver.Driver -> Driver.AddVehicleReq -> Euler.EulerClient APISuccess,
    addVehicleForFleet :: Text -> Maybe Text -> Text -> Driver.AddVehicleReq -> Euler.EulerClient APISuccess,
    getAllVehicleForFleet :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.ListVehicleRes,
    getAllDriverForFleet :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.FleetListDriverRes,
    fleetUnlinkVehicle :: Text -> Id Driver.Driver -> Text -> Euler.EulerClient APISuccess,
    fleetRemoveVehicle :: Text -> Text -> Euler.EulerClient APISuccess,
    fleetRemoveDriver :: Text -> Id Driver.Driver -> Euler.EulerClient APISuccess,
    fleetTotalEarning :: Text -> Euler.EulerClient Driver.FleetTotalEarningResponse,
    fleetVehicleEarning :: Text -> Text -> Maybe (Id Driver.Driver) -> Euler.EulerClient Driver.FleetEarningRes,
    fleetDriverEarning :: Text -> Id Driver.Driver -> Euler.EulerClient Driver.FleetEarningRes,
    updateDriverName :: Id Driver.Driver -> Driver.UpdateDriverNameReq -> Euler.EulerClient APISuccess,
    clearOnRideStuckDrivers :: Maybe Int -> Euler.EulerClient Driver.ClearOnRideStuckDriversRes,
    getDriverHomeLocation :: Id Driver.Driver -> Euler.EulerClient Driver.GetHomeLocationsRes,
    updateDriverHomeLocation :: Id Driver.Driver -> Driver.UpdateDriverHomeLocationReq -> Euler.EulerClient APISuccess,
    incrementDriverGoToCount :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    getDriverGoHomeInfo :: Id Driver.Driver -> Euler.EulerClient Driver.CachedGoHomeRequestInfoRes,
    setRCStatus :: Id Driver.Driver -> Driver.RCStatusReq -> Euler.EulerClient APISuccess,
    deleteRC :: Id Driver.Driver -> Driver.DeleteRCReq -> Euler.EulerClient APISuccess,
    getPaymentHistory :: Id Driver.Driver -> Maybe INV.InvoicePaymentMode -> Maybe Int -> Maybe Int -> Euler.EulerClient ADriver.HistoryEntityV2,
    getPaymentHistoryEntityDetails :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient ADriver.HistoryEntryDetailsEntityV2,
    updateSubscriptionDriverFeeAndInvoice :: Id Driver.Driver -> Driver.SubscriptionDriverFeesAndInvoicesToUpdate -> Euler.EulerClient Driver.SubscriptionDriverFeesAndInvoicesToUpdate,
    getFleetDriverVehicleAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    getFleetDriverAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    getFleetVehicleAssociation :: Text -> Maybe Int -> Maybe Int -> Euler.EulerClient Driver.DrivertoVehicleAssociationRes,
    setVehicleDriverRcStatusForFleet :: Id Driver.Driver -> Text -> Driver.RCStatusReq -> Euler.EulerClient APISuccess,
    sendMessageToDriverViaDashboard :: Id Driver.Driver -> Text -> DDriver.SendSmsReq -> Euler.EulerClient APISuccess
  }

data RidesAPIs = RidesAPIs
  { rideList :: Maybe Int -> Maybe Int -> Maybe Ride.BookingStatus -> Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Money -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient Ride.RideListRes,
    rideStart :: Id Ride.Ride -> Ride.StartRideReq -> Euler.EulerClient APISuccess,
    rideEnd :: Id Ride.Ride -> Ride.EndRideReq -> Euler.EulerClient APISuccess,
    multipleRideEnd :: Ride.MultipleRideEndReq -> Euler.EulerClient Ride.MultipleRideEndResp,
    currentActiveRide :: Text -> Euler.EulerClient (Id Ride.Ride),
    rideCancel :: Id Ride.Ride -> Ride.CancelRideReq -> Euler.EulerClient APISuccess,
    multipleRideCancel :: Ride.MultipleRideCancelReq -> Euler.EulerClient Ride.MultipleRideCancelResp,
    rideInfo :: Id Ride.Ride -> Euler.EulerClient Ride.RideInfoRes,
    rideSync :: Id Ride.Ride -> Euler.EulerClient Ride.RideSyncRes,
    multipleRideSync :: Ride.MultipleRideSyncReq -> Euler.EulerClient Ride.MultipleRideSyncRes,
    rideRoute :: Id Ride.Ride -> Euler.EulerClient Ride.RideRouteRes,
    bookingWithVehicleNumberAndPhone :: Ride.BookingWithVehicleAndPhoneReq -> Euler.EulerClient Ride.BookingWithVehicleAndPhoneRes,
    ticketRideList :: Maybe (ShortId Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Euler.EulerClient Ride.TicketRideListRes
  }

data BookingsAPIs = BookingsAPIs
  { stuckBookingsCancel :: Booking.StuckBookingsCancelReq -> Euler.EulerClient Booking.StuckBookingsCancelRes,
    multipleBookingSync :: Booking.MultipleBookingSyncReq -> Euler.EulerClient Booking.MultipleBookingSyncResp
  }

data MerchantAPIs = MerchantAPIs
  { merchantUpdate :: Merchant.MerchantUpdateReq -> Euler.EulerClient Merchant.MerchantUpdateRes,
    merchantCommonConfig :: Euler.EulerClient Merchant.MerchantCommonConfigRes,
    merchantCommonConfigUpdate :: Merchant.MerchantCommonConfigUpdateReq -> Euler.EulerClient APISuccess,
    driverPoolConfig :: Maybe Meters -> Euler.EulerClient Merchant.DriverPoolConfigRes,
    driverPoolConfigUpdate :: Meters -> Maybe Common.Variant -> Merchant.DriverPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    driverPoolConfigCreate :: Meters -> Maybe Common.Variant -> Merchant.DriverPoolConfigCreateReq -> Euler.EulerClient APISuccess,
    driverIntelligentPoolConfig :: Euler.EulerClient Merchant.DriverIntelligentPoolConfigRes,
    driverIntelligentPoolConfigUpdate :: Merchant.DriverIntelligentPoolConfigUpdateReq -> Euler.EulerClient APISuccess,
    onboardingDocumentConfig :: Maybe Merchant.DocumentType -> Euler.EulerClient Merchant.OnboardingDocumentConfigRes,
    onboardingDocumentConfigUpdate :: Merchant.DocumentType -> Merchant.OnboardingDocumentConfigUpdateReq -> Euler.EulerClient APISuccess,
    onboardingDocumentConfigCreate :: Merchant.DocumentType -> Merchant.OnboardingDocumentConfigCreateReq -> Euler.EulerClient APISuccess,
    serviceUsageConfig :: Euler.EulerClient Merchant.ServiceUsageConfigRes,
    mapsServiceConfigUpdate :: Merchant.MapsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    mapsServiceUsageConfigUpdate :: Merchant.MapsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceConfigUpdate :: Merchant.SmsServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    smsServiceUsageConfigUpdate :: Merchant.SmsServiceUsageConfigUpdateReq -> Euler.EulerClient APISuccess,
    verificationServiceConfigUpdate :: Merchant.VerificationServiceConfigUpdateReq -> Euler.EulerClient APISuccess,
    createFPDriverExtraFee :: Id Common.FarePolicy -> Meters -> Merchant.CreateFPDriverExtraFeeReq -> Euler.EulerClient APISuccess,
    updateFPDriverExtraFee :: Id Common.FarePolicy -> Meters -> Merchant.CreateFPDriverExtraFeeReq -> Euler.EulerClient APISuccess
  }

data DriverReferralAPIs = DriverReferralAPIs
  { updateReferralLinkPassword :: DriverReferral.ReferralLinkPasswordUpdateAPIReq -> Euler.EulerClient APISuccess,
    linkDriverReferralCode :: (LBS.ByteString, DriverReferral.ReferralLinkReq) -> Euler.EulerClient DriverReferral.LinkReport
  }

data DriverRegistrationAPIs = DriverRegistrationAPIs
  { documentsList :: Id Driver.Driver -> Euler.EulerClient Registration.DocumentsListResponse,
    getDocument :: Id Driver.Image -> Euler.EulerClient Registration.GetDocumentResponse,
    uploadDocument :: Id Driver.Driver -> Registration.UploadDocumentReq -> Euler.EulerClient Registration.UploadDocumentResp,
    registerDL :: Id Driver.Driver -> Registration.RegisterDLReq -> Euler.EulerClient APISuccess,
    registerRC :: Id Driver.Driver -> Registration.RegisterRCReq -> Euler.EulerClient APISuccess,
    generateAadhaarOtp :: Id Driver.Driver -> Registration.GenerateAadhaarOtpReq -> Euler.EulerClient Registration.GenerateAadhaarOtpRes,
    verifyAadhaarOtp :: Id Driver.Driver -> Registration.VerifyAadhaarOtpReq -> Euler.EulerClient Registration.VerifyAadhaarOtpRes,
    auth :: Bool -> Text -> Registration.AuthReq -> Euler.EulerClient Registration.AuthRes,
    verify :: Text -> Bool -> Text -> Registration.AuthVerifyReq -> Euler.EulerClient APISuccess
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

data VolunteerAPIs = VolunteerAPIs
  { bookingInfo :: Text -> Euler.EulerClient Volunteer.BookingInfoResponse,
    assignCreateAndStartOtpRide :: Volunteer.AssignCreateAndStartOtpRideAPIReq -> Euler.EulerClient APISuccess
  }

data OverlayAPIs = OverlayAPIs
  { createOverlay :: Overlay.CreateOverlayReq -> Euler.EulerClient APISuccess,
    deleteOverlay :: Overlay.DeleteOverlayReq -> Euler.EulerClient APISuccess,
    listOverlay :: Euler.EulerClient Overlay.ListOverlayResp,
    overlayInfo :: Overlay.OverlayInfoReq -> Euler.EulerClient Overlay.OverlayInfoResp,
    scheduleOverlay :: Overlay.ScheduleOverlay -> Euler.EulerClient APISuccess
  }

data RevenueAPIs = RevenueAPIs
  { getCollectionHistory :: Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient Revenue.CollectionList,
    getAllDriverFeeHistory :: Maybe UTCTime -> Maybe UTCTime -> Euler.EulerClient [Revenue.AllFees]
  }

data IssueAPIs = IssueAPIs
  { issueCategoryList :: Euler.EulerClient Issue.IssueCategoryListRes,
    issueList :: Maybe Int -> Maybe Int -> Maybe IssueStatus -> Maybe (Id IssueCategory) -> Maybe Text -> Euler.EulerClient Issue.IssueReportListResponse,
    issueInfo :: Id IssueReport -> Euler.EulerClient Issue.IssueInfoRes,
    issueUpdate :: Id IssueReport -> Issue.IssueUpdateByUserReq -> Euler.EulerClient APISuccess,
    issueAddComment :: Id IssueReport -> Issue.IssueAddCommentByUserReq -> Euler.EulerClient APISuccess,
    issueFetchMedia :: Text -> Euler.EulerClient Text,
    ticketStatusCallBack :: Issue.TicketStatusCallBackReq -> Euler.EulerClient APISuccess
  }

data SubscriptionAPIs = SubscriptionAPIs
  { planList :: Id Driver.Driver -> Euler.EulerClient Subscription.PlanListAPIRes,
    planSelect :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient APISuccess,
    planSuspend :: Id Driver.Driver -> Euler.EulerClient APISuccess,
    planSubscribe :: Id Driver.Driver -> Id DPlan.Plan -> Euler.EulerClient Subscription.PlanSubscribeRes,
    currentPlan :: Id Driver.Driver -> Euler.EulerClient Subscription.CurrentPlanRes,
    paymentStatus :: Id Driver.Driver -> Id INV.Invoice -> Euler.EulerClient APayment.PaymentStatusResp
  }

mkDriverOfferAPIs :: CheckedShortId DM.Merchant -> City.City -> Text -> DriverOfferAPIs
mkDriverOfferAPIs merchantId city token = do
  let drivers = DriversAPIs {..}
  let rides = RidesAPIs {..}
  let subscription = SubscriptionAPIs {..}
  let driverReferral = DriverReferralAPIs {..}
  let driverRegistration = DriverRegistrationAPIs {..}
  let bookings = BookingsAPIs {..}
  let merchant = MerchantAPIs {..}
  let message = MessageAPIs {..}
  let volunteer = VolunteerAPIs {..}
  let issue = IssueAPIs {..}
  let revenue = RevenueAPIs {..}
  let overlay = OverlayAPIs {..}
  DriverOfferAPIs {..}
  where
    driversClient
      :<|> ridesClient
      :<|> subscriptionClient
      :<|> bookingsClient
      :<|> merchantClient
      :<|> messageClient
      :<|> driverReferralClient
      :<|> driverRegistrationClient
      :<|> volunteerClient
      :<|> issueClient
      :<|> revenueClient
      :<|> overlayClient = clientWithMerchantAndCity (Proxy :: Proxy BPP.API') merchantId city token

    planList
      :<|> planSelect
      :<|> planSuspend
      :<|> planSubscribe
      :<|> currentPlan
      :<|> paymentStatus = subscriptionClient

    driverDocumentsInfo
      :<|> driverAadhaarInfo
      :<|> driverAadhaarInfoByPhone
      :<|> listDrivers
      :<|> getDriverDue
      :<|> driverActivity
      :<|> enableDriver
      :<|> disableDriver
      :<|> blockDriverWithReason
      :<|> blockDriver
      :<|> blockReasonList
      :<|> collectCash
      :<|> exemptCash
      :<|> unblockDriver
      :<|> driverLocation
      :<|> driverInfo
      :<|> deleteDriver
      :<|> unlinkVehicle
      :<|> unlinkDL
      :<|> unlinkAadhaar
      :<|> endRCAssociation
      :<|> updatePhoneNumber
      :<|> updateByPhoneNumber
      :<|> addVehicle
      :<|> addVehicleForFleet
      :<|> getAllVehicleForFleet
      :<|> getAllDriverForFleet
      :<|> fleetUnlinkVehicle
      :<|> fleetRemoveVehicle
      :<|> fleetRemoveDriver
      :<|> fleetTotalEarning
      :<|> fleetVehicleEarning
      :<|> fleetDriverEarning
      :<|> updateDriverName
      :<|> setRCStatus
      :<|> deleteRC
      :<|> clearOnRideStuckDrivers
      :<|> getDriverHomeLocation
      :<|> updateDriverHomeLocation
      :<|> incrementDriverGoToCount
      :<|> getDriverGoHomeInfo
      :<|> getPaymentHistory
      :<|> getPaymentHistoryEntityDetails
      :<|> updateSubscriptionDriverFeeAndInvoice
      :<|> getFleetDriverVehicleAssociation
      :<|> getFleetDriverAssociation
      :<|> getFleetVehicleAssociation
      :<|> setVehicleDriverRcStatusForFleet
      :<|> sendMessageToDriverViaDashboard = driversClient

    rideList
      :<|> rideStart
      :<|> rideEnd
      :<|> multipleRideEnd
      :<|> currentActiveRide
      :<|> rideCancel
      :<|> multipleRideCancel
      :<|> rideInfo
      :<|> rideSync
      :<|> multipleRideSync
      :<|> rideRoute
      :<|> bookingWithVehicleNumberAndPhone
      :<|> ticketRideList = ridesClient

    stuckBookingsCancel
      :<|> multipleBookingSync = bookingsClient

    merchantUpdate
      :<|> merchantCommonConfig
      :<|> merchantCommonConfigUpdate
      :<|> driverPoolConfig
      :<|> driverPoolConfigUpdate
      :<|> driverPoolConfigCreate
      :<|> driverIntelligentPoolConfig
      :<|> driverIntelligentPoolConfigUpdate
      :<|> onboardingDocumentConfig
      :<|> onboardingDocumentConfigUpdate
      :<|> onboardingDocumentConfigCreate
      :<|> serviceUsageConfig
      :<|> mapsServiceConfigUpdate
      :<|> mapsServiceUsageConfigUpdate
      :<|> smsServiceConfigUpdate
      :<|> smsServiceUsageConfigUpdate
      :<|> verificationServiceConfigUpdate
      :<|> createFPDriverExtraFee
      :<|> updateFPDriverExtraFee = merchantClient

    updateReferralLinkPassword
      :<|> linkDriverReferralCode = driverReferralClient

    documentsList
      :<|> getDocument
      :<|> uploadDocument
      :<|> registerDL
      :<|> registerRC
      :<|> generateAadhaarOtp
      :<|> verifyAadhaarOtp
      :<|> auth
      :<|> verify = driverRegistrationClient

    uploadFile
      :<|> addLinkAsMedia
      :<|> addMessage
      :<|> sendMessage
      :<|> messageList
      :<|> messageInfo
      :<|> messageDeliveryInfo
      :<|> messageReceiverList = messageClient

    bookingInfo
      :<|> assignCreateAndStartOtpRide = volunteerClient

    createOverlay
      :<|> deleteOverlay
      :<|> listOverlay
      :<|> overlayInfo
      :<|> scheduleOverlay = overlayClient

    issueCategoryList
      :<|> issueList
      :<|> issueInfo
      :<|> issueUpdate
      :<|> issueAddComment
      :<|> issueFetchMedia
      :<|> ticketStatusCallBack = issueClient

    getCollectionHistory
      :<|> getAllDriverFeeHistory = revenueClient

callDriverOfferBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI DriverOfferAPIs m r b c
  ) =>
  CheckedShortId DM.Merchant ->
  City.City ->
  (DriverOfferAPIs -> b) ->
  c
callDriverOfferBPP merchantId city = callServerAPI @_ @m @r DRIVER_OFFER_BPP (mkDriverOfferAPIs merchantId city) "callDriverOfferBPP"

newtype ExotelAPIs = ExotelAPIs
  { exotelHeartbeat :: Exotel.ExotelHeartbeatReq -> Euler.EulerClient APISuccess
  }

mkDynamicOfferDriverAppExotelAPIs :: Text -> ExotelAPIs
mkDynamicOfferDriverAppExotelAPIs token = do
  ExotelAPIs {..}
  where
    exotelHeartbeat = Euler.client (Proxy :: Proxy BPP.ExotelAPI) token

callDynamicOfferDriverAppExotelApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI ExotelAPIs m r b c
  ) =>
  (ExotelAPIs -> b) ->
  c
callDynamicOfferDriverAppExotelApi = callServerAPI @_ @m @r DRIVER_OFFER_BPP mkDynamicOfferDriverAppExotelAPIs "callDynamicOfferDriverAppExotelApi"

data FleetAPIs = FleetAPIs
  { fleetOwnerLogin :: Fleet.FleetOwnerLoginReq -> Euler.EulerClient APISuccess,
    fleetOwnerVerify :: Fleet.FleetOwnerLoginReq -> Euler.EulerClient APISuccess
  }

mkDynamicOfferDriverAppFleetAPIs :: Text -> FleetAPIs
mkDynamicOfferDriverAppFleetAPIs token = do
  FleetAPIs {..}
  where
    fleetOwnerLogin
      :<|> fleetOwnerVerify = Euler.client (Proxy :: Proxy BPP.FleetAPI) token

callDynamicOfferDriverAppFleetApi ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI FleetAPIs m r b c
  ) =>
  (FleetAPIs -> b) ->
  c
callDynamicOfferDriverAppFleetApi = callServerAPI @_ @m @r DRIVER_OFFER_BPP mkDynamicOfferDriverAppFleetAPIs "callDynamicOfferDriverAppFleetApi"
