{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Types where

import MerchantConfig.Types
import PrestoDOM.List

import Common.Types.App as CTA
import Components.ChatView.Controller (ChatComponentConfig, Config)
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.SettingSideBar.Controller (SettingSideBarState)
import Components.SettingSideBar.Controller as SideBar
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Domain.Payments as PP
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (Object)
import Halogen.VDom.DOM.Prop (PropValue)
import JBridge (Location)
import Language.Types (STR(..))
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import PrestoDOM (LetterSpacing, BottomSheetState(..), Visibility(..))
import RemoteConfig as RC
import Services.API (AddressComponents, BookingLocationAPIEntity, EstimateAPIEntity(..), QuoteAPIEntity, TicketPlaceResp, RideBookingRes, Route, BookingStatus(..), LatLong(..), PlaceType(..), ServiceExpiry(..), Chat, SosFlow(..), MetroTicketBookingStatus(..),GetMetroStationResp(..),TicketCategoriesResp(..), MetroQuote, RideShareOptions(..), SavedLocationsListRes,  Route(..), DeadKmFare(..))
import Components.SettingSideBar.Controller as SideBar
import Components.MessagingView.Controller (ChatComponent)
import Screens(ScreenName)
import PrestoDOM.List
import JBridge (Location)
import Data.HashMap as DHM
import Data.Map as DM
import MerchantConfig.Types as MRC

type Contacts = {
  name :: String,
  number :: String
}

type NewContacts = {
  name :: String,
  number :: String,
  isSelected :: Boolean,
  enableForFollowing :: Boolean,
  enableForShareRide:: Boolean,
  onRide :: Boolean,
  priority :: Int
}

type NewContactsProp = {
  name :: PropValue,
  number :: PropValue,
  contactBackgroundColor :: PropValue,
  visibilitySelectedImage :: PropValue,
  visibilityUnSelectedImage :: PropValue
}

type EditTextInLabelState =
 {
    topLabel :: String
  , showTopLabel :: Boolean
  , inLabel :: String
  , showInLabel :: Boolean
  , valueId :: String
  , hint :: String
  , pattern :: Maybe String
  , id :: String
  }

type LanguageItemState =
 {
    key :: String
  , language :: String
  , selected :: Boolean
  }

type NotificationItemState = {
    color :: String
  , color1 :: String
  , icon :: String
  , title :: String
  , description :: String
}

type PrimarySelectItemState =
 {
    label :: String
  , placeholder :: String
  , selectedItem :: String
  , screenName :: String
  }

type EditTextState =
 {
    title :: String
  , hint :: String
  }

type EditTextInImageState =
 {
    topLabel :: String
  , showTopLabel :: Boolean
  , inImage :: String
  , showInImage :: Boolean
  , hint :: String
  }

type DateEditTextState =
 {
    label :: String
  , hint :: String
  , id :: String
  , value :: String
  }

type SplashScreenState =  {
   data :: SplashScreenData
 }

type SplashScreenData =  {
   message :: String
 }

-- ############################################################# WelcomeScreen ################################################################################




type WelcomeScreenState = {
  data :: WelcomeScreenData
}

type WelcomeScreenData = {
  carouselModal :: CTA.CarouselModal,
  logField :: Object Foreign
}

type StepsHeaderModelState = {
  activeIndex :: Int,
  textArray :: Array String,
  backArrowVisibility :: Boolean
, config :: AppConfig
}

-- ############################################################# ChooseLanguageScreen ################################################################################

type ChooseLanguageScreenState = {
  data :: ChooseLanguageScreenData,
  props :: ChooseLanguageScreenProps
}

type ChooseLanguageScreenData =  {
  isSelected :: Boolean,
  config :: AppConfig
 }

type ChooseLanguageScreenProps =  {
  selectedLanguage :: String,
  btnActive :: Boolean,
  exitAnimation :: Boolean
 }


-- ################################################ EnterMobileNumberScreenState #############################################
type EnterMobileNumberScreenState =
  {
    props :: EnterMobileNumberScreenStateProps,
    data :: EnterMobileNumberScreenStateData
  }

type EnterMobileNumberScreenStateProps = {
  enterOTP :: Boolean,
  btnActiveMobileNumber :: Boolean,
  btnActiveOTP :: Boolean,
  isValidMobileNumber :: Boolean,
  wrongOTP :: Boolean,
  resendEnable :: Boolean,
  isReadingOTP :: Boolean,
  capturedOtp :: String,
  letterSpacing :: LetterSpacing,
  mNumberEdtFocused :: Boolean,
  otpEdtFocused :: Boolean,
  editTextVal :: String,
  attemptLeft :: String,
  countryCodeOptionExpanded :: Boolean
}

type EnterMobileNumberScreenStateData = {
    mobileNumber :: String
  , countryObj :: CTA.CountryCodeObj
  , tokenId :: String
  , attempts :: Int
  , otp :: String
  , timer :: Int
  , timerID :: String
  , config :: AppConfig
  , logField :: Object Foreign
  , otpChannel :: CTA.OTPChannel
}
-- ################################################ AccountSetUpScreenState ##################################################

data Gender = MALE | FEMALE | OTHER | PREFER_NOT_TO_SAY

derive instance genericGender :: Generic Gender _
instance eqGender :: Eq Gender where eq = genericEq

type AccountSetUpScreenState =
  { props :: AccountSetUpScreenStateProps ,
    data :: AccountSetUpScreenStateData
  }


type AccountSetUpScreenStateProps =
  {   btnActive :: Boolean
    , backPressed :: Boolean
    , genderSelected :: Maybe String
    , genderOptionExpanded :: Boolean
    , expandEnabled :: Boolean
    , showOptions :: Boolean
    , activeField :: Maybe ActiveFieldAccountSetup
    , isNameValid :: Boolean
    , isSpecialAssistList :: Boolean
  }



data ActiveFieldAccountSetup = DropDown | NameSection

derive instance genericActiveFieldAccountSetup :: Generic ActiveFieldAccountSetup _
instance eqActiveFieldAccountSetup :: Eq ActiveFieldAccountSetup where eq = genericEq

type AccountSetUpScreenStateData =
  {   name :: String
    , email :: String
    , gender :: Maybe Gender
    , nameErrorMessage :: Maybe ErrorType
    , config :: AppConfig
    , disabilityOptions :: DisabilityData
  }



 -- ######################################  TripDetailsScreenState   ######################################
type TripDetailsScreenState =
  {
    data :: TripDetailsScreenData,
    props :: TripDetailsScreenProps
  }

data PaymentMode = CASH | ONLINE

derive instance genericPaymentMode :: Generic PaymentMode _
instance showPaymentMode :: Show PaymentMode where show = genericShow
instance eqPaymentMode :: Eq PaymentMode where eq = genericEq
instance encodePaymentMode :: Encode PaymentMode where encode = defaultEnumEncode
instance decodePaymentMode :: Decode PaymentMode where decode = defaultEnumDecode

type TripDetailsScreenData =
  {
    message :: String,
    driverName :: String,
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: String,
    paymentMode :: PaymentMode,
    rating :: Int,
    selectedItem :: IndividualRideCardState,
    tripId :: String,
    config :: AppConfig,
    vehicleVariant :: Maybe VehicleVariant,
    categories :: Array CTA.CategoryListType
    -- bookingId :: String
  }

type TripDetailsScreenProps =
  {
    reportIssue :: Boolean,
    issueReported :: Boolean,
    activateSubmit :: Boolean,
    fromMyRides :: TripDetailsGoBackType,
    showConfirmationPopUp :: Boolean,
    canConnectWithDriver :: Boolean,
    triggerUIUpdate :: Boolean,
    showIssueOptions :: Boolean
  }

data TripDetailsGoBackType = Home | MyRides | HelpAndSupport
derive instance genericTripDetailsGoBackType :: Generic TripDetailsGoBackType _
instance showTripDetailsGoBackType :: Show TripDetailsGoBackType where show = genericShow
instance eqTripDetailsGoBackType :: Eq TripDetailsGoBackType where eq = genericEq

-- ######################################  InvoiceScreenState   ######################################

type InvoiceScreenState =
  {
    data :: InvoiceScreenData,
    props :: InvoiceScreenProps
  }

type InvoiceScreenData =
  {
    tripCharges :: String,
    promotion :: Number,
    gst :: Number,
    totalAmount :: String,
    date :: String ,
    selectedItem :: IndividualRideCardState,
    config :: AppConfig,
    logField :: Object Foreign,
    pdfHeading :: String
  }

type InvoiceScreenProps =
  {
    paymentMode :: String
  , fromHomeScreen :: Boolean
  }

-- ################################################ ContactUsScreen ##################################################

type ContactUsScreenState =
  {
    data :: ContactUsScreenData,
    props :: ContactUsScreenProps
  }

type ContactUsScreenData =
  {
    email :: String,
    subject :: String,
    description :: String,
    bookingId :: String,
    errorMessage :: Maybe ErrorType,
    config :: AppConfig,
    logField :: Object Foreign
  }

type ContactUsScreenProps =
  {
    btnActive :: Boolean,
    isSubmitted :: Boolean
  }




type IssueInfo = {
    issueReportId :: String,
    status :: String,
    category :: String,
    createdAt :: String,
    issueReportShortId :: Maybe String,
    optionLabel :: Maybe String,
    rideId :: Maybe String
}

-- ################################################ MyRidesScreenState ##################################################
data AnimationState
  = AnimatedIn
  | AnimatingIn
  | AnimatingOut
  | AnimatedOut

derive instance genericAnimationState :: Generic AnimationState _
instance showAnimationState :: Show AnimationState where show = genericShow
instance eqAnimationState :: Eq AnimationState where eq = genericEq
instance encodeAnimationState :: Encode AnimationState where encode = defaultEnumEncode
instance decodeAnimationState :: Decode AnimationState where decode = defaultEnumDecode

type MyRidesScreenState =
  {
    shimmerLoader :: AnimationState,
    prestoListArrayItems :: Array ItemState,
    itemsRides :: Array IndividualRideCardState,
    props :: MyRideScreenProps,
    data :: MyRideScreenData
  }

type MyRideScreenData = {
    selectedItem :: IndividualRideCardState,
    offsetValue :: Int,
    loadMoreText :: Boolean,
    config :: AppConfig,
    logField :: Object Foreign,
    isSrcServiceable :: Boolean
  }

type MyRideScreenProps = {
  loaderButtonVisibility :: Boolean,
  loadMoreDisabled :: Boolean,
  receivedResponse :: Boolean,
  apiFailure :: Boolean,
  fromNavBar :: Boolean,
  optionsVisibility :: Boolean,
  fromBanner :: Boolean
}
-- ################################################ IndividualRideCardState ##################################################

type IndividualRideCardState =
  {
    date :: String,
    time :: String,
    source :: String,
    destination :: String,
    totalAmount :: String,
    cardVisibility :: String,
    shimmerVisibility :: String,
    driverImage :: String,
    isCancelled :: String,
    isSuccessfull :: String,
    rating :: Int,
    driverName :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    vehicleNumber :: String,
    rideId :: String,
    status :: String,
    shortRideId :: String,
    bookingId :: String,
    rideEndTimeUTC :: String,
    sourceLocation :: BookingLocationAPIEntity,
    destinationLocation :: BookingLocationAPIEntity,
    alpha :: String,
    fareBreakUpList :: Fares, -- Added only For Backward Compatibility
    faresList :: Array FareComponent ,
    baseFare :: String -- Added only For Backward Compatibility
  , pickupCharges :: String
  , extraFare :: String
  , waitingCharges :: String
  , baseDistance :: String
  , extraDistance :: String
  , referenceString :: String
  , isSpecialZone :: Boolean
  , nightCharges :: Boolean
  , zoneType :: ZoneType
  , vehicleVariant :: Maybe VehicleVariant
  , isSrcServiceable :: Boolean
  , optionsVisibility :: Boolean
  , merchantExoPhone :: String
  , serviceTierName :: Maybe String
  , totalTime :: String
  , vehicleModel :: String
  , rideStartTimeUTC :: String
  , providerName :: String
  , providerType :: CTA.ProviderType
  , showRepeatRide :: String
  , rideType :: FareProductType
  , estimatedDistance :: Int
  , isScheduled :: String
  , estimatedDuration :: Int
  , estimatedFare :: Int
  , showDestination :: String
  , rideScheduledTime :: String
  }


data VehicleVariant = SUV | SEDAN | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS

derive instance genericVehicleVariant :: Generic VehicleVariant _
instance eqVehicleVariant :: Eq VehicleVariant where eq = genericEq
instance showVehicleVariant :: Show VehicleVariant where show = genericShow

type ItemState =
  {
    date :: PropValue,
    time :: PropValue,
    source :: PropValue,
    destination :: PropValue,
    totalAmount :: PropValue,
    cardVisibility :: PropValue,
    shimmerVisibility :: PropValue,
    driverImage :: PropValue,
    isCancelled :: PropValue,
    isSuccessfull :: PropValue,
    isScheduled :: PropValue,
    rating :: PropValue,
    driverName :: PropValue,
    rideStartTime :: PropValue,
    rideEndTime :: PropValue,
    vehicleNumber :: PropValue,
    rideId :: PropValue,
    status :: PropValue,
    rideEndTimeUTC :: PropValue,
    alpha :: PropValue,
    zoneVisibility :: PropValue,
    variantImage :: PropValue,
    showVariantImage :: PropValue,
    showRepeatRide :: PropValue,
    showDestination :: PropValue
  }

-- ################################################ PermissionScreenState ##################################################

type PermissionScreenState = {
    appConfig :: AppConfig,
    logField :: Object Foreign,
    stage :: PermissionScreenStage
}

data PermissionScreenStage = NORMAL | LOCATION_DISABLED | INTERNET_ACTION | LOCATION_DENIED 
derive instance genericPermissionScreenStage :: Generic PermissionScreenStage _
instance eqPermissionScreenStage :: Eq PermissionScreenStage where eq = genericEq
instance showPermissionScreenStage :: Show PermissionScreenStage where show = genericShow
-- ######################################  HomeScreenState   ######################################

data Stage = HomeScreen
           | SettingPrice
           | FindingEstimate
           | ConfirmingRide
           | RideAccepted
           | ReAllocated
           | RideStarted
           | RideCompleted
           | PricingTutorial
           | SearchLocationModel
           | FindingQuotes
           | QuoteList
           | PreviousRating
           | GoToConfirmLocation
           | ConfirmingLocation
           | DistanceOutsideLimits
           | ShortDistance
           | TryAgain
           | RideRating
           | FavouriteLocationModel
           | ChatWithDriver
           | FindEstimateAndSearch
           | RetryFindingQuote
           | PickUpFarFromCurrentLocation
           | LoadMap
           | ProviderSelection
           | RideSearch
           | ConfirmRentalRide
           | ChangeToRideAccepted
           | ChangeToRideStarted
           | ConfirmingQuotes
           | GoToTripSelect

derive instance genericStage :: Generic Stage _
instance eqStage :: Eq Stage where eq = genericEq
instance showStage :: Show Stage where show = genericShow

data SearchLocationModelType = SearchLocation | LocateOnMap | NoView | SelectTripType

data PopupType = Logout | ConfirmBack | NoPopUp | ActiveQuotePopUp | TipsPopUp | CancelConfirmingQuotes

derive instance genericPopupType :: Generic PopupType _
instance eqPopupType :: Eq PopupType where eq = genericEq

derive instance genericSearchLocationModelType :: Generic SearchLocationModelType _
instance eqSearchLocationModelType :: Eq SearchLocationModelType where eq = genericEq

type HomeScreenState =
  {
    data :: HomeScreenStateData,
    props :: HomeScreenStateProps
  }

type HomeScreenStateData =
  {
    suggestedAmount :: Int
  , finalAmount :: Int
  , startedAt :: String
  , endedAt :: String
  , source :: String
  , destination :: String
  , eta :: String
  , vehicleDetails :: String
  , registrationNumber :: String
  , rating :: Number
  , locationList :: Array LocationListItemState
  , savedLocations :: Array LocationListItemState
  , recentSearchs :: RecentlySearchedObject
  , destinationSuggestions :: Array LocationListItemState
  , tripSuggestions :: Array Trip
  , selectList :: Array QuoteAPIEntity
  , quoteListModelState :: Array QuoteListItemState
  , driverInfoCardState :: DriverInfoCard
  , rideRatingState :: RatingCard
  , settingSideBar :: SettingSideBarState
  , sourceAddress :: Address
  , destinationAddress :: Address
  , route :: Maybe Route
  , startedAtUTC :: String
  , rateCard :: RateCard
  , speed :: Int
  , selectedLocationListItem :: Maybe LocationListItemState
  , saveFavouriteCard :: SaveFavouriteCardState
  , rideDistance :: String
  , rideDuration :: String
  , showPreferences :: Boolean
  , previousCurrentLocations:: PreviousCurrentLocations
  , messages :: Array ChatComponentConfig
  , messagesSize :: String
  , chatSuggestionsList :: Array String
  , messageToBeSent :: String
  , nearByPickUpPoints :: Array Location
  , polygonCoordinates :: String
  , specialZoneQuoteList :: Array ChooseVehicle.Config
  , specialZoneSelectedQuote :: Maybe String
  , specialZoneSelectedVariant :: Maybe String
  , quoteList :: Array ChooseVehicle.Config
  , selectedQuoteId :: Maybe String
  , selectedQuoteVariant :: Maybe String
  , selectedEstimatesObject :: ChooseVehicle.Config
  , lastMessage :: ChatComponentConfig
  , cancelRideConfirmationData :: CancelRideConfirmationData
  , ratingViewState :: RatingViewState
  , config :: AppConfig
  , logField :: Object Foreign
  , nearByDrivers :: Maybe Int
  , disability :: Maybe DisabilityT
  , searchLocationModelData :: SearchLocationModelData
  , waitTimeInfo :: Boolean
  , lastSentMessage :: ChatComponent
  , lastReceivedMessage :: ChatComponent
  , triggerPatchCounter :: Int
  , infoCardPeekHeight :: Int
  , suggestionsData :: SuggestionsData
  , peekHeight :: Int
  , rideHistoryTrip :: Maybe Trip
  , rentalsInfo :: Maybe RentalsInfo
  , bannerData :: BannerCarousalData
  , contactList :: Maybe (Array NewContacts)
  , followers :: Maybe (Array Followers)
  , vehicleVariant :: String
  , hotSpotInfo :: Array HotSpotData
  , startTimeUTC :: String
  , returnTimeUTC :: String
  , estReturnTimeUTC :: String
  , selectedDateTimeConfig :: DateTimeConfig
  , fareProductType :: FareProductType
  , invalidBookingId :: Maybe String
  , iopState :: InteroperabilityState
  , currentCityConfig :: MRC.CityConfig
  , otherSelectedEstimates :: Array String
  , rateCardCache :: Maybe RateCard
  , maxEstimatedDuration :: Int
  , invalidBookingPopUpConfig :: Maybe InvalidBookingPopUpConfig
  , rideCompletedData :: RideCompletedData -- put necesssary data which is required in ride completed screen
  , tripTypeDataConfig :: TripTypeConfig
  , destCity :: Maybe String 
  , srcCity :: Maybe String
  , tripEstDuration :: Int
  }

type InteroperabilityState = {
  timerId :: String,
  timerVal :: String,
  showMultiProvider :: Boolean,
  providerPrefVisible :: Boolean,
  providerSelectionStage :: Boolean,
  showPrefButton :: Boolean,
  providerPrefInfo :: Boolean,
  hasTopProviderEstimate :: Boolean
 }
 
type InvalidBookingPopUpConfig = {
    fromLocation :: String
  , toLocation :: String
  , bookingId :: String
  , rideScheduledTime :: String
  , maxEstimatedDuration :: Int
  , fareProductType :: FareProductType
}

type RentalsInfo = 
  { rideScheduledAtUTC :: String 
  , bookingId :: String
  , multipleScheduled :: Boolean
  , fareProductType :: FareProductType
  , nearestRideScheduledAtUTC :: String
  , vehicleVariant :: String
  }
  
type Followers = {
  name :: Maybe String,
  bookingId :: String,
  mobileNumber :: String,
  priority :: Int
}

type QuoteListItemState = 
  {
    seconds :: Int
  , id :: String  
  , timer :: String
  , timeLeft :: Int
  , driverRating :: Number
  , profile :: String
  , price :: String
  , vehicleType :: String
  , driverName :: String
  , selectedQuote :: Maybe String
  , appConfig :: AppConfig
  , city :: City
  , vehicleImage :: String
  , serviceTierName :: Maybe String
  }


type LocationDetails = {
    formattedAddress :: String,
    location :: LatLong,
    plusCode :: Maybe String,
    addressComponents :: Array AddressComponents,
    placeId :: Maybe String
  }

type BannerCarousalData = {
  bannerItem :: Maybe ListItem,
  currentBanner :: Int,
  bannerScrollState :: String,
  currentPage :: Int
}


type RideCompletedData = {
  issueReportData :: IssueReportData
, toll :: FinalTollData
}

type FinalTollData = {
  confidence :: Maybe CTA.Confidence
, showAmbiguousPopUp :: Boolean
}

type IssueReportData = {
  bannerItem :: Maybe ListItem
, currentBannerIndex :: Int
, currentPageIndex :: Int
, showIssueBanners :: Boolean
, hasAccessibilityIssue :: Boolean
, hasTollIssue :: Boolean
, hasSafetyIssue :: Boolean
, customerResponse :: Array {issueType :: CTA.CustomerIssueTypes, selectedYes :: Maybe Boolean}
, respondedValidIssues :: Boolean
}

type DisabilityT = 
  {
    id :: String
  , tag :: String
  , description :: String
  }

type DisabilityData = {
    activeIndex :: Int
  , specialAssistActiveIndex :: Int
  , disabilityOptionList :: Array DisabilityT
  , selectedDisability :: Maybe DisabilityT
  , otherDisabilityReason :: Maybe String 
  , editedDisabilityReason :: String
}

type HomeScreenStateProps =
  {
    currentStage :: Stage
  , showCallPopUp :: Boolean
  , rideRequestFlow :: Boolean
  , isSearchLocation :: SearchLocationModelType
  , isSource :: Maybe Boolean
  , sourceLat :: Number
  , sourceLong :: Number
  , destinationLat :: Number
  , destinationLong :: Number
  , canScheduleRide :: Boolean
  , stopLoc :: Maybe {
      lat :: Number
    , lng :: Number
    , stopLocAddress :: String
    }
  , sourcePlaceId :: Maybe String
  , destinationPlaceId :: Maybe String
  , estimateId :: String
  , selectedQuote :: Maybe String
  , locationRequestCount :: Int
  , searchId :: String
  , bookingId :: String
  , customerTip :: CustomerTipProps
  , expiredQuotes :: Array String
  , isCancelRide :: Boolean
  , cancellationReasons :: Array CTA.OptionButtonList
  , cancelRideActiveIndex :: Maybe Int
  , cancelDescription :: String
  , cancelReasonCode :: String
  , isPopUp :: PopupType
  , forFirst :: Boolean
  , callbackInitiated :: Boolean
  , isLocationTracking :: Boolean
  , isInApp :: Boolean
  , locateOnMap :: Boolean
  , distance :: Int
  , isSrcServiceable :: Boolean
  , isDestServiceable :: Boolean
  , isRideServiceable :: Boolean
  , showlocUnserviceablePopUp :: Boolean
  , isMockLocation :: Boolean
  , autoSelecting :: Boolean
  , searchExpire :: Int
  , isEstimateChanged :: Boolean
  , showRateCard :: Boolean
  , showRateCardIcon :: Boolean
  , sendMessageActive :: Boolean
  , chatcallbackInitiated :: Boolean
  , estimatedDistance :: Maybe Int
  , waitingTimeTimerIds :: Array String
  , tagType :: Maybe CardType
  , isSaveFavourite :: Boolean
  , showShareAppPopUp :: Boolean
  , showMultipleRideInfo :: Boolean
  , hasTakenRide :: Boolean
  , isReferred :: Boolean
  , storeCurrentLocs :: Boolean
  , unReadMessages :: Boolean
  , openChatScreen :: Boolean
  , emergencyHelpModelState :: EmergencyHelpModelState
  , showLiveDashboard :: Boolean
  , isBanner :: Boolean
  , callSupportPopUp :: Boolean
  , defaultPickUpPoint :: String
  , isSpecialZone :: Boolean
  , showChatNotification :: Boolean
  , cancelSearchCallDriver :: Boolean
  , zoneType :: SpecialTags
  , cancelRideConfirmationPopup :: Boolean
  , searchAfterEstimate :: Boolean
  , tipViewProps :: TipViewProps
  , timerId :: String
  , findingRidesAgain :: Boolean
  , routeEndPoints :: Maybe RouteEndPoints
  , findingQuotesProgress :: Number
  , confirmLocationCategory :: ZoneType
  , zoneTimerExpired :: Boolean
  , canSendSuggestion :: Boolean
  , sheetState :: Maybe BottomSheetState
  , currentSheetState :: BottomSheetState
  , showDisabilityPopUp :: Boolean
  , isChatNotificationDismissed :: Boolean
  , searchLocationModelProps :: SearchLocationModelProps
  , flowWithoutOffers :: Boolean
  , showEducationalCarousel :: Boolean
  , locateOnMapLocation :: LocateOnMapLocation
  , currentLocation :: Location
  , isShorterTrip :: Boolean
  , isNotificationExpanded :: Boolean
  , bottomSheetState :: SheetState
  , removeNotification :: Boolean
  , city :: City
  , destCity :: Maybe City
  , isHomescreenExpanded :: Boolean
  , isRepeatRide :: Boolean
  , currSlideIndex :: Number
  , suggestionsListExpanded :: Boolean
  , repeatRideTimer :: String
  , repeatRideTimerId :: String
  , showShimmer :: Boolean
  , reAllocation :: ReAllocationProp
  , homeScreenSheetState :: BottomSheetState
  , autoScrollTimer :: String
  , autoScrollTimerId :: String
  , autoScroll :: Boolean
  , enableChatWidget :: Boolean
  , focussedBottomIcon :: BottomNavBarIcon
  , sosBannerType :: Maybe SosBannerType
  , showShareRide :: Boolean
  , followsRide :: Boolean
  , isChatWithEMEnabled :: Boolean
  , referral :: ReferralStatusProp
  , showBookingPreference :: Boolean
  , safetyAlertType :: Maybe SafetyAlertType
  , rideSearchProps :: RideSearchProps
  , selectedEstimateHeight :: Int
  , suggestedRideFlow :: Boolean
  , isSafetyCenterDisabled :: Boolean
  , locateOnMapProps :: LocateOnMapProps
  , showSpecialZoneInfoPopup :: Boolean
  , hotSpot :: HotSpotProps
  , isBannerDataComputed :: Boolean
  , repeatRideVariant :: String
  , hasToll :: Boolean
  , repeatRideServiceTierName :: Maybe String
  , isSearchCancelled :: Boolean
  , referralComponentProps :: ReferralComponentState
  , showAcWorkingPopup :: Boolean
  , repeateRideTimerStoped :: Boolean
  , currentEstimateHeight :: Int
  , showEndOTP :: Boolean
  , rideDurationTimer :: String
  , rideDurationTimerId :: String
  , showRentalInfo :: Boolean
  , maxDateBooking :: Int
  , showIntercityUnserviceablePopUp :: Boolean
  , showNormalRideNotSchedulablePopUp :: Boolean
  , zoneOtpExpired :: Boolean
  , stageBeforeChatScreen :: Stage
  , specialZoneType :: String
  , scheduledRidePollingDelay :: Number
  , startScheduledRidePolling :: Boolean
  , showScheduledRideExistsPopUp :: Boolean
  }

data BottomNavBarIcon = TICKETING | MOBILITY

type BookingTime = {
  rideStartTime :: String,
  bookingId :: String,
  estimatedDuration :: Int
}

derive instance genericBottomNavBarIcon :: Generic BottomNavBarIcon _
instance showBottomNavBarIcon :: Show BottomNavBarIcon where show = genericShow
instance eqBottomNavBarIcon :: Eq BottomNavBarIcon where eq = genericEq

data SafetyAlertType = DEVIATION | STATIONARY_VEHICLE

derive instance genericSafetyAlertType :: Generic SafetyAlertType _
instance showSafetyAlertType :: Show SafetyAlertType where show = genericShow
instance eqSafetyAlertType :: Eq SafetyAlertType where eq = genericEq

data City
  = Bangalore
  | Kolkata
  | Paris
  | Kochi
  | Delhi
  | Hyderabad
  | Mumbai
  | Chennai
  | Coimbatore
  | Pondicherry
  | Goa
  | Pune
  | Mysore
  | Tumakuru
  | AnyCity

derive instance genericCity :: Generic City _
instance showCity :: Show City where show = genericShow
instance eqCity :: Eq City where eq = genericEq
instance encodeCity :: Encode City where encode = defaultEnumEncode
instance decodeCity :: Decode City where decode = defaultEnumDecode

type SearchLocationModelProps = {
    isAutoComplete :: Boolean
  , showLoader :: Boolean
  , crossBtnSrcVisibility :: Boolean
  , crossBtnDestVisibility :: Boolean
  , tripType :: TicketType
  , totalRideDistance :: Number
  , totalRideDuration :: Int
}

type TripTypeConfig = {
  tripPickupData :: Maybe TripTypeData,
  tripReturnData :: Maybe TripTypeData
}

type TripTypeData =  {
  tripDateTimeConfig :: DateTimeConfig,
  tripDateUTC :: String,
  tripDateReadableString :: String 
}

type SearchLocationModelData = {
    prevLocation :: String
}

type LocateOnMapLocation = {
    source :: String
  , sourceAddress :: Address
  , sourceLat :: Number
  , sourceLng :: Number
  , destination :: String
  , destinationAddress :: Address
  , destinationLat :: Number
  , destinationLng :: Number
}

type RouteEndPoints = {
    source :: Location
  , destination :: Location
}

type SpecialTags = {
    sourceTag :: ZoneType
  , destinationTag :: ZoneType
  , priorityTag :: ZoneType
}

type CancelRideConfirmationData = {
  delayInSeconds :: Int,
  timerID :: String,
  enableTimer :: Boolean,
  continueEnabled :: Boolean
}
type RatingViewState = {
    selectedYes :: Maybe Boolean,
    selectedRating :: Int,
    issueReportActiveIndex :: Maybe Int,
    issueReasonCode :: Maybe String,
    openReportIssue :: Boolean,
    doneButtonVisibility :: Boolean,
    issueReason :: Maybe String,
    issueDescription :: String,
    rideBookingRes :: RideBookingRes,
    wasOfferedAssistance :: Maybe Boolean
}

type CustomerTipProps = {
    enableTips :: Boolean
  , tipActiveIndex :: Int
  , tipForDriver :: Int
  , isTipSelected :: Boolean
}

data SheetState = STATE_DRAGGING | STATE_SETTLING | STATE_EXPANDED | STATE_COLLAPSED | STATE_HIDDEN | STATE_HALF_EXPANDED

derive instance genericSheetState :: Generic SheetState _
instance showSheetState :: Show SheetState where show = genericShow
instance eqSheetState :: Eq SheetState where eq = genericEq
instance encodeSheetState :: Encode SheetState where encode = defaultEnumEncode
instance decodeSheetState :: Decode SheetState where decode = defaultEnumDecode

data TipViewStage = DEFAULT | TIP_AMOUNT_SELECTED | TIP_ADDED_TO_SEARCH | RETRY_SEARCH_WITH_TIP

derive instance genericTipViewStage :: Generic TipViewStage _
instance showTipViewStage :: Show TipViewStage where show = genericShow
instance eqTipViewStage :: Eq TipViewStage where eq = genericEq
instance encodeTipViewStage :: Encode TipViewStage where encode = defaultEncode
instance decodeTipViewStage :: Decode TipViewStage where decode = defaultDecode

type TipViewProps = {
    stage :: TipViewStage
  , isVisible :: Boolean
  , onlyPrimaryText :: Boolean
  , isprimaryButtonVisible :: Boolean
  , primaryText :: String
  , secondaryText :: String
  , customerTipArray :: Array String
  , customerTipArrayWithValues :: Array Int
  , activeIndex :: Int
  , primaryButtonText :: String
}

type Contact = {
     name :: String,
     phoneNo :: String
}

type RateCard =
  {
    additionalFare :: Int,
    createdTime :: String,
    tollCharge :: Number,
    waitingTimeInfo :: CTA.WaitingTimeInfo,
    currentRateCardType :: CTA.RateCardType,
    driverAdditions :: Array CTA.FareList,
    extraFare :: Array CTA.FareList,
    fareInfoDescription :: Array String,
    isNightShift :: Boolean,
    nightChargeFrom :: String,
    nightChargeTill :: String,
    onFirstPage :: Boolean,
    pickUpCharges :: Number,
    vehicleVariant :: String,
    baseFare :: Int
  }


type RateCardDetails = {
  title :: String ,
  description :: String
}

data SosBannerType = SETUP_BANNER | MOCK_DRILL_BANNER

derive instance genericSosBannerType :: Generic SosBannerType _
instance showSosBannerType :: Show SosBannerType where show = genericShow
instance eqSosBannerType :: Eq SosBannerType where eq = genericEq

type EmergencyHelpModelState = {
   currentlySelectedContact :: Contact,
   showCallSuccessfulPopUp :: Boolean,
   showCallContactPopUp :: Boolean,
   sosId :: String,
   sosStatus :: String,
   isSelectEmergencyContact :: Boolean,
   showContactSupportPopUp :: Boolean,
   showCallPolicePopUp :: Boolean,
   emergencyContactData :: Array Contact,
   waitingDialerCallback :: Boolean
}

type RecentlySearchedObject =
  {
    predictionArray :: Array LocationListItemState
  }

type ReferralScreenState =
  {   referralCode :: String
    , btnActive :: Boolean
    , showThanks :: Boolean
    , isInvalidCode :: Boolean
    , isExpandReference :: Boolean
    , config :: AppConfig
    , logField :: Object Foreign
    , referralType :: ReferralType
    , showQRCodePopUp :: Boolean
    , referralComponentProps :: ReferralComponentState
  }


-- ################################## SelectLanguageScreenState ###############################

type SelectLanguageScreenState = {
  data :: SelectLanguageScreenData,
  props :: SelectLanguageScreenProps
}

type SelectLanguageScreenData =  {
  isSelected :: Boolean,
  config :: AppConfig
 }

type SelectLanguageScreenProps =  {
  selectedLanguage :: String,
  btnActive :: Boolean
 }

-- ############################################## EmergencyContactsScreenState #############################


type EmergencyContactsScreenState = {
  data :: EmergencyContactsScreenData,
  props :: EmergencyContactsScreenProps
}

type EmergencyContactsScreenData = {
  emergencyContactsList :: Array NewContacts,
  storedContactsList :: Array NewContacts,
  selectedContacts :: Array NewContacts,
  searchResult :: Array NewContacts,
  prestoListArrayItems :: Array NewContactsProp,
  loadMoreDisabled :: Boolean,
  removedContactDetail :: NewContacts,
  offsetForEmergencyContacts :: Int,
  limitForEmergencyContacts :: Int,
  editedText :: String,
  logField :: Object Foreign
}

type EmergencyContactsScreenProps = {
  showContactList :: Boolean,
  showInfoPopUp :: Boolean,
  fromSosFlow :: Boolean,
  appName :: String
}

type ContactDetail = {
  name :: String,
  phoneNumber :: String
}

-- ############################################## AboutUsScreenState #############################

type AboutUsScreenState = {
    appConfig :: AppConfig
}

-- ############################################## MyProfileScreenState #############################

type MyProfileScreenState = {
  props :: MyProfileScreenProps,
  data :: MyProfileScreenData
}
data DeleteStatus = CONFIRM_REQ | DEL_REQUESTED | ACTIVE

derive instance genericDeleteStatus :: Generic DeleteStatus _
instance showDeleteStatus :: Show DeleteStatus where show = genericShow
instance eqDeleteStatus :: Eq DeleteStatus where eq = genericEq
instance encodeDeleteStatus :: Encode DeleteStatus where encode = defaultEnumEncode
instance decodeDeleteStatus :: Decode DeleteStatus where decode = defaultEnumDecode


type MyProfileScreenProps = {
  updateProfile :: Boolean,
  genderOptionExpanded :: Boolean,
  expandEnabled :: Boolean,
  isEmailValid :: Boolean,
  isNameValid :: Boolean,
  isBtnEnabled :: Boolean,
  showOptions :: Boolean,
  fromHomeScreen :: Boolean,
  showAccessibilityPopUp :: Boolean,
  changeAccessibility :: Boolean,
  isSpecialAssistList :: Boolean
}

data FieldType = NAME | EMAILID_ | GENDER_ | MOBILE | DISABILITY_TYPE

derive instance genericFieldType :: Generic FieldType _
instance eqFieldType :: Eq FieldType where eq = genericEq

type MyProfileScreenData = {
  name :: String,
  mobileNumber :: String,
  editedName :: String,
  emailId :: Maybe String,
  gender :: Maybe Gender,
  editedEmailId :: Maybe String,
  editedGender :: Maybe Gender,
  emailErrorMessage :: Maybe ErrorType,
  nameErrorMessage :: Maybe ErrorType,
  config :: AppConfig,
  logField :: Object Foreign,
  disabilityType :: Maybe DisabilityT,
  disabilityOptions :: DisabilityData,
  editedDisabilityOptions :: DisabilityData,
  hasDisability :: Maybe Boolean
}

data ErrorType = INVALID_EMAIL | EMAIL_EXISTS | EMAIL_CANNOT_BE_BLANK | INVALID_NAME | NAME_CANNOT_BE_BLANK


derive instance genericErrorType :: Generic ErrorType _
instance eqErrorType :: Eq ErrorType where eq = genericEq

type DriverInfoCard =
  { otp :: String
  , driverName :: String
  , eta :: Maybe Int
  , vehicleDetails :: String
  , registrationNumber :: String
  , rating :: Number
  , startedAt :: String
  , endedAt :: String
  , source :: String
  , destination :: String
  , rideId :: String
  , price :: Int
  , sourceLat :: Number
  , sourceLng :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  , bppRideId :: String
  , driverNumber :: Maybe String
  , merchantExoPhone :: String
  , createdAt :: String
  , initDistance :: Maybe Int
  , config :: AppConfig
  , vehicleVariant :: String
  , sourceAddress :: Address
  , destinationAddress :: Address
  , status :: String
  , serviceTierName :: Maybe String
  , vehicleModel :: String
  , vehicleColor :: String
  , providerType :: CTA.ProviderType
  , providerName :: String
  , rentalData :: CTA.RentalBookingConfig
  , fareProductType :: FareProductType
  }

type RatingCard =
  {
    rideId :: String
  , rating :: Int
  , driverName :: String
  , finalAmount :: Int
  , rideStartTime :: String
  , rideEndTime :: String
  , source :: String
  , destination :: String
  , rideStartDate :: String
  , vehicleNumber :: String
  , status :: String
  , shortRideId :: String
  , bookingId :: String
  , rideEndTimeUTC :: String
  , dateDDMMYY :: String
  , offeredFare :: Int
  , distanceDifference :: Int
  , feedback :: String
  , feedbackList :: Array CTA.FeedbackAnswer
  }

type Address =
  { area :: Maybe String
  , state :: Maybe String
  , country :: Maybe String
  , building  :: Maybe String
  , door :: Maybe String
  , street :: Maybe String
  , city :: Maybe String
  , areaCode :: Maybe String
  , ward :: Maybe String
  , placeId :: Maybe String
  }


type SavedLocationScreenState =
  {
      data :: SavedLocationScreenData
    , props :: SavedLocationScreenProps
  }

type SavedLocationScreenProps =
  {
    showDeleteLocationModel :: Boolean
  , apiRespReceived :: Boolean
  }

type SavedLocationScreenData =
  {
    savedLocations :: Array LocationListItemState
  , deleteTag :: Maybe String
  , config :: AppConfig
  , logField :: Object Foreign
  }

type DistInfo =
  { locationName :: String
  , distanceDiff :: Number
  }

type SavedLocationData =
  {
    address :: String
  , lat :: Number
  , lon :: Number
  , tag :: String
  , placeName :: String
  , placeId :: Maybe String
  }

data CallType = ANONYMOUS_CALLER | DIRECT_CALLER
derive instance genericCallType :: Generic CallType _
instance eqCallType :: Eq CallType where eq = genericEq
instance showCallType :: Show CallType where show = genericShow
instance encodeCallType :: Encode CallType where encode = defaultEnumEncode
instance decodeCallType :: Decode CallType where decode = defaultEnumDecode


data CardType = HOME_TAG | WORK_TAG | OTHER_TAG

derive instance genericCardType :: Generic CardType _
instance eqCardType :: Eq CardType where eq = genericEq
instance showCardType :: Show CardType where show = genericShow
instance encodeCardType :: Encode CardType where encode = defaultEnumEncode
instance decodeCardType :: Decode CardType where decode = defaultEnumDecode

type AddNewAddressScreenState =
  {
    data :: AddNewAddressScreenData
  , props :: AddNewAddressScreenProps
  }

type AddNewAddressScreenData =
  {
    locationList :: Array LocationListItemState
  , savedLocations :: Array LocationListItemState
  , selectedItem :: LocationListItemState
  , activeIndex :: Maybe Int
  , selectedTag :: Maybe CardType
  , savedTags :: Array String
  , addressSavedAs :: String
  , placeName :: String
  , lat :: Number
  , lon :: Number
  , editTag :: String
  , existsAs :: String
  , currentLocation :: String
  , currLat :: Maybe Number
  , currLon :: Maybe Number
  , address :: String
  , recentSearchs :: RecentlySearchedObject
  , locSelectedFromMap :: String
  , latSelectedFromMap :: Number
  , lonSelectedFromMap :: Number
  , addressComponents :: Array AddressComponents
  , polygonCoordinates :: String
  , nearByPickUpPoints :: Array Location
  , config :: AppConfig
  }

type AddNewAddressScreenProps =
  {
    showSavePlaceView :: Boolean
  , isBtnActive :: Boolean
  , editLocation :: Boolean
  , tagExists :: Boolean
  , placeNameExists :: Boolean
  , isLocateOnMap :: Boolean
  , isLocationServiceable :: Boolean
  , fromHome :: Boolean
  , fromScreen :: String
  , selectFromCurrentOrMap :: Boolean
  , isSearchedLocationServiceable :: Boolean
  , editSavedLocation :: Boolean
  , isSpecialZone :: Boolean
  , defaultPickUpPoint :: String
  , isServiceable :: Boolean
  }

type AppUpdatePopUpState =
 { version :: Int ,
   logField :: Object Foreign,
   updatePopup :: UpdatePopupType,
   appUpdatedView :: AppUpdatedViewState,
   config :: AppConfig
 }

type AppUpdatedViewState = {
  primaryText :: String,
  secondaryText :: String,
  optionTwoText :: String,
  coverImageUrl :: String
}

data UpdatePopupType =  AppVersion
                      | DateAndTime
                      | NoUpdatePopup
                      | AppUpdated

derive instance genericUpdatePopupType :: Generic UpdatePopupType _
instance showUpdatePopupType :: Show UpdatePopupType where show = genericShow
instance eqUpdatePopupType :: Eq UpdatePopupType where eq = genericEq

data NotifyFlowEventType = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED

derive instance genericNotifyFlowEventType :: Generic NotifyFlowEventType _
instance showNotifyFlowEventType :: Show NotifyFlowEventType where show = genericShow
data LocItemType = LOC_LIST | CURR_LOC | LOCATE_ON_MAP

derive instance genericLocItemType :: Generic LocItemType _
instance eqLocItemType :: Eq LocItemType where eq = genericEq
instance showLocItemType :: Show LocItemType where show = genericShow
instance encodeLocItemType :: Encode LocItemType where encode = defaultEnumEncode
instance decodeLocItemType:: Decode LocItemType where decode = defaultEnumDecode

data SearchResultType = QUOTES | ESTIMATES | RENTALS | INTERCITY

derive instance genericSearchResultType :: Generic SearchResultType _
instance eqSearchResultType :: Eq SearchResultType where eq = genericEq
instance showSearchResultType :: Show SearchResultType where show = genericShow

type LocationTagBarState =
  { savedLocations :: Array LocationListItemState }

type LocationListItemState = {
    prefixImageUrl :: String
  , postfixImageUrl :: String
  , postfixImageVisibility :: Boolean
  , title :: String
  , subTitle :: String
  , placeId :: Maybe String
  , lat :: Maybe Number
  , lon :: Maybe Number
  , description :: String
  , tag :: String
  , tagType :: Maybe String
  , cardType :: Maybe String
  , address :: String
  , tagName :: String
  , isEditEnabled :: Boolean
  , savedLocation :: String
  , placeName :: String
  , isClickable :: Boolean
  , alpha :: Number
  , fullAddress :: Address
  , locationItemType :: Maybe LocationItemType
  , distance :: Maybe String
  , showDistance :: Maybe Boolean
  , actualDistance :: Maybe Int
  , frequencyCount :: Maybe Int
  , recencyDate :: Maybe String
  , locationScore :: Maybe Number
}

type SuggestionsMap = Map SourceGeoHash Suggestions
type SourceGeoHash = String
type DestinationGeoHash = String

type Suggestions = {
    destinationSuggestions :: Array LocationListItemState
  , tripSuggestions :: Array Trip 
}

type Trip = {
    sourceLat :: Number
  , source :: String 
  , destination :: String
  , sourceAddress :: Address 
  , destinationAddress :: Address
  , sourceLong :: Number
  , destLat :: Number
  , destLong :: Number
  , frequencyCount :: Maybe Int
  , recencyDate :: Maybe String
  , locationScore :: Maybe Number
  , isSpecialZone :: Boolean
  , vehicleVariant :: Maybe String
  , serviceTierNameV2 :: Maybe String
}
type SuggestionsData =  {
    suggestionsMap :: SuggestionsMap
}

data LocationItemType = RECENTS | PREDICTION | SAVED_LOCATION | SUGGESTED_DESTINATIONS

derive instance genericLocationItemType :: Generic LocationItemType _
instance eqLocationItemType :: Eq LocationItemType where eq = genericEq
instance showLocationItemType :: Show LocationItemType where show = genericShow
instance encodeLocationItemType :: Encode LocationItemType where encode = defaultEnumEncode
instance decodeLocationItemType:: Decode LocationItemType where decode = defaultEnumDecode
instance encodeJsonLocationItemType :: EncodeJson LocationItemType where
  encodeJson = genericEncodeJson
instance decodeJsonLocationItemType :: DecodeJson LocationItemType where
  decodeJson = genericDecodeJson
  

type SaveFavouriteCardState =
  {
    address :: String
  , tag :: String
  , tagExists :: Boolean
  , selectedItem :: LocationListItemState
  , isBtnActive :: Boolean
  }

type Fares = {
  baseFare :: String
, pickupCharges :: String
, nominalFare :: String
, waitingCharges :: String
}

type FareComponent = {
  fareType :: String
, price :: String
, title :: String
}

type SuccessScreenState = {
    title :: String
  , subTitle :: String
}
type CurrentLocationDetails =  {
    lat :: Number
  , lon :: Number
  , placeName :: String
  }

type PreviousCurrentLocations =  {
    pastCurrentLocations :: Array CurrentLocationDetails
  }
type CurrentLocationDetailsWithDistance =  {
    locationDetails :: CurrentLocationDetails
  , distance :: Number
}

newtype FlowStatusData = FlowStatusData {
    source :: Location
  , destination :: Location
  , sourceAddress :: Address
  , destinationAddress :: Address
  , sourceLabelIcon :: Maybe String
  , destLabelIcon :: Maybe String
  , sourceGeoJson :: Maybe String
  , sourceGates :: Maybe (Array Location)
}

derive instance genericFlowStatusData :: Generic FlowStatusData _
instance showFlowStatusData :: Show FlowStatusData where show = genericShow
instance encodeFlowStatusData :: Encode FlowStatusData where encode = defaultEncode
instance decodeFlowStatusData :: Decode FlowStatusData where decode = defaultDecode

data ZoneType = METRO
              | SHOPPING_MALL
              | HOSPITAL
              | AIRPORT
              | SCHOOL
              | RAILWAY
              | NOZONE
              | AUTO_BLOCKED
              | SPECIAL_PICKUP
              | HOTSPOT Boolean

derive instance genericZoneType :: Generic ZoneType _
instance showZoneType :: Show ZoneType where show = genericShow
instance eqZoneType :: Eq ZoneType where eq = genericEq
instance encodeZoneType :: Encode ZoneType where encode = defaultEncode
instance decodeZoneType :: Decode ZoneType where decode = defaultDecode


newtype TipViewData = TipViewData {
    stage :: TipViewStage
  , activeIndex :: Int
  , isVisible :: Boolean
}

derive instance genericTipViewData :: Generic TipViewData _
instance showTipViewData :: Show TipViewData where show = genericShow
instance encodeTipViewData :: Encode TipViewData where encode = defaultEncode
instance decodeTipViewData :: Decode TipViewData where decode = defaultDecode

-- ###################################### TicketInfoScreen ######################################

type TicketInfoScreenState =
  { data :: TicketInfoScreenData,
    props :: TicketInfoScreenProps
  }

type TicketInfoScreenData = {
  selectedBookingInfo :: IndividualBookingItem
}

type TicketInfoScreenProps = {
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  leftButtonDisable :: Boolean,
  rightButtonDisable :: Boolean
}


-- ######################################  TicketBookingScreen   ######################################

type TicketBookingScreenState =
  { data :: TicketBookingScreenData,
    props :: TicketBookingScreenProps
  }

type TicketServiceData =
  { id :: String,
    serviceName :: String,
    allowFutureBooking :: Boolean,
    shortDesc :: Maybe String,
    expiry :: ServiceExpiry,
    isExpanded :: Boolean,
    serviceCategories :: Array ServiceCategory,
    selectedBHId :: Maybe String
  }

type ServiceCategory =
  { categoryId :: String,
    categoryName :: String,
    availableSeats :: Maybe Int,
    allowedSeats :: Maybe Int,
    bookedSeats :: Int,
    isSelected :: Boolean,
    peopleCategories :: Array PeopleCategoriesData,
    operationalDays :: Array OperationalDaysData,
    validOpDay :: Maybe OperationalDaysData
  }

type FlattenedBusinessHourData =
  { id :: String,
    slot :: Maybe String, -- array of slots
    startTime :: Maybe String,
    endTime :: Maybe String,
    specialDayDescription :: Maybe String,
    specialDayType :: Maybe String,
    operationalDays :: Array String,
    category :: TicketCategoriesResp
  }

type PeopleCategoriesData =
  { peopleCategoryName :: String,
    pricePerUnit :: Int,
    currentValue :: Int,
    peopleCategoryId :: String,
    ticketLimitCrossed :: Boolean
  }

type OperationalDaysData = 
  { operationalDays :: Array String,
    slot :: Array SlotInterval,
    timeIntervals :: Array TimeInterval
  }

type SlotInterval = {
  bhourId :: String,
  slot :: String
}
type TimeInterval = {
  bhourId :: String,
  startTime :: String,
  endTime :: String
}

type TiketingListTransformedData =
  { timings :: Array KeyVal,
    fees :: Array KeyVal
  }

type KVPairArr =
  { key :: String
  , val :: Array KeyVal
  , disableCategory :: Boolean
  }

type TicketBookingScreenData = {
  servicesAvailing :: Array TicketServiceI, -- TODO:: Use this for generic handling
  dateOfVisit :: String,
  keyValArray :: Array KeyVal,
  transactionId :: String,
  bookedForArray :: Array String,
  zooName :: String,
  totalAmount :: Int,
  placeInfo :: Maybe TicketPlaceResp,
  servicesInfo :: Array TicketServiceData,
  shortOrderId :: String,
  selectedPlaceType :: PlaceType
}

type TicketServiceI = {
  id :: String,
  peopleCategoryName :: String,
  numberOfUnits :: Int
}

type KeyVal = {
  key :: String,
  val :: String
}

type KeyVal2 = {
  key :: Array String,
  val :: String
}

type TicketBookingItem = 
  { shortId :: String,
    ticketPlaceName :: String,
    amount :: Number,
    visitDate :: String,
    status :: BookingStatus,
    ticketPlaceId ::  String,
    personId :: String
  }

type TicketBookings = 
  { pendingBooking :: Array TicketBookingItem,
    booked :: Array TicketBookingItem
  }

type TicketBookingScreenProps = {
  currentStage :: TicketBookingScreenStage,
  previousStage :: TicketBookingScreenStage,
  termsAndConditionsSelected :: Boolean,
  validDate :: Boolean,
  showShimmer :: Boolean,
  paymentStatus :: PP.PaymentStatus,
  ticketBookingList :: TicketBookings,
  selectedBookingId :: String,
  selectedBookingInfo :: IndividualBookingItem,
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  rightButtonDisable :: Boolean,
  leftButtonDisable :: Boolean,
  navigateToHome :: Boolean,
  selectedOperationalDay :: String
}

type TicketItem = {
  ticketType :: String,
  fare :: Int,
  time :: String,
  serviceId :: String,
  validTill :: String
  }

type IndividualBookingItem =
  { shortId :: String,
    ticketPlaceId :: String,
    ticketPlaceName :: String,
    personId :: String,
    amount :: Number,
    visitDate :: String,
    status :: BookingStatus,
    services :: Array TicketBookingServiceDetails
  }

type TicketBookingServiceDetails =
  { amount :: Number,
    status :: String,
    verificationCount :: Int,
    expiryDate :: Maybe String,
    ticketServiceName :: String,
    categories :: Array TicketBookingCategoryDetails,
    ticketServiceShortId :: String,
    slot :: Maybe String
  }

type TicketBookingCategoryDetails =
  { amount :: Number,
    bookedSeats :: Int,
    name :: String,
    peopleCategories :: Array TicketBookingPeopleCategoryDetails
  }

type TicketBookingPeopleCategoryDetails =
  { name :: String,
    numberOfUnits ::Int,
    pricePerUnit :: Number
  }

data TicketBookingScreenStage = DescriptionStage 
                              | ChooseTicketStage
                              | BookingConfirmationStage
                              | ViewTicketStage
                              | MyTicketsStage
                              | TicketInfoStage

derive instance genericTicketBookingScreenStage :: Generic TicketBookingScreenStage _
instance showTicketBookingScreenStage :: Show TicketBookingScreenStage where show = genericShow
instance eqTicketBookingScreenStage :: Eq TicketBookingScreenStage where eq = genericEq


-- ######################################### TicketingScreenState ####################################################

type TicketingScreenState = {
  data :: TicketingScreenData ,
  props :: TicketingScreenProps
}

type TicketingScreenData = {
  placeInfoArray :: Array TicketPlaceResp
} 

type TicketingScreenProps = {
  hideMyTickets :: Boolean
} 
type ReAllocationProp =
  { showPopUp :: Boolean
  }


-- ######################################### RideScheduledState ####################################################

type RideScheduledScreenState = {
    data :: RideScheduledScreenData,
    props :: RideScheduledScreenProps
}

type RideScheduledScreenData = {
  primaryButtonText :: String
  , source :: LocationInfo
  , destination :: Maybe LocationInfo
  , startDate :: String
  , startTime :: String
  , finalPrice :: String
  , baseDuration :: String
  , baseDistance :: String
  , bookingId :: String
  , cancellationReasons :: Array CTA.OptionButtonList
  , config :: AppConfig
  , fareProductType :: FareProductType
  , fromScreen :: String
}

type RideScheduledScreenProps = {
    cancelRideActiveIndex :: Maybe Int
  , isCancelRide :: Boolean
  , cancelDescription :: String 
  , cancelReasonCode :: String
  , driverAllocationTime :: String

}
-- ######################################### SearchLocationScreenState ####################################################

data IssueModalType = HELP_AND_SUPPORT_SCREEN_MODAL | REPORTED_ISSUES_MODAL | RESOLVED_ISSUES_MODAL

derive instance genericIssueModalType :: Generic IssueModalType _
instance eqIssueModalType :: Eq IssueModalType where eq = genericEq


-- ######################################### MetroTicketDetailsState ####################################################
type MetroTicketDetailsScreenState = {
    data :: MetroTicketDetailsScreenData
  , props :: MetroTicketDetailsScreenProps
}

type MetroTicketDetailsScreenData = {
  dummyData :: String
, bookingId :: String
, city :: City
, bookingUpdatedAt :: String
, metroRoute :: Array MetroRoute
, ticketsInfo :: Array MetroTicketInfo
, ticketType :: String
, ticketPrice :: Int
, noOfTickets :: Int
}

type MetroTicketInfo = {
  qrString :: String
, ticketNumber :: String 
, validUntil :: String
, status :: String
}

type MetroRoute = {
  name :: String
, line :: MetroLine 
, stops :: Array MetroStop
, listExpanded :: Boolean
}
data MetroLine = BlueLine 
               | GreenLine 
               | RedLine
               | NoColorLine

derive instance genericMetroLine :: Generic MetroLine _                                  
instance showMetroLine :: Show MetroLine where show = genericShow
instance eqMetroLine :: Eq MetroLine where eq = genericEq 

type MetroStop = {
  name :: String
}

type MetroTicketDetailsScreenProps = {
  dummyProps :: String
, stage :: MetroTicketDetailsScreenStage
, currentTicketIndex :: Int
, previousScreenStage :: PreviousMetroTicketDetailsStage
, isBookingCancellable :: Maybe Boolean
, cancellationCharges :: Maybe Number
, refundAmount :: Maybe Number
, showLoader :: Boolean
}

data PreviousMetroTicketDetailsStage = MetroMyTicketsStage 
                                     | SearchMetroLocationStage 
                                     | MetroTicketSelectionStage
                                     | MetroTicketStatusStage

derive instance genericPreviousMetroTicketDetailsStage :: Generic PreviousMetroTicketDetailsStage _                                  
instance showPreviousMetroTicketDetailsStage :: Show PreviousMetroTicketDetailsStage where show = genericShow
instance eqPreviousMetroTicketDetailsStage :: Eq PreviousMetroTicketDetailsStage where eq = genericEq 

data MetroTicketDetailsScreenStage = MetroTicketDetailsStage 
                                   | MetroMapStage 
                                   | MetroRouteDetailsStage
                                   | MetroSoftCancelStatusStage
                                   | MetroHardCancelStatusStage
                                   | MetroBookingCancelledStage

derive instance genericMetroTicketDetailsScreenStage :: Generic MetroTicketDetailsScreenStage _                                  
instance showMetroTicketDetailsScreenStage :: Show MetroTicketDetailsScreenStage where show = genericShow
instance eqMetroTicketDetailsScreenStage :: Eq MetroTicketDetailsScreenStage where eq = genericEq 


-- ######################################### MetroMyTicket ####################################################
type MetroMyTicketsScreenState = {
    data :: MetroMyTicketsScreenData
  , props :: MetroMyTicketsScreenProps
}

type MetroMyTicketsScreenData = {
  activeTickets :: Array MetroTicketCardData
, pastTickets :: Array MetroTicketCardData
}

type MetroMyTicketsScreenProps = {
  dummyProps :: String
, showShimmer :: Boolean
, entryPoint :: MetroMyTicketsEntry
}

type MetroTicketCardData = {
  sourceName :: String
  , destinationName :: String
  , createdAt :: String
  , noOfTickets :: Int
  , metroTicketStatusApiResp :: MetroTicketBookingStatus
  , status :: String
  , validUntill :: String
}

data MetroMyTicketsEntry = HomeScreenToMetroMyTickets | MetroTicketBookingToMetroMyTickets


-- ######################################### TicketBookingStatus #################################################### 

type TicketStatusScreenState =
  { data :: TicketStatusScreenData,
    props :: TicketStatusScreenProps
  }

type TicketStatusScreenData = {
  servicesAvailing :: Array TicketServiceI, -- TODO:: Use this for generic handling
  dateOfVisit :: String,
  keyValArray :: Array KeyVal,
  transactionId :: String,
  bookedForArray :: Array String,
  totalAmount :: Int,
  placeInfo :: Maybe TicketPlaceResp,
  servicesInfo :: Array TicketServiceData,
  shortOrderId :: String,
  selectedPlaceType :: PlaceType
}

type TicketStatusScreenProps = {
  currentStage :: TicketBookingScreenStage,
  previousStage :: TicketBookingScreenStage,
  termsAndConditionsSelected :: Boolean,
  validDate :: Boolean,
  showShimmer :: Boolean,
  paymentStatus :: PP.PaymentStatus,
  ticketBookingList :: TicketBookings,
  selectedBookingId :: String,
  selectedBookingInfo :: IndividualBookingItem,
  activeListItem :: TicketBookingServiceDetails,
  activeIndex :: Int,
  rightButtonDisable :: Boolean,
  leftButtonDisable :: Boolean,
  navigateToHome :: Boolean,
  selectedOperationalDay :: String,
  actionType :: TicketStatusEntry
}


data TicketStatusEntry = MetroTicketToPaymentStatusEntry
                       | ZooTicketToPaymentStatusEntry
derive instance genericTicketStatusEntry :: Generic TicketStatusEntry _ 
instance showTicketStatusEntry :: Show TicketStatusEntry where show = genericShow
instance eqTicketStatusEntry :: Eq TicketStatusEntry where eq = genericEq

--- ######################################### Search Location Screen State ####################################################


type SearchLocationScreenState = 
  { data :: SearchLocationScreenData ,
    props :: SearchLocationScreenProps,
    appConfig :: AppConfig
  }

type SearchLocationScreenData = 
  {
    srcLoc :: Maybe LocationInfo,
    destLoc :: Maybe LocationInfo,
    route :: Maybe Route,
    currentLoc :: LocationInfo,
    locationList :: Array LocationListItemState,
    fromScreen :: String,
    saveFavouriteCard :: SaveFavouriteCardState,
    latLonOnMap :: LocationInfo,
    selectedQuote :: Maybe QuotesList,
    defaultGate :: String,
    nearByGates :: Array Location,
    specialZoneCoordinates :: String,
    confirmLocCategory :: ZoneType,
    metroStations :: Array Station,
    updatedMetroStations :: Array Station,
    predictionSelectedFromHome :: LocationListItemState,
    quotesList :: Array QuotesList,
    rideDetails :: RideDetails
  }

type RideDetails = {
  searchId :: String ,
  rideDistance :: Int ,
  rideDuration :: Int ,
  rideScheduledDate :: String,
  rideScheduledTime :: String,
  rideScheduledTimeUTC :: String
}

type Station = {
  stationName :: String,
  stationCode :: String
}

type SearchLocationScreenProps = 
  { searchLocStage :: SearchLocationStage
  , focussedTextField :: Maybe SearchLocationTextField
  , actionType :: SearchLocationActionType
  , showSaveFavCard :: Boolean
  , areBothLocMandatory :: Boolean
  , canSelectFromFav :: Boolean
  , showLoader :: Boolean
  , canClearText :: Boolean 
  , locUnserviceable :: Boolean
  , isSpecialZone :: Boolean
  , isAutoComplete :: Boolean
  , pickUpSelectedOnMap :: Boolean
  , showRateCard :: Boolean 
  , tipViewProps :: TipViewProps 
  , customerTip :: CustomerTipProps
  , fareProductType :: FareProductType
  , currentEstimateHeight :: Int
  , selectedEstimateHeight :: Int }

data SearchLocationActionType = AddingStopAction 
                              | SearchLocationAction
                              | MetroStationSelectionAction

derive instance genericSearchLocationActionType :: Generic SearchLocationActionType _
instance eqSearchLocationActionType :: Eq SearchLocationActionType where eq = genericEq

data SearchLocationTextField =  SearchLocPickup
                              | SearchLocDrop

derive instance genericSearchLocationTextField :: Generic SearchLocationTextField _
instance showSearchLocationTextField :: Show SearchLocationTextField where show = genericShow
instance eqSearchLocationTextField :: Eq SearchLocationTextField where eq = genericEq

data SearchLocationStage =  ConfirmLocationStage 
                          | PredictionsStage 
                          | LocateOnMapStage
                          | AllFavouritesStage
                          | PredictionSelectedFromHome
                          | ChooseYourRide

derive instance genericSearchLocationStage :: Generic SearchLocationStage _
instance eqSearchLocationStage :: Eq SearchLocationStage where eq = genericEq

type GlobalProps = 
  { savedLocations :: Array LocationListItemState
  , recentSearches :: Array LocationListItemState
  , cachedSearches :: Array LocationListItemState
  }

type LocationInfo = 
  { lat :: Maybe Number ,
    lon :: Maybe Number ,
    placeId :: Maybe String ,
    address :: String ,
    addressComponents :: Address ,
    metroInfo :: Maybe Station,
    stationCode :: String,
    city :: City
  }
  
-- ############################################## NammaSafetyScreenState #############################


data SafetySetupStage =  SetNightTimeSafetyAlert
                        | SetDefaultEmergencyContacts
                        | SetPersonalSafetySettings

derive instance genericSafetySetupStage :: Generic SafetySetupStage _
instance eqSafetySetupStage :: Eq SafetySetupStage where eq = genericEq
instance showSafetySetupStage :: Show SafetySetupStage where show = genericShow

type NammaSafetyScreenState = {
  data :: NammaSafetyScreenData,
  props :: NammaSafetyScreenProps
}

type NammaSafetyScreenData =  {
  shareToEmergencyContacts :: Boolean,
  nightSafetyChecks :: Boolean,
  hasCompletedMockSafetyDrill :: Boolean,
  shareTripWithEmergencyContactOption :: RideShareOptions,
  shareOptionCurrent :: RideShareOptions,
  hasCompletedSafetySetup :: Boolean,
  emergencyContactsList :: Array NewContacts,
  sosId :: String,
  rideId :: String,
  videoPath :: String,
  updateActionType :: String,
  removedContactDetail :: NewContacts,
  currentLocation :: String,
  vehicleDetails :: String,
  videoList :: Array RC.SafetyVideoConfig,
  sosType :: Maybe SosFlow,
  config :: AppConfig,
  lastRideDetails :: Maybe IndividualRideCardState
 }

type NammaSafetyScreenProps =  {
  setupStage :: SafetySetupStage,
  recordingState :: RecordingState,
  confirmPopup :: Boolean,
  timerId :: String,
  timerValue :: Int,
  enableLocalPoliceSupport :: Boolean,
  showInfoPopUp :: Boolean,
  localPoliceNumber :: String,
  showShimmer :: Boolean,
  showTestDrill :: Boolean,
  triggeringSos :: Boolean,
  confirmTestDrill :: Boolean,
  educationViewIndex :: Maybe Int,
  showCallPolice :: Boolean,
  shouldCallAutomatically :: Boolean,
  fromDeepLink :: Boolean,
  showRideShareOptionsPopup :: Boolean,
  showVideoView :: Boolean,
  isSafetyCenterDisabled :: Boolean,
  fromBannerLink :: Boolean,
  showPastRidePopUp :: Boolean,
  checkPastRide :: Boolean,
  reportPastRide :: Boolean,
  appName :: String,
  isOffUs :: Boolean
}
data RecordingState = RECORDING | NOT_RECORDING | SHARING | UPLOADING | SHARED

derive instance genericRecordingState :: Generic RecordingState _
instance eqRecordingState :: Eq RecordingState where eq = genericEq
instance showRecordingState :: Show RecordingState where show = genericShow

data FollowRideScreenStage = PersonList | FollowingRide | ChatWithEM | MockFollowRide | RideCompletedStage

derive instance genericFollowRideScreenStage :: Generic FollowRideScreenStage _
instance showFollowRideScreenStage :: Show FollowRideScreenStage where show = genericShow
instance eqFollowRideScreenStage :: Eq FollowRideScreenStage where eq = genericEq

type FollowRideScreenState = {
  data :: FollowRideScreenData,
  props :: FollowRideScreenProps
}

type FollowRideScreenData = {
  driverInfoCardState :: Maybe DriverInfoCard
, currentStage :: FollowRideScreenStage
, currentFollower :: Maybe Followers
, followers :: Array Followers
, zoneType :: SpecialTags
, route :: Maybe Route
, speed :: Int
, config :: AppConfig
, messages :: Array ChatComponentConfig
, messagesSize :: String
, chatSuggestionsList :: Array String
, lastMessage :: ChatComponentConfig
, lastSentMessage :: ChatComponent
, lastReceivedMessage :: ChatComponent
, logField :: Object Foreign
, messageToBeSent:: String
, sosStatus :: Maybe CTA.SosStatus
, emergencyAudioStatus :: EmAudioPlayStatus
, counter :: Int
}

type FollowRideScreenProps = {
  city :: City
, showChatNotification :: Boolean
, canSendSuggestion :: Boolean
, isChatNotificationDismissed :: Boolean
, unReadMessages :: Boolean
, removeNotification :: Boolean
, enableChatWidget :: Boolean
, chatCallbackInitiated :: Boolean
, openChatScreen:: Boolean
, sendMessageActive :: Boolean
, sheetState :: Maybe BottomSheetState
, currentSheetState :: BottomSheetState
, isNotificationExpanded :: Boolean
, startMapAnimation :: Boolean
, isRideStarted :: Boolean
, isMock :: Boolean
, currentUserOnRide :: Boolean
}


data EmAudioPlayStatus = STOPPED | STARTED | COMPLETED | RESTARTED

derive instance genericEmAudioPlayStatus :: Generic EmAudioPlayStatus _
instance eqEmAudioPlayStatus :: Eq EmAudioPlayStatus where eq = genericEq

type ReferralStatusProp = {
  referralStatus :: ReferralStatus,
  referralCode :: Maybe String,
  showAddReferralPopup :: Boolean
}

data ReferralStatus = NO_REFERRAL | REFERRAL_APPLIED | REFERRAL_INVALID | REFERRAL_ALREADY_APPLIED

derive instance genericReferralStatus :: Generic ReferralStatus _
instance eqReferralStatus :: Eq ReferralStatus where eq = genericEq

data ReferralType = GIVE_REFERRAL | GET_REFERRED

derive instance genericReferralType :: Generic ReferralType _
instance eqReferralType :: Eq ReferralType where eq = genericEq

type MetroStation = {
  code :: String
  , name :: String
  , lat :: Maybe Number
  , lon :: Maybe Number
  , address :: Maybe String
  , stationType :: Maybe String
  , color :: Maybe String
  , sequenceNum :: Maybe Int
}

type MetroStations = {
  city :: City,
  stations :: Array GetMetroStationResp,
  lastUpdatedAt :: String
}

-- ######################################### MetroTicketBookingScreenState ####################################################

type MetroTicketBookingScreenState = {
  data :: MetroTicketBookingScreenData,
  props :: MetroTicketBookingScreenProps, 
  config :: AppConfig
}

type MetroTicketBookingScreenData = {
  ticketType :: TicketType
  , ticketCount :: Int
  , srcLoc :: String
  , destLoc :: String
  , srcCode :: String
  , destCode :: String
  , searchId :: String
  , ticketPrice :: Int
  , bookingId :: String
  , quoteId :: String
  , quoteResp :: Array MetroQuote
}

type MetroTicketBookingScreenProps = {
  isLimitExceeded :: Boolean
, termsAndConditionsSelected :: Boolean
, currentStage :: MetroTicketBookingStage
, isButtonActive :: Boolean
, showMetroBookingTimeError :: Boolean
}

data MetroTicketBookingStage = MetroTicketSelection | GetMetroQuote | ConfirmMetroQuote | PaymentSDKPooling

derive instance genericMetroTicketBookingStage :: Generic MetroTicketBookingStage _
instance eqMetroTicketBookingStage :: Eq MetroTicketBookingStage where eq = genericEq
instance showMetroTicketBookingStage :: Show MetroTicketBookingStage where show = genericShow

data TicketType = ONE_WAY_TRIP | ROUND_TRIP

derive instance genericTicketType :: Generic TicketType _
instance eqTicketType :: Eq TicketType where eq = genericEq

data LocationActionId = Src | Dest

derive instance genericLocationActionId :: Generic LocationActionId _
instance eqLocationActionId :: Eq LocationActionId where eq = genericEq
instance showLocationActionId :: Show LocationActionId where show = genericShow


-- ######################################### MetroTicketStatusScreenState ####################################################
type MetroTicketStatusScreenState = {
  data :: MetroTicketStatusScreenData,
  props :: MetroTicketStatusScreenProps
}

type MetroTicketStatusScreenData = {
  shortOrderId :: String,
  keyValArray :: Array KeyVal,
  validUntil :: String,
  bookingId :: String,
  resp :: MetroTicketBookingStatus,
  timerId :: String,
  quoteId :: String
}

type MetroTicketStatusScreenProps = {
  showShimmer :: Boolean
, paymentStatus :: PP.PaymentStatus
, entryPoint :: MetroTicketStatusScreenEntry
}

data MetroTicketStatusScreenEntry = HomescreenToMetroTicketStatus | MyMetroTicketsToMetroTicketStatus

type RideSearchProps = {
    sessionId :: String
  , sourceManuallyMoved :: Boolean
  , destManuallyMoved :: Boolean
  , autoCompleteType :: Maybe AutoCompleteReqType
  , sourceSelectType :: LocationSelectType
  , cachedPredictions :: DHM.HashMap String (Array LocationListItemState)
}

data AutoCompleteReqType = PICKUP | DROP
derive instance genericAutoCompleteReqType :: Generic AutoCompleteReqType _
instance showAutoCompleteReqType :: Show AutoCompleteReqType where show = genericShow

data LocationType = Source | Destination 
derive instance genericLocationType :: Generic LocationType _
instance eqLocationType :: Eq LocationType where eq = genericEq

type GlobalFlowCache = {
  savedLocations :: Maybe SavedLocationsListRes
}

type LocateOnMapProps = {
    sourceLocationName :: Maybe String
  , sourceGeoJson :: Maybe String
  , sourceGates :: Maybe (Array Location)
  , isSpecialPickUpGate :: Boolean
  , cameraAnimatedToSource :: Boolean
}

data NavigationMode = WALK | DRIVE

derive instance genericNavigationMode :: Generic NavigationMode _
instance showNavigationMode :: Show NavigationMode where show = genericShow

type HotSpotProps = {
    selectedSpot :: Maybe Location
  , centroidPoint :: Maybe CTA.Paths
}

type HotSpotData = {
    lat :: Number
  , lon :: Number
}

data LocationSelectType = SEARCH | MAP | FAVOURITE | REPEAT_RIDE | RETRY_SEARCH | SUGGESTION

derive instance genericLocationSelectType :: Generic LocationSelectType _
instance eqLocationSelectType :: Eq LocationSelectType where eq = genericEq

type ReferralComponentState =
  { stage :: ReferralStage
  , referralCode :: Maybe String
  , applyButtonActive :: Boolean
  , showReferredUserInfoPopup :: Boolean
  , showReferralProgramInfoPopup :: Boolean
  , isInvalidCode :: Boolean
  }

data ReferralStage = ENTER_REFERRAL_CODE
                   | INVALID_POPUP
                   | APPLIED_POPUP
                   | ALREADY_APPLIED_POPUP
                   | NO_REFERRAL_STAGE

derive instance genericReferralStage :: Generic ReferralStage _
instance eqReferralStage :: Eq ReferralStage where eq = genericEq

type SpecialLocationMap = DM.Map String SpecialLocationList

type SpecialLocationList = {
    geoJson :: String
  , gates :: Array Location
  , locationName :: String
  , category :: String
  , city :: String
}

type SpecialZoneTagConfig = {
    icon :: String
  , text :: String
  , infoPopUpConfig :: Maybe SpecialZoneInfoPopUp
  , backgroundColor :: String
}

type SpecialZoneInfoPopUp = {
    title :: String
  , primaryText :: String
  , secondaryText :: String
  , primaryButtonText :: String
  , icon :: String
}

-- ######################################### RentalScreenState ####################################################

type RentalScreenState = {
  data :: RentalScreenData,
  props :: RentalScreenProps
}

type RentalScreenData = {
    rentalBookingData :: CTA.RentalBookingConfig
  , startTimeUTC :: String
  , currentStage :: RentalScreenStage
  , rentalsQuoteList :: Array QuotesList
  , selectedQuote :: Maybe QuotesList
  , endOTP :: Maybe String
  , nextStop :: Maybe String
  , selectedDateTimeConfig :: DateTimeConfig
  , pickUpLoc :: LocationInfo 
  , dropLoc :: Maybe LocationInfo
  , searchId :: String
  , bookingId :: String
  , config :: AppConfig
}

type QuotesList = {
  quoteDetails :: ChooseVehicle.Config,
  index :: Int,
  activeIndex :: Int,
  fareDetails :: FareDetails
}


type FareDetails = {
  plannedPerKmRate ::  Int,
  baseFare :: Int,
  includedKmPerHr :: Int,
  perExtraKmRate :: Int,
  perExtraMinRate :: Int,
  perHourCharge :: Int,
  nightShiftCharge :: Int,
  tollCharges :: Maybe Number,
  deadKmFare :: Maybe DeadKmFare
}

data RentalScreenStage = RENTAL_SELECT_PACKAGE | RENTAL_SELECT_VARIANT | RENTAL_CONFIRMATION

derive instance genericRentalScreenStage :: Generic RentalScreenStage _
instance showRentalScreenStage :: Show RentalScreenStage where show = genericShow
instance eqRentalScreenStage :: Eq RentalScreenStage where eq = genericEq

type RentalScreenProps = {
    minDuration :: Int
  , maxDuration :: Int
  , minDistance :: Int
  , maxDistance :: Int
  , farePerKm :: String
  , maxDateBooking :: Int
  , showRateCard :: Boolean
  , showShimmer :: Boolean
  , showPrimaryButton :: Boolean
  , showPopUpModal :: Boolean
  , showRentalPolicy :: Boolean
}

type DateTimeConfig = {
    year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
}

----------------------------------------------------------------------

data FareProductType = RENTAL | INTER_CITY | ONE_WAY | ONE_WAY_SPECIAL_ZONE | DRIVER_OFFER

derive instance genericFareProductType :: Generic FareProductType _
instance showFareProductType :: Show FareProductType where show = genericShow
instance eqFareProductType :: Eq FareProductType where eq = genericEq

data CancelSearchType = NORMAL_RIDE_CANCEL | RENTAL_SEARCH_CANCEL

derive instance genericCancelSearchType :: Generic CancelSearchType _
instance showCancelSearchType :: Show CancelSearchType where show = genericShow
instance eqCancelSearchType :: Eq CancelSearchType where eq = genericEq

data VehicleViewType = LEFT_VIEW | RIGHT_VIEW

derive instance genericVehicleViewType :: Generic VehicleViewType _
instance showVehicleViewType :: Show VehicleViewType where show = genericShow
instance eqVehicleViewType :: Eq VehicleViewType where eq = genericEq