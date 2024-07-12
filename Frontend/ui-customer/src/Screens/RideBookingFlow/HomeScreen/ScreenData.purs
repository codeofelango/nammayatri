{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Common.Types.App (RateCardType(..), RideType(..), RentalBookingConfig, SearchResultType(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Components.ChooseVehicle.Controller (config) as CV
import Data.Maybe (Maybe(..))
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address, EmergencyHelpModelState,Location, ZoneType(..), SpecialTags, TipViewStage(..), Trip(..), City(..), SheetState(..), BottomNavBarIcon(..))
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..), SpecialLocation(..), QuoteAPIContents(..), RideBookingRes(..), RideBookingAPIDetails(..), RideBookingDetails(..), FareRange(..), FareBreakupAPIEntity(..))
import Prelude (($) ,negate)
import Data.Array (head)
import Prelude(negate)
import Foreign.Object (empty)
import ConfigProvider
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import PrestoDOM (BottomSheetState(..), Margin(..))
import Data.Map as Map 

initData :: HomeScreenState
initData = {
    data: {
      suggestedAmount : 0
    , finalAmount : 0
    , startedAt : ""
    , currentSearchResultType : ESTIMATES
    , endedAt : ""
    , source : ""
    , destination : ""
    , eta : "2 mins"
    , vehicleDetails : "Bajaj RE Auto"
    , registrationNumber : "KA  01  YF  4921"
    , rating : 4.0
    , locationList : []
    , savedLocations : []
    , recentSearchs : { predictionArray : []}
    , destinationSuggestions : []
    , tripSuggestions: []
    , suggestionsData : { suggestionsMap: Map.empty }
    , previousCurrentLocations : {pastCurrentLocations:[]}
    , selectList : []
    , quoteListModelState : []
    , driverInfoCardState : dummyDriverInfo
    , rideRatingState : dummyPreviousRiderating
    , settingSideBar : dummySettingBar
    , sourceAddress : dummyAddress
    , destinationAddress : dummyAddress
    , route : Nothing
    , startedAtUTC : ""
    , selectedDateTimeConfig : {
        year : 0
      , month : 0
      , day : 0
      , hour : 0
      , minute : 0
    }
    , rateCard : {
       additionalFare : 0,
       nightShiftMultiplier : 0.0,
       nightCharges : false,
       currentRateCardType : DefaultRateCard,
       onFirstPage:false,
       baseFare : 0,
       extraFare : 0,
       pickUpCharges : 0,
       vehicleVariant : ""
       }
    , speed : 0
    , selectedLocationListItem : Nothing
    , saveFavouriteCard : {
        address : ""
      , tag : ""
      , tagExists : false
      , selectedItem : locationListStateObj
      , isBtnActive : false
      }
    , rideDistance : "--"
    , rideDuration : "--"
    , showPreferences : false
    , messages : []
    , messagesSize : "-1"
    , chatSuggestionsList : []
    , messageToBeSent : ""
    , nearByPickUpPoints : []
    , polygonCoordinates : ""
    , specialZoneQuoteList : []
    , specialZoneSelectedQuote : Nothing
    , specialZoneSelectedVariant : Nothing
    , quoteList : []
    , selectedQuoteId : Nothing
    , selectedQuoteVariant : Nothing
    , intercity : false
    , selectedEstimatesObject : CV.config
    , lastMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , cancelRideConfirmationData : { delayInSeconds : 5, timerID : "", enableTimer : true, continueEnabled : false }
    , pickUpCharges : 0
    , ratingViewState : {
        selectedYesNoButton : -1,
        selectedRating : -1,
        issueReportActiveIndex : Nothing ,
        issueReasonCode : Nothing,
        openReportIssue : false,
        doneButtonVisibility : false,
        issueFacedView : false,
        issueReason : Nothing,
        issueDescription : "",
        rideBookingRes : dummyRideBooking,
        wasOfferedAssistance : Nothing
    }
    , config : getAppConfig appConfig
    , logField : empty
    , nearByDrivers : Nothing
    , disability : Nothing
    , searchLocationModelData : dummySearchLocationModelData
    , waitTimeInfo : false
    , lastSentMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , lastReceivedMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , triggerPatchCounter : 0
    , infoCardPeekHeight : 0
    , peekHeight : 0
    , rideHistoryTrip : Nothing
    , bannerData : {
        bannerItem : Nothing
      , currentBanner : 0
      , bannerScrollState: "0"
      , currentPage : 0
    } 
    , contactList : []
    , followers : Nothing
    , rentalsInfo : Nothing 
    , rideType : NORMAL_RIDE
    , startTimeUTC : ""
    , vehicleVariant : ""
    },
    props: {
      rideRequestFlow : false
    , doUpdatePickupLocation : false
    , maxDateBooking : 5
    , nightSafetyFlow : false
    , isHomescreenExpanded : false
    , canScheduleRide : false
    , isSearchLocation : NoView
    , currentStage : HomeScreen
    , stageBeforeChatScreen : RideAccepted
    , showCallPopUp : false
    , sourceLat : 0.0
    , isSource : Nothing
    , sourceLong : 0.0
    , destinationLat : 0.0
    , destinationLong : 0.0
    , sourcePlaceId : Nothing
    , destinationPlaceId : Nothing
    , estimateId : ""
    , selectedQuote : Nothing
    , locationRequestCount : 0
    , zoneTimerExpired : false
    , customerTip : {
        enableTips: false
      , tipForDriver: 10
      , tipActiveIndex: 1
      , isTipSelected: false
      }
    , searchId : ""
    , bookingId : ""
    , expiredQuotes : []
    , isCancelRide : false
    , cancellationReasons : []
    , cancelRideActiveIndex : Nothing
    , cancelDescription : ""
    , cancelReasonCode : ""
    , isPopUp : NoPopUp
    , forFirst : true
    , callbackInitiated : false
    , isLocationTracking : false
    , isInApp : true
    , locateOnMap : false
    , sourceSelectedOnMap : false
    , distance : 0
    , isSrcServiceable : true
    , isDestServiceable : true
    , isRideServiceable : true
    , showlocUnserviceablePopUp : false
    , autoSelecting : true
    , searchExpire : 90
    , isEstimateChanged : false
    , showRateCard : false
    , showRateCardIcon : false
    , sendMessageActive : false
    , chatcallbackInitiated : false
    , emergencyHelpModal : false
    , estimatedDistance : Nothing
    , waitingTimeTimerIds : []
    , tagType : Nothing
    , isSaveFavourite : false
    , showShareAppPopUp : false
    , showMultipleRideInfo : false
    , hasTakenRide : true
    , isReferred : false
    , storeCurrentLocs : false
    , unReadMessages : false
    , openChatScreen : false
    , emergencyHelpModelState : emergencyHelpModalData
    , showLiveDashboard : false
    , isBanner : true
    , callSupportPopUp : false
    , isMockLocation: false
    , isSpecialZone : false
    , defaultPickUpPoint : ""
    , showChatNotification : false
    , cancelSearchCallDriver : false
    , zoneType : dummyZoneType
    , cancelRideConfirmationPopup : false
    , searchAfterEstimate : false
    , tipViewProps : {
        stage : DEFAULT
      , isVisible : false
      , onlyPrimaryText : false
      , isprimaryButtonVisible : false
      , primaryText : ""
      , secondaryText : ""
      , customerTipArray : ["₹10 🙂", "₹20 😄", "₹30 🤩"]
      , customerTipArrayWithValues : [10, 20, 30]
      , activeIndex : -1
      , primaryButtonText : ""
      }
    , focussedBottomIcon : MOBILITY
    , timerId : ""
    , findingRidesAgain : false
    , routeEndPoints : Nothing
    , findingQuotesProgress : 0.0
    , confirmLocationCategory : ""
    , canSendSuggestion : true
    , sheetState : Nothing
    , currentSheetState : COLLAPSED
    , showOfferedAssistancePopUp : false
    , showDisabilityPopUp : false
    , isChatNotificationDismissed : false
    , searchLocationModelProps : dummySearchLocationModelProps
    , flowWithoutOffers : true
    , showEducationalCarousel : false
    -- , specialZoneType : ""
    , currentLocation : {
        lat : 0.0,
        lng : 0.0,
        place : "",
        address : Nothing,
        city : Nothing
      }
    , isShorterTrip : false
    , locateOnMapLocation : {
          source : ""
        , sourceLat : 0.0
        , sourceLng : 0.0
        , sourceAddress : dummyAddress
        , destination : ""
        , destinationLat : 0.0
        , destinationLng : 0.0
        , destinationAddress : dummyAddress
      }
    , isNotificationExpanded : false
    , bottomSheetState : STATE_COLLAPSED
    , removeNotification : true
    , city : AnyCity
    , destCity : Nothing
    , isRepeatRide : false
    , currSlideIndex : 0.0
    , suggestionsListExpanded : false
    , repeatRideTimer : ""
    , repeatRideTimerId : ""
    , reAllocation : {
        showPopUp : false
      }
    , showShimmer : true
    , homeScreenSheetState : COLLAPSED
    , autoScrollTimer : ""
    , autoScrollTimerId : ""
    , autoScroll : true
    , enableChatWidget : false
    , sosBannerType : Nothing
    , showShareRide : false
    , followsRide: false
    , showEndOTP : false
    , rideDurationTimer : ""
    , rideDurationTimerId : ""
    , stopLoc : Nothing
    , showRentalInfo : false
    , showIntercityUnserviceablePopUp : false
    , showNormalRideNotSchedulablePopUp : false
    , zoneOtpExpired : false
    , isContactSupportPopUp : false
    }
}

dummySearchLocationModelProps = {
    isAutoComplete : false
  , showLoader : false
  , crossBtnSrcVisibility : false
  , crossBtnDestVisibility : false
  , findPlaceIllustration : true
}

dummySearchLocationModelData = {
  prevLocation : ""
}

dummyZoneType = {
    sourceTag : NOZONE
  , destinationTag : NOZONE
  , priorityTag : NOZONE
}

dummyContactData :: Array Contact
dummyContactData = []

selectedContactData ::  Contact
selectedContactData =
  { name : "", phoneNo : "" }

emergencyHelpModalData :: EmergencyHelpModelState
emergencyHelpModalData = {
  showCallPolicePopUp : false,
  showCallContactPopUp : false,
  showCallSuccessfulPopUp : false,
  showContactSupportPopUp : false,
  emergencyContactData : dummyContactData,
  currentlySelectedContact : selectedContactData,
  sosId : "",
  sosStatus : "",
  isSelectEmergencyContact : false,
  waitingDialerCallback : false
}


dummyPreviousRiderating :: RatingCard
dummyPreviousRiderating = {
  rideId : ""
, rating : 0
, driverName : ""
, finalAmount : 0
, rideStartTime : ""
, rideEndTime : ""
, source : ""
, destination : ""
, rideStartDate : ""
, vehicleNumber : ""
, status : ""
, shortRideId : ""
, bookingId : ""
, rideEndTimeUTC : ""
, dateDDMMYY : ""
, offeredFare : 0
, distanceDifference : 0
, feedback : ""
, feedbackList : []
}


dummyDriverInfo :: DriverInfoCard
dummyDriverInfo =
  { otp : ""
  , driverName : ""
  , eta : Nothing
  , vehicleDetails : ""
  , currentSearchResultType : ESTIMATES
  , registrationNumber : ""
  , rating : 0.0
  , startedAt : ""
  , endedAt : ""
  , source : ""
  , destination : ""
  , rideId : ""
  , price : 0
  , sourceLat : 0.0
  , sourceLng : 0.0
  , destinationLat : 0.0
  , destinationLng : 0.0
  , driverLat : 0.0
  , driverLng : 0.0
  , distance : 0
  , waitingTime : "--"
  , driverArrived : false
  , estimatedDistance : ""
  , driverArrivalTime : 0
  , bppRideId : ""
  , driverNumber : Nothing
  , merchantExoPhone : ""
  , createdAt : ""
  , initDistance : Nothing
  , config : getAppConfig appConfig
  , vehicleVariant : ""
  , sourceAddress : dummyAddress
  , destinationAddress : dummyAddress
  , rentalData : dummyRentalBookingConfig
  }

dummySettingBar :: SettingSideBarState
dummySettingBar = {
    name : ""
  , number : ""
  , opened : CLOSED
  , email : Nothing
  , gender : Nothing
  , appConfig : getAppConfig appConfig
  , sideBarList : ["MyRides", "Tickets", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "ShareApp", "LiveStatsDashboard", "About", "Logout"]
  , hasCompletedSafetySetup : false
}

dummyAddress :: Address
dummyAddress = 
  { "area"      : Nothing
  , "state"     : Nothing
  , "country"   : Nothing
  , "building"  : Nothing
  , "door"      : Nothing
  , "street"    : Nothing
  , "city"      : Nothing
  , "areaCode"  : Nothing
  , "ward"      : Nothing
  , "placeId"   : Nothing
  }

dummyDriverOfferAPIEntity :: QuoteAPIContents
dummyDriverOfferAPIEntity =
  DRIVER_OFFER
    $ DriverOfferAPIEntity
        { rating: Nothing
        , validTill: ""
        , driverName: ""
        , distanceToPickup: 0.0
        , durationToPickup: 0
        }

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : [],
  "placeId" : Nothing
}

specialLocation :: SpecialLocation
specialLocation = SpecialLocation{
    "category" :"",
     "gates": [],
     "locationName" : ""
 }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing
 }


dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : "",
  status : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  duration : Nothing,
  fareBreakup :[],
  createdAt : "",
  discount : Nothing ,
  estimatedTotalFare : 0,
  agencyName : "",
  rideList :[] ,
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  hasNightIssue : Just true,
  updatedAt : "",
  bookingDetails : dummyRideBookingAPIDetails ,
  fromLocation :  dummyBookingDetails,
  merchantExoPhone : "",
  specialLocationTag : Nothing,
  hasDisability : Nothing,
  sosStatus: Nothing,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  rideScheduledTime : Nothing
  }

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails = RideBookingDetails {
  toLocation : Nothing,
  estimatedDistance : Nothing,
  otpCode : Nothing,
  stopLocation : Nothing
}

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{
  amount : 0,
  description : "fare"
}

dummyTrip :: Trip
dummyTrip = {
    sourceLat: 0.0,
    source: "",
    destination: "",
    sourceAddress: dummyAddress,
    destinationAddress: dummyAddress,
    sourceLong: 0.0,
    destLat: 0.0,
    destLong: 0.0,
    frequencyCount: Nothing,  
    recencyDate: Nothing,  
    locationScore: Nothing,  
    isSpecialZone: true
}

dummyRentalBookingConfig :: RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , estimatedFare : 0
  , finalFare : 0
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : ""
  , rideEndedAt : ""
  }