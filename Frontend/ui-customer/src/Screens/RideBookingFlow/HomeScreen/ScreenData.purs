{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.ScreenData where

import Common.Types.App (RateCardType(..), RentalBookingConfig, CustomerIssueTypes(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Components.ChooseVehicle.Controller as CV
import Data.Maybe (Maybe(..))
import Screens.Types (Contact, DriverInfoCard, HomeScreenState, LocationListItemState, PopupType(..), RatingCard(..), SearchLocationModelType(..), Stage(..), Address, EmergencyHelpModelState, ZoneType(..), SpecialTags, TipViewStage(..), SearchResultType(..), Trip(..), City(..), SheetState(..), BottomNavBarIcon(..), ReferralStatus(..), LocationSelectType(..), ReferralStage(..), BookingTime, InvalidBookingPopUpConfig, RideCompletedData(..))
import Services.API (DriverOfferAPIEntity(..), QuoteAPIDetails(..), QuoteAPIEntity(..), PlaceName(..), LatLong(..), SpecialLocation(..), QuoteAPIContents(..), RideBookingRes(..), RideBookingAPIDetails(..), RideBookingDetails(..), FareRange(..), FareBreakupAPIEntity(..))
import Prelude (($) ,negate)
import Data.Array (head)
import Prelude(negate)
import Foreign.Object (empty)
import ConfigProvider
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import PrestoDOM (BottomSheetState(..), Margin(..))
import Data.Map as Map 
import JBridge (Location)
import Data.HashMap as DHM
import Common.Types.App as CT
import MerchantConfig.DefaultConfig as MRC
import Screens.Types (FareProductType(..)) as FPT
import Screens.Types as ST

initData :: HomeScreenState
initData = let
  config = getAppConfig appConfig
  in
  {
    data: {
      suggestedAmount : 0
    , finalAmount : 0
    , startedAt : ""
    , endedAt : ""
    , source : ""
    , destination : ""
    , eta : "2 mins"
    , vehicleDetails : "Bajaj RE Auto"
    , registrationNumber : "KA  01  YF  4921"
    , rating : 4.0
    , locationList : []
    , savedLocations : []
    , fareProductType : FPT.ONE_WAY
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
       currentRateCardType : DefaultRateCard,
       onFirstPage:false,
       baseFare : 0,
       extraFare : [],
       serviceTierName : Nothing,
       createdTime : "",
       driverAdditions : [],
       fareInfoDescription : [],
       isNightShift : false,
       nightChargeTill : "",
       nightChargeFrom : "",
       waitingTimeInfo : { freeMinutes: "", charge: "" }
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
    , newEstimatedDistance : Nothing
    , newEstimatedFare : Nothing
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
    , selectedEstimatesObject : CV.config {
        showInfo = true
      , searchResultType = CV.ESTIMATES
      , layoutMargin = Margin 0 0 0 0
    }
    , quoteList : []
    , selectedQuoteId : Nothing
    , selectedQuoteVariant : Nothing
    , lastMessage : { message : "", sentBy : "", timeStamp : "", type : "", delay : 0 }
    , cancelRideConfirmationData : { delayInSeconds : 5, timerID : "", enableTimer : true, continueEnabled : false }
    , ratingViewState : {
        selectedYes : Nothing,
        selectedRating : -1,
        issueReportActiveIndex : Nothing ,
        issueReasonCode : Nothing,
        openReportIssue : false,
        doneButtonVisibility : false,
        issueReason : Nothing,
        issueDescription : "",
        rideBookingRes : dummyRideBooking,
        wasOfferedAssistance : Nothing
    }
    , config : config
    , currentCityConfig : MRC.defaultCityConfig
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
    , contactList : Nothing
    , followers : Nothing
    , vehicleVariant : ""
    , hotSpotInfo : []
    , iopState : {
        timerId : "",
        timerVal : "",
        showMultiProvider : false,
        providerPrefVisible : false,
        providerSelectionStage : false,
        showPrefButton : false,
        providerPrefInfo : false,
        hasTopProviderEstimate : true
    }
    , otherSelectedEstimates : []
    , rateCardCache : Nothing
    , rentalsInfo : Nothing 
    , startTimeUTC : ""
    , invalidBookingId : Nothing
    , maxEstimatedDuration : 0
    , invalidBookingPopUpConfig : Nothing
    , rideCompletedData : initialRideCompletedData
    , routeCacheForAdvancedBooking : Nothing
    , previousRideDrop : false
    , famousDestinations : []
    },
    props: {
      rideRequestFlow : false
    , scheduledRidePollingDelay : 0.0
    , startScheduledRidePolling : false
    , maxDateBooking : 5
    , isHomescreenExpanded : false
    , canScheduleRide : false
    , isSearchLocation : NoView
    , bookingUpdateRequestId : Nothing
    , showConfirmEditDestPopUp : false
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
      , tipForDriver: 0
      , tipActiveIndex: -1
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
    , distance : 0
    , isSrcServiceable : true
    , isDestServiceable : true
    , isRideServiceable : true
    , showlocUnserviceablePopUp : false
    , autoSelecting : true
    , searchExpire : 90
    , isEstimateChanged : false
    , showRateCard : false
    , showRevisedFareDetails : false
    , showRateCardIcon : false
    , sendMessageActive : false
    , chatcallbackInitiated : false
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
      , customerTipArray : []
      , customerTipArrayWithValues : []
      , activeIndex : -1
      , primaryButtonText : ""
      }
    , focussedBottomIcon : MOBILITY
    , timerId : ""
    , findingRidesAgain : false
    , routeEndPoints : Nothing
    , findingQuotesProgress : 0.0
    , confirmLocationCategory : NOZONE
    , canSendSuggestion : true
    , sheetState : Nothing
    , currentSheetState : COLLAPSED
    , showDisabilityPopUp : false
    , isChatNotificationDismissed : false
    , searchLocationModelProps : dummySearchLocationModelProps
    , flowWithoutOffers : true
    , showEducationalCarousel : false
    , currentLocation : dummyLocation
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
    , showBookingPreference : false
    , isChatWithEMEnabled: false
    , referral : {
        referralStatus : NO_REFERRAL,
        referralCode : Nothing,
        showAddReferralPopup : false
      }
    , safetyAlertType : Nothing
    , rideSearchProps : {
            sessionId : ""
          , sourceManuallyMoved : false
          , destManuallyMoved : false
          , autoCompleteType : Nothing
          , sourceSelectType : SEARCH
          , cachedPredictions : DHM.empty
        }
    , selectedEstimateHeight : 0
    , isSafetyCenterDisabled : false
    , suggestedRideFlow : false
    , locateOnMapProps : { sourceLocationName : Nothing, sourceGeoJson : Nothing, sourceGates : Nothing, isSpecialPickUpGate : false, cameraAnimatedToSource : true }
    , showSpecialZoneInfoPopup : false
    , hotSpot : { selectedSpot : Nothing, centroidPoint : Nothing }
    , repeatRideVariant : ""
    , hasToll : false
    , repeatRideServiceTierName : Nothing
    , isSearchCancelled : false
    , referralComponentProps : { stage : NO_REFERRAL_STAGE
                               , referralCode : Nothing
                               , applyButtonActive : false
                               , showReferredUserInfoPopup : false
                               , showReferralProgramInfoPopup : false 
                               , isInvalidCode : false 
                               }
    , showAcWorkingPopup : false
    , repeateRideTimerStoped : false
    , currentEstimateHeight : 184
    , showEndOTP : false
    , rideDurationTimer : ""
    , rideDurationTimerId : ""
    , stopLoc : Nothing
    , showRentalInfo : false
    , showIntercityUnserviceablePopUp : false
    , showNormalRideNotSchedulablePopUp : false
    , zoneOtpExpired : false
    , isBannerDataComputed : false
    , showScheduledRideExistsPopUp : false
    , isOffline : false
    , hasEstimateBackpoint : false
    , shimmerViewTimer : config.homeScreen.shimmerTimer
    , shimmerViewTimerId : ""
    , isKeyBoardOpen : false
    , isContactSupportPopUp : false
  }
}

dummySearchLocationModelProps = {
    isAutoComplete : false
  , showLoader : false
  , crossBtnSrcVisibility : false
  , crossBtnDestVisibility : false
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
  , status : ""
  , serviceTierName : Nothing
  , vehicleModel : ""
  , vehicleColor : ""
  , providerName : ""
  , providerType : CT.ONUS
  , rentalData : dummyRentalBookingConfig
  , fareProductType : FPT.ONE_WAY
  , driversPreviousRideDropLocLat : Nothing
  , driversPreviousRideDropLocLon : Nothing
  , spLocationName : Nothing
  , addressWard : Nothing
  , currentSearchResultType : ST.ESTIMATES
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
        , distanceToPickup: Nothing
        , durationToPickup: Nothing
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
    "category" : "",
     "locationName" : "",
     "gatesInfo" : [],
     "geoJson" : Nothing
 }

dummyLocation :: Location
dummyLocation = {
   place : "",
   lat : 0.0,
   lng : 0.0,
   address : Nothing,
   city : Nothing,
   isSpecialPickUp : Nothing
 }


dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : Nothing,
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
  serviceTierName : Nothing, 
  airConditioned : Nothing,
  isValueAddNP : Nothing,
  providerName : Nothing,
  estimatedDistance : Nothing,
  estimatedDuration : Nothing,
  rideScheduledTime : Nothing,
  vehicleServiceTierType : Nothing,
  tollConfidence : Nothing,
  driversPreviousRideDropLocLat : Nothing,
  driversPreviousRideDropLocLon : Nothing,
  specialLocationName : Nothing
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
    isSpecialZone: true,
    vehicleVariant: Nothing,
    serviceTierNameV2 : Nothing
}

dummyRentalBookingConfig :: RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : ""
  , rideEndedAt : ""
  , extraDistanceFare : ""
  , extraTimeFare : ""
  }

dummyBookingTime :: BookingTime
dummyBookingTime = {
  bookingId : "0",
  rideStartTime : "",
  estimatedDuration : 0
}

dummyInvalidBookingPopUpConfig :: InvalidBookingPopUpConfig
dummyInvalidBookingPopUpConfig = {
  fromLocation : "",
  toLocation : "",
  bookingId : "",
  rideScheduledTime : "",
  maxEstimatedDuration : 0,
  fareProductType : FPT.ONE_WAY
}

initialRideCompletedData :: RideCompletedData
initialRideCompletedData = {
  issueReportData : {
    bannerItem : Nothing 
  , currentBannerIndex : 0
  , currentPageIndex : 0
  , showIssueBanners : true
  , hasAccessibilityIssue : false
  , hasTollIssue : false
  , hasSafetyIssue : false
  , customerResponse : [
    {
      issueType : TollCharge
    , selectedYes : Nothing
    }
  , {
      issueType : NightSafety
    , selectedYes : Nothing
    }
  , {
      issueType : Accessibility
    , selectedYes : Nothing
    }
  , {
      issueType : NoIssue
    , selectedYes : Nothing
    }
  ]
  , respondedValidIssues : false
  }
, toll : {
    confidence : Nothing
  , showAmbiguousPopUp : false
  }
}

