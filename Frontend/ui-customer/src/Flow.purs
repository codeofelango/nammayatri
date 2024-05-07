{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Accessor
import Engineering.Helpers.LogEvent
import Screens.TicketBookingFlow.TicketBooking.Transformer
import Services.API
import Common.Resources.Constants (zoomLevel)
import Common.Types.App (GlobalPayload(..), SignatureAuthData(..), Payload(..), Version(..), LocationData(..), EventPayload(..), ClevertapEventParams, OTPChannel(..), LazyCheck(..), FCMBundleUpdate)
import Common.Types.App as Common
import Components.ChatView.Controller (makeChatComponent')
import Components.LocationListItem.Controller (locationListStateObj, dummyAddress)
import Components.SavedLocationCard.Controller (getCardType)
import Components.SettingSideBar.Controller as SettingSideBarController
import Constants as Constants
import Control.Monad.Except (runExcept)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Control.Transformers.Back.Trans as App
import Data.Array (catMaybes, reverse, filter, length, null, snoc, (!!), any, sortBy, head, uncons, last, concat, all, elemIndex, mapWithIndex, elem, nubByEq)
import Data.Array as Arr
import Data.Either (Either(..), either)
import Data.Function.Uncurried (runFn3, runFn2, runFn1)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.Int as INT
import Data.Ord (compare)
import Data.String (Pattern(..), Replacement(..), drop, indexOf, split, toLower, trim, take, joinWith)
import Data.String (length) as STR
import Data.String as DS
import Data.String.Common (joinWith, split, toUpper, trim, replaceAll)
import Debug (spy)
import Effect (Effect)
import Effect.Random as ER
import Effect.Aff (Milliseconds(..), makeAff, nonCanceler, launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn2, runEffectFn9)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, getExpiryTime, convertUTCtoISC, getCurrentUTC, getWindowVariable, flowRunner, resetIdMap, markPerformance)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Events as Events
import Engineering.Helpers.Utils (loaderText, toggleLoader, saveObject, reboot, showSplash, fetchLanguage, handleUpdatedTerms, getReferralCode)
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Encode)
import Foreign.Class (class Encode, encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import JBridge (getCurrentLatLong, addMarker, cleverTapSetLocation, currentPosition, drawRoute, emitJOSEvent, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, firebaseUserID, generateSessionId, getLocationPermissionStatus, getVersionCode, getVersionName, hideKeyboardOnNavigation, hideLoader, initiateLocationServiceClient, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, launchInAppRatingPopup, locateOnMap, locateOnMapConfig, metaLogEvent, openNavigation, reallocateMapFragment, removeAllPolylines, removeMarker, saveSuggestionDefs, saveSuggestions, setCleverTapUserData, setCleverTapUserProp, stopChatListenerService, toast, toggleBtnLoader, updateRoute, updateMarker, extractReferrerUrl, getLocationNameV2, getLatLonFromAddress, showDialer, cleverTapCustomEventWithParams, cleverTapCustomEvent, showKeyboard, differenceBetweenTwoUTCInMinutes, shareTextMessage, defaultMarkerConfig, Location, setMapPadding)
import JBridge as JB
import Helpers.Utils (convertUTCToISTAnd12HourFormat, decodeError, addToPrevCurrLoc, addToRecentSearches, adjustViewWithKeyboard, checkPrediction, differenceOfLocationLists, drawPolygon, filterRecentSearches, fetchImage, FetchImageFrom(..), getCurrentDate, getNextDateV2, getCurrentLocationMarker, getCurrentLocationsObjFromLocal, getDistanceBwCordinates, getGlobalPayload, getMobileNumber, getNewTrackingId, getObjFromLocal, getPrediction, getRecentSearches, getScreenFromStage, getSearchType, parseFloat, parseNewContacts, removeLabelFromMarker, requestKeyboardShow, saveCurrentLocations, seperateByWhiteSpaces, setText, showCarouselScreen, sortPredictionByDistance, toStringJSON, triggerRideStatusEvent, withinTimeRange, fetchDefaultPickupPoint, updateLocListWithDistance, getCityCodeFromCity, getCityNameFromCode, getDistInfo, getExistingTags, getMetroStationsObjFromLocal, updateLocListWithDistance, getCityConfig, getMockFollowerName, getCityFromString, getMetroConfigFromAppConfig)
import Language.Strings (getString)
import Language.Types (STR(..)) as STR
import Log (logInfo, logStatus)
import MerchantConfig.Types (AppConfig(..), MetroConfig(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import MerchantConfig.Utils as MU
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, identity, otherwise, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>), (<<<), ($>), (>>=), (*))
import Mobility.Prelude (capitalize)
import ModifyScreenState (modifyScreenState, updateRepeatRideDetails, FlowState(..))
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, otherwise, identity, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>), (<<<), ($>), (>>=), (*))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField)
import Helpers.Pooling(delay)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard, ticketPlaceId)
import Screens (getScreen)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation, getLocTag, savedLocTransformer) as AddNewAddress
import Screens.AddNewAddressScreen.ScreenData (dummyLocation) as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HelpAndSupportScreen.Transformer (reportIssueMessageTransformer)
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime, isTipEnabled, findingQuotesSearchExpired, tipEnabledState)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.FollowRideScreen.ScreenData as FollowRideScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.HomeScreen.Transformer (getLocationList, getDriverInfo, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList, getTripFromRideHistory, getFormattedContacts)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.Types (TicketBookingScreenStage(..), CardType(..), AddNewAddressScreenState(..), SearchResultType(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), ErrorType(..), ZoneType(..), TipViewData(..),TripDetailsGoBackType(..), DisabilityT(..), UpdatePopupType(..) , PermissionScreenStage(..), TicketBookingItem(..), TicketBookings(..), TicketBookingScreenData(..),TicketInfoScreenData(..),IndividualBookingItem(..), SuggestionsMap(..), Suggestions(..), Address(..), LocationDetails(..), City(..), TipViewStage(..), Trip(..), SearchLocationTextField(..), SearchLocationScreenState, SearchLocationActionType(..), SearchLocationStage(..), LocationInfo, BottomNavBarIcon(..), FollowRideScreenStage(..), ReferralStatus(..), LocationType(..), Station(..),MetroTicketBookingStage(..), MetroStations(..))
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (ScreenOutput(..)) as RideScheduledScreenOutput
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationConfig, getTipViewData, setTipViewData)
import Screens.RideSelectionScreen.Controller (getTitle)
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.SearchLocationScreen.Controller as SearchLocationController
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Screens.TicketBookingFlow.MetroTicketDetails.ScreenData as MetroTicketDetailsScreenData
import Screens.TicketBookingFlow.PlaceDetails.Controller as PlaceDetailsC
import Screens.TicketBookingFlow.PlaceDetails.View as PlaceDetailsS
import Screens.TicketBookingFlow.PlaceList.Controller as PlaceListC
import Screens.TicketBookingFlow.PlaceList.ScreenData as PlaceListData
import Screens.TicketBookingFlow.PlaceList.View as PlaceListS
import Screens.TicketBookingFlow.TicketBooking.ScreenData as TicketBookingScreenData
import Screens.TicketInfoScreen.ScreenData as TicketInfoScreenData
import Screens.Types (Gender(..)) as Gender
import Screens.Types as ST
import Screens.Types
import Services.Backend as Remote
import Services.Config (getBaseUrl, getSupportNumber)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Effect.Aff (Milliseconds(..), makeAff, nonCanceler, launchAff)
import Types.App
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), TICKET_BOOKING_SCREEN_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), APP_UPDATE_POPUP(..), TICKET_BOOKING_SCREEN_OUTPUT(..),TICKET_INFO_SCREEN_OUTPUT(..),defaultGlobalState,  TICKETING_SCREEN_SCREEN_OUTPUT(..),METRO_TICKET_SCREEN_OUTPUT(..),METRO_TICKET_DETAILS_SCREEN_OUTPUT(..),METRO_MY_TICKETS_SCREEN_OUTPUT(..),METRO_MY_TICKETS_SCREEN_OUTPUT(..),METRO_TICKET_STATUS_SCREEN_OUTPUT(..))
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)
import Constants.Configs
import PrestoDOM (initUI)
import Common.Resources.Constants (zoomLevel)
import PaymentPage
import Screens.TicketBookingFlow.TicketBooking.Transformer
import Screens.Types as ST
import Domain.Payments as PP
import PrestoDOM.Core (terminateUI)
import Helpers.Storage.Flow.BaseApp
import Helpers.Storage.Flow.SearchStatus
import Helpers.Logs -- TODO :: Move helpers import into a single file and reexport
import Helpers.Auth
import Helpers.Version
import Helpers.Ride
import Helpers.Firebase
import Foreign.Class (class Encode)
import SuggestionUtils
import ConfigProvider
import Components.ChatView.Controller (makeChatComponent')
import Components.MessagingView (ChatComponent)
import Mobility.Prelude (capitalize)
import Screens.HelpAndSupportScreen.Transformer (reportIssueMessageTransformer)
import Timers
import Screens.TicketBookingFlow.PlaceList.View as PlaceListS
import Screens.TicketBookingFlow.PlaceDetails.View as PlaceDetailsS
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Control.Transformers.Back.Trans as App
import Locale.Utils
import Screens.RentalBookingFlow.RideScheduledScreen.Controller (ScreenOutput(..)) as RideScheduledScreenOutput 
import Screens.SearchLocationScreen.Controller as SearchLocationController
import Screens.SearchLocationScreen.ScreenData as SearchLocationScreenData
import Screens (ScreenName(..), getScreen) as Screen
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Screens.NammaSafetyFlow.SafetySettingsScreen.Controller as SafetySettingsScreen
import Screens.NammaSafetyFlow.SetupSafetySettingsScreen.Controller as SetupSafetySettingsScreen
import Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller as ActivateSafetyScreen
import Screens.NammaSafetyFlow.SosActiveScreen.Controller as SosActiveScreen
import Screens.NammaSafetyFlow.SafetyEducationScreen.Controller as SafetyEducationScreen
import Screens.NammaSafetyFlow.Components.SafetyUtils
import RemoteConfig as RC
import Engineering.Helpers.RippleCircles (clearMap)
import Data.Array (groupBy, fromFoldable, singleton)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Types.App
import Screens.TicketBookingFlow.TicketStatus.ScreenData as TicketStatusScreenData
import Screens.Types
import Screens.TicketBookingFlow.TicketStatus.Transformer as TicketStatusTransformer
import Screens.TicketBookingFlow.MetroTicketStatus.Transformer
import Screens.TicketBookingFlow.MetroTicketDetails.Transformer
import Screens.TicketBookingFlow.MetroMyTickets.Transformer
import Screens.TicketBookingFlow.MetroTicketBooking.ScreenData as MetroTicketBookingScreenData
import Screens.NammaSafetyFlow.ScreenData (defaultTimerValue)
import Services.Config(getNumbersToWhiteList)
import SessionCache(getValueFromWindow, setValueInWindow)
import LocalStorage.Cache (clearCache)
import DecodeUtil (getAnyFromWindow, stringifyJSON, setInWindow)
import Data.Foldable (foldMap)
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.FollowRideScreen.Controller (deleteDismisedMockDrills)
import Data.Map as Map
import Foreign.Object (lookup)
import Screens.RideSelectionScreen.Transformer (myRideListTransformer)
import Services.FlowCache as FlowCache
import Data.HashMap as DHM
import Services.CacAPIType (GetUiConfigResp (..))
import ConfigProvider (getAppConfigCAC)
import Helpers.API as HelpersAPI
import Helpers.Referral (applyReferralCode)
import Helpers.SpecialZoneAndHotSpots

baseAppFlow :: GlobalPayload -> Boolean-> FlowBT String Unit
baseAppFlow gPayload callInitUI = do
  liftFlowBT $ markPerformance "BASE_APP_FLOW"
  baseAppStorage -- TODO:: Restructure the files and names
  baseAppLogs
  liftFlowBT $ runEffectFn1 resetIdMap ""
  liftFlowBT $ resetAllTimers
  let showSplashScreen = fromMaybe false $ gPayload ^. _payload ^. _show_splash
  when callInitUI $ lift $ lift $ initUI -- TODO:: Can we move this to Main
  when showSplashScreen $ toggleSplash true
  tokenValidity <- validateToken signatureAuthData
  lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  
  when (getValueToLocalStore CAC_TOSS == "__failed") $ do
      toss <- lift $ lift $ liftFlow $ ER.randomInt 1 100
      void $ pure $ setValueToLocalStore CAC_TOSS (show toss)
  if tokenValidity 
    then handleDeepLinks (Just gPayload) false
    else validateAuthData $ signatureAuthData
  
  where
    signatureAuthData = 
      gPayload ^._payload ^._signatureAuthData
    validateAuthData signatureAuthData = 
      case signatureAuthData of
        Just signatureAuth -> do
          response <- lift $ lift $ Remote.triggerSignatureBasedOTP signatureAuth
          validationStatus <- validateSignaturePayload signatureAuth response
          when validationStatus $ handleDeepLinks (Just gPayload) false
        Nothing -> 
          if showCarouselScreen FunctionCall
            then welcomeScreenFlow
            else enterMobileNumberScreenFlow

handleDeepLinks :: Maybe GlobalPayload -> Boolean -> FlowBT String Unit
handleDeepLinks mBGlobalPayload skipDefaultCase = do
  liftFlowBT $ markPerformance "HANDLE_DEEP_LINKS"
  case mBGlobalPayload of 
    Just globalPayload ->
      case globalPayload ^. _payload ^._view_param of
        Just screen -> case screen of
          "rides" -> hideSplashAndCallFlow myRidesScreenFlow 
          "abt" -> hideSplashAndCallFlow aboutUsScreenFlow
          "fvrts" -> hideSplashAndCallFlow savedLocationFlow
          "help" -> hideSplashAndCallFlow $ flowRouter HelpAndSupportScreenFlow
          "prof" -> hideSplashAndCallFlow myProfileScreenFlow
          "lang" -> hideSplashAndCallFlow selectLanguageScreenFlow
          "tkts" -> hideSplashAndCallFlow placeListFlow
          "safety" -> hideSplashAndCallFlow safetySettingsFlow
          "smd" -> do
            modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen{props{confirmTestDrill = true}})
            hideSplashAndCallFlow activateSafetyScreenFlow
          "sedu" -> do
            case globalPayload ^. _payload ^._deepLinkJSON of
              Just (Common.QueryParam queryParam) -> do
                if isJust queryParam.option then do
                  let videoList = RC.safetyBannerVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
                  modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = videoList}, props{showVideoView = true, educationViewIndex = Just 0, fromBannerLink = true}})
                else 
                  modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen{props{fromDeepLink = true}})
              _ -> pure unit
            hideSplashAndCallFlow safetyEducationFlow
          "mt" -> hideSplashAndCallFlow metroTicketBookingFlow
          _ -> if skipDefaultCase then pure unit else currentFlowStatus
        Nothing -> currentFlowStatus
    Nothing -> do
      let mBPayload = getGlobalPayload Constants.globalPayload
      case mBPayload of
        Just _ -> handleDeepLinks mBPayload skipDefaultCase
        Nothing -> pure unit

hideSplashAndCallFlow :: FlowBT String Unit -> FlowBT String Unit
hideSplashAndCallFlow flow = do 
  hideLoaderFlow
  flow

hideLoaderFlow :: FlowBT String Unit
hideLoaderFlow = do
  toggleSplash false
  void $ lift $ lift $ toggleLoader false
  liftFlowBT $ hideLoader

toggleSplash :: Boolean -> FlowBT String Unit
toggleSplash = 
  if _ then UI.splashScreen
  else do
    state <- getState
    void $ liftFlowBT $ launchAff $ flowRunner state $ runExceptT $ runBackT $ do
      void $ lift $ lift $ delay $ Milliseconds 2000.0
      liftFlowBT $ terminateUI $ Just "SplashScreen"

currentFlowStatus :: FlowBT String Unit
currentFlowStatus = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ markPerformance "CURRENT_FLOW_STATUS"
  void $ lift $ lift $ toggleLoader false
  liftFlowBT $ markPerformance "VERIFY_PROFILE_CALL_API"
  verifyProfile "LazyCheck"
  liftFlowBT $ markPerformance "FLOW_STATUS_CALL_API"
  flowStatus <- Remote.flowStatusBT "LazyCheck"
  liftFlowBT $ markPerformance "RIDE_LIST_CALL_API"
  case flowStatus ^. _currentStatus of
    WAITING_FOR_DRIVER_OFFERS currentStatus -> goToFindingQuotesStage currentStatus (flowStatus ^. _isValueAddNP) false
    DRIVER_OFFERED_QUOTE currentStatus      -> goToFindingQuotesStage currentStatus (flowStatus ^. _isValueAddNP)true
    WAITING_FOR_DRIVER_ASSIGNMENT currentStatus -> goToConfirmRide currentStatus.bookingId currentStatus.fareProductType
    RIDE_ASSIGNED _                         -> checkRideStatus true
    PENDING_RATING _                        -> do
                                                firstRideCompletedEvent ""
                                                checkRideStatus false 
    _                                       -> checkRideStatus false
  liftFlowBT $ markPerformance "HIDE_LOADER_FLOW"
  hideLoaderFlow
  void $ pure $ hideKeyboardOnNavigation true -- TODO:: Why is this added here @ashkriti?  
  homeScreenFlow
  where
    verifyProfile :: String -> FlowBT String Unit
    verifyProfile dummy = do
      let toss = fromMaybe 49 $ INT.fromString $  getValueToLocalStore CAC_TOSS
      liftFlowBT $ markPerformance "VERIFY_PROFILE"
      response <- Remote.getProfileBT toss
      config <- getAppConfigFlowBT appConfig
      let appName = fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just 
      case appName of
        "Namma Yatri" -> logFirstCabRideEvent (fromMaybe false $ response ^. _hasTakenValidCabRide) "ny_cab_firstride"
        "Mana Yatri" -> logFirstCabRideEvent (fromMaybe false $ response ^. _hasTakenValidCabRide) "my_cab_firstride"
        _ -> pure unit
      updateVersion (response ^. _clientVersion) (response ^. _bundleVersion)
      updateFirebaseToken (response ^. _maskedDeviceToken) getUpdateToken
      updateLanguageAndReferralCode $ response ^. _language
      updateFlowStatusStorage response
      updateCTEventData response
      setValueToLocalStore CUSTOMER_REFERRAL_CODE (fromMaybe "" (response ^. _customerReferralCode))
      if isNothing (response ^. _firstName) 
        then do
          void $ updateLocalStage HomeScreen
          hideLoaderFlow
          accountSetUpScreenFlow
          handleDeepLinks Nothing true
        else do
          tag <- maybe (pure "") pure (response ^. _disability)
          let hasCompletedSafetySetup = fromMaybe false $ response ^. _hasCompletedSafetySetup
              hasCompletedMockSafetyDrill = fromMaybe false $ response ^. _hasCompletedMockSafetyDrill
              sosBannerType = case hasCompletedSafetySetup, hasCompletedMockSafetyDrill of
                false, _ -> Just ST.SETUP_BANNER
                true, false -> Just ST.MOCK_DRILL_BANNER
                _, _ -> Nothing
          modifyScreenState $ HomeScreenStateType
                  $ \homeScreen →
                      homeScreen
                        { data
                          { disability = Just { tag: tag, id: "", description: "" }
                          , followers = Nothing
                          , settingSideBar
                            { name = fromMaybe "" (response ^. _firstName)
                            , gender = response ^. _gender
                            , email = response ^. _email
                            , hasCompletedSafetySetup = hasCompletedSafetySetup
                            }
                          }
                        , props { isBanner = false
                          , sosBannerType = sosBannerType
                          , followsRide = fromMaybe false (response ^. _followsRide)
                          , isSafetyCenterDisabled = fromMaybe false (response ^. _isSafetyCenterDisabled)
                        }
                        }          
                        
    getUpdateToken :: String -> FlowBT String Unit --TODO:: Move this to common library
    getUpdateToken token =
      let
        UpdateProfileReq initialData = Remote.mkUpdateProfileRequest FunctionCall
        requiredData = initialData { deviceToken = Just token }
      in
        void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
    
    logFirstCabRideEvent :: Boolean -> String -> FlowBT String Unit
    logFirstCabRideEvent toLogEvent event = 
      if toLogEvent
        then do
          logField_ <- lift $ lift $ getLogFields
          void $ liftFlowBT $ logEvent logField_ $ event
          void $ pure $ metaLogEvent $ event
        else pure unit

    updateLanguageAndReferralCode :: Maybe String -> FlowBT String Unit
    updateLanguageAndReferralCode language = do
      config <- getAppConfigFlowBT appConfig
      let referralStatus = getValueToLocalStore REFERRAL_STATUS
      let referralCode =  if config.feature.enableReferral && config.feature.enableAutoReferral then do
                            case referralStatus of
                              "NOT_REFERRED_NOT_TAKEN_RIDE" -> getReferralCode (getValueToLocalStore REFERRER_URL)
                              _                             -> Nothing
                          else Nothing

      if isNothing language || (getKeyByLanguage (fromMaybe "ENGLISH" language) /= (getLanguageLocale languageKey)) || isJust referralCode then do
        case referralCode of
          Just code | referralStatus == "NOT_REFERRED_NOT_TAKEN_RIDE" -> do
            referralAppliedStatus <- applyReferralCode code
            case referralAppliedStatus of
              REFERRAL_APPLIED -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{ referral{ referralStatus = REFERRAL_APPLIED }, isReferred = true } })
              REFERRAL_INVALID -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ referral{ referralStatus = REFERRAL_INVALID } } })
              _ -> pure unit
          _ -> do
            let UpdateProfileReq updateProfileConfig = Remote.mkUpdateProfileRequest FunctionCall
            void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq updateProfileConfig)
      else pure unit

    goToFindingQuotesStage :: { validTill :: String , estimateId :: String} -> Maybe Boolean -> Boolean -> FlowBT String Unit
    goToFindingQuotesStage currentStatus mbIsValueAddNP driverOfferedQuote = do
      let estimateId = currentStatus.estimateId
          providerType = maybe Common.ONUS (\valueAdd -> if valueAdd then Common.ONUS else Common.OFFUS) mbIsValueAddNP -- This defines whether quote selected was ours or not after kill and relaunch
      removeChatService ""
      if any (_ == (getValueToLocalStore FINDING_QUOTES_START_TIME)) ["__failed", ""] then do
        updateFlowStatus SEARCH_CANCELLED
      else do
        let searchExpiryTime = getSearchExpiryTime "LazyCheck"
            secondsLeft = findingQuotesSearchExpired driverOfferedQuote
        if secondsLeft > 0 then do
          setValueToLocalStore RATING_SKIPPED "true"
          let stage = if isLocalStageOn ReAllocated then ReAllocated else FindingQuotes
          updateLocalStage stage
          setValueToLocalStore AUTO_SELECTING ""
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          (GlobalState currentState) <- getState
          let tipViewData = case (getTipViewData "LazyCheck") of
                              Just (TipViewData tipView) -> do
                                currentState.homeScreen.props.tipViewProps{stage = tipView.stage , activeIndex = tipView.activeIndex , isVisible = tipView.activeIndex >= 0 }
                              Nothing -> do
                                currentState.homeScreen.props.tipViewProps
          case (getFlowStatusData "LazyCheck") of
            Just (FlowStatusData flowStatusData) -> do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{
                props{ sourceLat = flowStatusData.source.lat
                     , sourceLong = flowStatusData.source.lng
                     , destinationLat = flowStatusData.destination.lat
                     , destinationLong = flowStatusData.destination.lng
                     , currentStage = stage
                     , searchExpire = secondsLeft
                     , estimateId = estimateId
                     , rideRequestFlow = true
                     , selectedQuote = Nothing
                     , tipViewProps = tipViewData
                     , city = getCityNameFromCode flowStatusData.source.city
                     , findingQuotesProgress = 1.0 - (INT.toNumber secondsLeft)/(INT.toNumber searchExpiryTime)
                     , locateOnMapProps{ sourceLocationName = flowStatusData.source.address, sourceGeoJson = flowStatusData.sourceGeoJson, sourceGates = flowStatusData.sourceGates } }
                , data { source = flowStatusData.source.place
                       , destination = flowStatusData.destination.place
                       , sourceAddress = flowStatusData.sourceAddress
                       , destinationAddress = flowStatusData.destinationAddress
                       , selectedEstimatesObject{
                          vehicleVariant = getValueToLocalStore SELECTED_VARIANT
                        , providerType = providerType
                        }
                    }
                })
            Nothing -> updateFlowStatus SEARCH_CANCELLED
        else updateFlowStatus SEARCH_CANCELLED

    goToConfirmRide :: String -> Maybe String -> FlowBT String Unit
    goToConfirmRide bookingId fareProductType = do
      if any (_ == fareProductType) [Just "ONE_WAY_SPECIAL_ZONE", Nothing] then
        checkRideStatus false
      else do
        updateLocalStage ConfirmingRide
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp}})
        homeScreenFlow

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  config <- getAppConfigFlowBT appConfig
  hideLoaderFlow -- Removed initial choose langauge screen
  if( any (_ == getLanguageLocale languageKey) ["__failed", "(null)"]) then void $ pure $ setLanguageLocale config.defaultLanguage else pure unit
  logField_ <- lift $ lift $ getLogFields
  if any (_ == getValueToLocalStore REFERRER_URL) ["__failed", "(null)"] then void $ pure $ extractReferrerUrl unit
  else pure unit
  void $ lift $ lift $ toggleLoader false
  void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_enter_mob_num_scn_view"
  setValueToLocalStore T_AND_C_VERSION (show config.termsVersion)
  flow <- UI.enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString STR.VERIFYING_OTP) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            let generatedID = "generated_" <> (generateSessionId unit)
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp generatedID) state.data.tokenId
            case resp of
              Right resp -> do
                    void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_verify_otp"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    if (customerId == "__failed") then do
                      void $ lift $ lift $ setLogField "customer_id" $ encode ("null")
                      pure unit
                      else do
                        void $ lift $ lift $ setLogField "customer_id" $ encode (customerId)
                        pure unit
                    setValueToLocalStore CUSTOMER_ID customerId
                    void $ liftFlowBT $ setCleverTapUserData "Identity" customerId
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    setValueToLocalStore USER_NAME $ (fromMaybe "" $ response.person ^. _firstName) <> " " <> (fromMaybe "" $ response.person ^. _middleName) <> " " <> (fromMaybe "" $ response.person ^. _lastName)
                    if isNothing (response.person ^. _firstName) then currentFlowStatus else handleDeepLinks Nothing false
              Left err -> do
                pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
                let errResp = err.response
                    codeMessage = decodeError errResp.errorMessage "errorCode"
                if ( err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    void $ pure $ toast (getString STR.OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{data{otp=""}, props{enterOTP = false, wrongOTP = false}})
                else if ( err.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                    let attemptsLeft = decodeError errResp.errorMessage "errorPayload"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false, attemptLeft = attemptsLeft}, data{otp=""}})
                else if ( err.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
                    pure $ toast (getString STR.TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false, wrongOTP = false}, data{otp=""}})
                else do
                    pure $ toast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false,wrongOTP = false}, data{otp=""}})
                enterMobileNumberScreenFlow
    GoToOTP state -> do
            when((getValueToLocalStore MOBILE_NUMBER) /= state.data.mobileNumber) $ do
              deleteValueFromLocalStore SUGGESTIONS_MAP
              deleteValueFromLocalStore RECENT_SEARCHES
            void $ pure $ setValueInWindow (show MOBILE_NUMBER) state.data.mobileNumber
            setValueToLocalStore COUNTRY_CODE (state.data.countryObj.countryCode)
            void $ liftFlowBT $ setCleverTapUserData "Phone" (state.data.countryObj.countryCode <> (getValueToLocalStore MOBILE_NUMBER))
            (TriggerOTPResp triggerOtpResp) <- Remote.triggerOTPBT (Remote.makeTriggerOTPReq state.data.mobileNumber state.data.countryObj.countryCode (show state.data.otpChannel))
            void $ pure $ toast (getString if state.data.otpChannel == SMS then STR.SENT_OTP_VIA_SMS else STR.SENT_OTP_VIA_WHATSAPP) 
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = triggerOtpResp.authId, attempts = triggerOtpResp.attempts}, props {enterOTP = true,resendEnable = false}})
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{number = state.data.mobileNumber}}})
            enterMobileNumberScreenFlow
    ResendOTP state -> do
            (ResendOTPResp resendResp) <-  Remote.resendOTPBT state.data.tokenId
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = resendResp.authId, attempts = resendResp.attempts}})
            enterMobileNumberScreenFlow
    GoBack state  ->  do
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data {timer = 30 }, props {enterOTP = false,resendEnable = false}})
            enterMobileNumberScreenFlow
    GoToWelcomeScreen state -> welcomeScreenFlow

welcomeScreenFlow :: FlowBT String Unit
welcomeScreenFlow = do
  hideLoaderFlow
  flow <- UI.welcomeScreen
  case flow of
    GoToMobileNumberScreen -> enterMobileNumberScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  disabilityListT <- updateDisabilityList "Account_Set_Up_Screen"
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{disabilityOptions{disabilityOptionList = disabilityListT }}})
  flow <- UI.accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let gender = getGenderValue state.data.gender
          selectedDisability = state.data.disabilityOptions.selectedDisability
          (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{firstName = (Just state.data.name),gender = gender, hasDisability = Just (isJust selectedDisability), disability = case selectedDisability of 
            Just disability -> Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.disabilityOptions.otherDisabilityReason))
            _ -> Nothing  }
      setValueToLocalStore DISABILITY_UPDATED "true"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{showDisabilityPopUp = (isJust selectedDisability)} , data{disability = selectedDisability}})
      case gender of
          Just value -> void $ liftFlowBT $ setCleverTapUserData "gender" value
          Nothing -> pure unit

      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          void $ liftFlowBT $ setCleverTapUserData "Name" (getValueToLocalStore USER_NAME)
          case gender of
            Just value -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just value}}, props{isBanner = false, referral{ showAddReferralPopup = false }}})
            Nothing    -> pure unit
          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_onboarded"
          void $ pure $ metaLogEvent "ny_user_onboarded"
          pure unit
        Left err -> do
          void $ pure $ toast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> state{props{btnActive = true},data{name=state.data.name}})
          accountSetUpScreenFlow
    GO_BACK -> do
      void $ pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      void $ pure $ deleteValueFromLocalStore MOBILE_NUMBER
      modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> AccountSetUpScreenData.initData)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen -> enterMobileNumberScreen{data{ otp = ""}})
      enterMobileNumberScreenFlow


updateDisabilityList :: String -> FlowBT String (Array DisabilityT)
updateDisabilityList screenType = do
  response <- Remote.disabilityList
  case response of 
    Right (GetDisabilityListResp resp) -> pure $ getDisabilityList resp
    Left err -> pure $ getDisabilityList []


cacFlow :: FlowBT String Unit
cacFlow  = do
  let toss = fromMaybe 49 $ INT.fromString $ getValueToLocalStore CAC_TOSS
  cfgResp' <- lift $ lift $ Remote.getUiConfigs toss
  case cfgResp' of 
    Right (cfgResp) ->
      case cfgResp of 
        EmptyGetUiConfigResp _ -> do
          _ <- pure $ spy "INFO " "Empty response from getUiConfigs driver so not updating city config."
          pure unit
        GetUiConfigResp cfg -> do
            _ <- pure $ spy "DEBUG : Response from driver getUiConfigs : " cfgResp
            setValueToLocalStore UI_CONFIGS (stringifyJSON cfg)
            _ <- pure $ runFn2 setInWindow "UI_CONFIGS" (stringifyJSON cfg)
            pure unit
    Left _ -> do
        _ <- pure $ spy "DEBUG " "Error in fetching driver info and hence unable to update city config." 
        pure unit

updateConfigState :: AppConfig -> FlowBT String Unit
updateConfigState config = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{config= config }})
  modifyScreenState $ EnterMobileNumberScreenType (\mobileNumberScreen -> mobileNumberScreen{data{config= config }})
  modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreen -> chooseLanguageScreen{data{config= config }})
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen{data{config= config }})
  modifyScreenState $ MyRideScreenStateType (\myRideScreen -> myRideScreen{data{config= config }})
  modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen{data{config= config }})
  modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen{data{config= config }})
  modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> selectLanguageScreen{data{config= config }})
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{config= config }})
  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{config= config }})
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreen -> myProfileScreen{data{config= config }})
  modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen{data{config= config }})
  modifyScreenState $ SavedLocationScreenStateType (\savedLocationScreen -> savedLocationScreen{data{config= config }})
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen{config= config })
  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen{appConfig= config })
  modifyScreenState $ AboutUsScreenStateType (\aboutUsScreen -> aboutUsScreen{appConfig= config })
  modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreen -> appUpdatePopUpScreen{config= config })
  modifyScreenState $ SearchLocationScreenStateType (\searchLocationScreen -> searchLocationScreen{appConfig= config })
  modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{data{config= config }})
  modifyScreenState $ FollowRideScreenStateType (\followRideScreen -> followRideScreen{data{config= config }})
  modifyScreenState $ MetroTicketBookingScreenStateType (\metroTicketBookingScreen -> metroTicketBookingScreen{config= config })


homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  void $ cacFlow
  let config = getAppConfigCAC appConfig
  void $ updateConfigState config
  
  liftFlowBT $ markPerformance "HOME_SCREEN_FLOW"
  logField_ <- lift $ lift $ getLogFields
  (GlobalState currentState) <- getState
  void $ checkAndUpdateSavedLocations currentState.homeScreen
  void $ pure $ cleverTapSetLocation unit
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})

  -- TODO: HANDLE LOCATION LIST INITIALLY
  void $ pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  let 
    _ = runFn2 EHC.updatePushInIdMap "bannerCarousel" true
    currentCityConfig = getCityConfig currentState.homeScreen.data.config.cityConfig $ getValueToLocalStore CUSTOMER_LOCATION
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{
    props{
      hasTakenRide = getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE"
    , isReferred = getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE"
    }
  , data {
      currentCityConfig = currentCityConfig
    }
  })
  liftFlowBT $ handleUpdatedTerms $ getString STR.TERMS_AND_CONDITIONS_UPDATED
  flow <- UI.homeScreen
  void $ lift $ lift $ fork $ Remote.pushSDKEvents
  case flow of
    ADD_STOP state -> do 
      let _ = spy "ADD_STOP" state
      modifyScreenState 
        $ SearchLocationScreenStateType (\slsState -> slsState{props{focussedTextField = Just SearchLocDrop}})
      searchLocationFlow
    CHECK_FLOW_STATUS -> currentFlowStatus
    GO_TO_MY_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen{data{offsetValue = 0}, props {fromNavBar = true}})
      myRidesScreenFlow
    GO_TO_HELP -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_help"
      flowRouter HelpAndSupportScreenFlow
    CHANGE_LANGUAGE ->  selectLanguageScreenFlow
    GO_TO_ABOUT -> aboutUsScreenFlow
    GO_TO_MY_TICKETS -> do
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      void $ pure $ spy "bookedRes" bookedRes
      void $ pure $ spy "pendingRes" pendingRes
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData{props{navigateToHome = true, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes)}})            
      modifyScreenState $ TicketingScreenStateType (\ticketingScreen -> ticketingScreen{ props { hideMyTickets = true }})
      ticketListFlow
    GO_TO_MY_PROFILE  updateProfile -> do
        void $ lift $ lift $ liftFlow $ logEvent logField_ (if updateProfile then "safety_banner_clicked" else "ny_user_profile_click")
        modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData{props{fromHomeScreen = updateProfile , updateProfile = updateProfile, changeAccessibility = true, isBtnEnabled = true , genderOptionExpanded = false , showOptions = false, expandEnabled = true }})
        myProfileScreenFlow
    GO_TO_FIND_ESTIMATES updatedState -> do
      findEstimates updatedState
    RETRY_FINDING_QUOTES showLoader -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handled Loader in IOS Side
      void $ lift $ lift $ toggleLoader showLoader
      (GlobalState newState) <- getState
      let state = newState.homeScreen
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_tip_search" $ [{key : "Tip amount (₹)", value : unsafeToForeign state.props.customerTip.tipForDriver}]
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_retry_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                      {key : "Estimate Fare (₹)", value : unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare)},
                                                                                                      {key : "Customer tip (₹)", value : unsafeToForeign state.props.customerTip.tipForDriver},
                                                                                                      {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                      {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges}]
      if (not (isLocalStageOn QuoteList)) then do
        void $ pure $ firebaseLogEvent "ny_user_cancel_and_retry_request_quotes"
        cancelEstimate state.props.estimateId
      else do
        void $ pure $ firebaseLogEvent "ny_user_retry_request_quotes"
      setValueToLocalStore AUTO_SELECTING "false"
      setValueToLocalStore FINDING_QUOTES_POLLING "false"
      setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
      when (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") do
        void $ pure $ firebaseLogEvent "ny_user_auto_confirm"

      let currentTime = (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")
          findingQuotesTime = convertUTCtoISC (getValueToLocalNativeStore FINDING_QUOTES_START_TIME) "HH:mm:ss"
      if withinTimeRange findingQuotesTime currentTime "22:00:00" || withinTimeRange findingQuotesTime currentTime "05:00:00" then do
        void $ pure $ toast (getString STR.PLEASE_FIND_REVISED_FARE_ESTIMATE)
        void $ pure $ firebaseLogEvent "ny_user_new_estimate_after_night_charges_applicable"
        updateLocalStage FindEstimateAndSearch
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = false } })
      else do
        void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
        response <- lift $ lift $ Remote.selectEstimate (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected && state.props.customerTip.tipForDriver > 0 then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
        case response of
          Right res -> do
            updateLocalStage FindingQuotes
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } })
          Left err -> do
            void $ pure $ firebaseLogEvent "ny_user_estimate_expired"
            updateLocalStage FindEstimateAndSearch
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = true } })
        let tipViewData = if state.props.customerTip.isTipSelected then state.props.tipViewProps{ stage = TIP_ADDED_TO_SEARCH } else HomeScreenData.initData.props.tipViewProps
        logInfo "retry_finding_quotes" ( "selectedEstimate Current Stage: " <> (show state.props.currentStage) <> " LOCAL_STAGE: " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id :" <> state.props.estimateId )
        void $ pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = 0.0 , scaleType="CENTER_CROP"}
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { customerTip = if homeScreen.props.customerTip.isTipSelected then homeScreen.props.customerTip else HomeScreenData.initData.props.customerTip{enableTips = homeScreen.props.customerTip.enableTips } , tipViewProps = tipViewData, findingQuotesProgress = 0.0 }})
      homeScreenFlow
    LOCATION_SELECTED item addToRecents-> do
        void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
        void $ lift $ lift $ toggleLoader true
        (GlobalState newState) <- getState
        updateCurrentLocation ""
        let state = newState.homeScreen
            searchWithoutPlaceName = any (_ == state.props.rideSearchProps.sourceSelectType) [ST.MAP, ST.FAVOURITE, ST.RETRY_SEARCH, ST.SUGGESTION] && state.props.isSource == Just true
        case searchWithoutPlaceName of
          true -> pure unit
          false -> 
            case state.props.isSource of
              Just true -> do
                (GetPlaceNameResp sourceDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.sourcePlaceId state.props.sourceLat state.props.sourceLong (if state.props.isSource == Just false then dummyLocationListItemState else item)
                let (PlaceName sourceDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (sourceDetailResp !! 0))
                    (LatLong sourceLocation) = sourceDetailResponse.location
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{sourceLat = sourceLocation.lat, sourceLong = sourceLocation.lon} })
              Just false  -> do
                (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) state.props.destinationPlaceId state.props.destinationLat state.props.destinationLong (if state.props.isSource == Just true then dummyLocationListItemState else item)
                let (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp!!0))
                    (LatLong destinationLocation) = (destinationDetailResponse.location)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon} })
              _          -> pure unit
        updateSourceLocation ""
        (GlobalState updatedState) <- getState
        let bothLocationChangedState = updatedState.homeScreen
        (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong)
        let srcServiceable = sourceServiceabilityResp.serviceable
        let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
        let pickUpPoints = if null srcSpecialLocation.gatesInfo then
                              filterHotSpots bothLocationChangedState sourceServiceabilityResp.hotSpotInfo bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong
                           else
                              mapSpecialZoneGates srcSpecialLocation.gatesInfo
        (ServiceabilityResDestination destServiceabilityResp) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong)
        let destServiceable = destServiceabilityResp.serviceable
        setValueToLocalStore CUSTOMER_LOCATION $ show (getCityNameFromCode sourceServiceabilityResp.city)
        let geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
            isHotSpot = null srcSpecialLocation.gatesInfo && not (null pickUpPoints)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> bothLocationChangedState{ data{ polygonCoordinates = geoJson
                                                                                              , nearByPickUpPoints=pickUpPoints}
                                                                                        , props { defaultPickUpPoint = ""
                                                                                                , city = getCityNameFromCode sourceServiceabilityResp.city 
                                                                                                , isSpecialZone =  (srcSpecialLocation.geoJson) /= Nothing
                                                                                                , confirmLocationCategory = if length pickUpPoints > 0 then (getZoneType srcSpecialLocation.category) else NOZONE
                                                                                                , findingQuotesProgress = 0.0
                                                                                                , locateOnMapProps{ sourceLocationName = Just srcSpecialLocation.locationName
                                                                                                                  , sourceGates = Just $ pickUpPoints }
                                                                                                , hotSpot{ centroidPoint = if isHotSpot then
                                                                                                                            Just { lat : bothLocationChangedState.props.sourceLat, lng : bothLocationChangedState.props.sourceLong }
                                                                                                                           else Nothing }
                                                                                        }})
        when (addToRecents) $ do
          addLocationToRecents item bothLocationChangedState sourceServiceabilityResp.serviceable destServiceabilityResp.serviceable
          fetchAndModifyLocationLists bothLocationChangedState.data.savedLocations
        (GlobalState globalState) <- getState
        let updateScreenState = globalState.homeScreen
            recentList = 
                updateLocListWithDistance 
                  updateScreenState.data.recentSearchs.predictionArray 
                  updateScreenState.props.sourceLat 
                  updateScreenState.props.sourceLong 
                  true 
                  state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
        if (not srcServiceable && (updateScreenState.props.sourceLat /= -0.1 && updateScreenState.props.sourceLong /= -0.1) && (updateScreenState.props.sourceLat /= 0.0 && updateScreenState.props.sourceLong /= 0.0)) then do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isSrcServiceable = false, isRideServiceable= false, isSource = Just true}, data {recentSearchs {predictionArray = recentList}}})
          homeScreenFlow
        else if ((not destServiceable) && (updateScreenState.props.destinationLat /= 0.0 && updateScreenState.props.destinationLat /= -0.1) && (updateScreenState.props.destinationLong /= 0.0 && bothLocationChangedState.props.destinationLong /= -0.1)) then do
          if (getValueToLocalStore LOCAL_STAGE == "HomeScreen") then do
            void $ pure $ toast (getString STR.LOCATION_UNSERVICEABLE)
            pure unit
            else pure unit
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isDestServiceable = false, isRideServiceable = false,isSource = Just false, isSrcServiceable = true}, data {recentSearchs {predictionArray = recentList}}})
          homeScreenFlow
        else 
          modifyScreenState $ 
            HomeScreenStateType 
              (\homeScreen -> 
                updateScreenState
                  { props
                      { isRideServiceable = true
                      , isSrcServiceable = true
                      , isDestServiceable = true
                      }
                  , data 
                      { recentSearchs 
                          { predictionArray = 
                              recentList
                          }
                      }
                  }
              )
        rideSearchFlow "NORMAL_FLOW"

    SEARCH_LOCATION input state -> do
      let 
        cityConfig = case state.props.isSource of
                        Just false -> let config = getCityConfig state.data.config.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
                                      in config{ geoCodeConfig{ strictBounds = true }}
                        _          -> defaultCityConfig
        event = case state.props.isSource of
                  Just true -> "ny_user_auto_complete_api_trigger_src"
                  Just false -> "ny_user_auto_complete_api_trigger_dst"
                  Nothing -> ""
      void $ lift $ lift $ liftFlow $ logEvent logField_ event
      case DHM.lookup (DS.toLower input) state.props.rideSearchProps.cachedPredictions of
        Just locationList' -> do
          logInfo "auto_complete_cached_predictions" input
          modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList = locationList'}, props{searchLocationModelProps{isAutoComplete = true, showLoader = false}}})
        Nothing -> do
          logInfo "auto_complete_search_predictions" input
          (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input state.props.sourceLat state.props.sourceLong (EHC.getMapsLanguageFormat $ getLanguageLocale languageKey) "" cityConfig.geoCodeConfig state.props.rideSearchProps.autoCompleteType state.props.rideSearchProps.sessionId)
          let 
            sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions
            predictionList = getLocationList sortedByDistanceList
            listToBeUpdated = 
              if state.props.isSource == Just true 
                then state.data.recentSearchs.predictionArray 
                else state.data.destinationSuggestions
            recentLists = 
              updateLocListWithDistance 
                listToBeUpdated
                state.props.sourceLat 
                state.props.sourceLong 
                true 
                state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist 
            filteredRecentsList = filterRecentSearches recentLists predictionList
            filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList
            filteredLocationList = 
              map
                (\item -> do
                          let savedLocation  = getPrediction item state.data.savedLocations
                              locIsPresentInSavedLoc = checkPrediction item state.data.savedLocations
                          if not locIsPresentInSavedLoc then
                            item {
                              lat = savedLocation.lat,
                              lon = savedLocation.lon,
                              locationItemType = Just SAVED_LOCATION,
                              postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav_red" }
                            else
                              item {
                                lat = item.lat,
                                lon = item.lon,
                                locationItemType = item.locationItemType,
                                postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav" }
                ) (filteredRecentsList <> filteredPredictionList)
            cachedPredictions = DHM.insert (DS.toLower input) filteredLocationList state.props.rideSearchProps.cachedPredictions
          modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data {locationList = filteredLocationList}
                                                                      , props{ searchLocationModelProps{ isAutoComplete = true
                                                                                                      , showLoader = false }
                                                                             , rideSearchProps { cachedPredictions = cachedPredictions } }})
      homeScreenFlow
    GET_QUOTES state -> do
          setValueToLocalStore AUTO_SELECTING "false"
          setValueToLocalStore LOCAL_STAGE (show FindingQuotes)
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          liftFlowBT $ logEvent logField_ "ny_user_request_quotes"
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                          {key : "Estimate Fare (₹)", value : unsafeToForeign (state.data.suggestedAmount + state.data.rateCard.additionalFare)},
                                                                                                          {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                          {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges}]
          if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
            void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_auto_confirm"
            pure unit
          else do
            pure unit
          void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
          void $ Remote.selectEstimateBT (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.data.selectedEstimatesObject.id)
          logStatus "finding_quotes" ("estimateId : " <> state.props.estimateId)
          homeScreenFlow
    SELECT_ESTIMATE state -> do
        logStatus "setting_price" ""
        void $ pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
        updateLocalStage SettingPrice
        let sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag
            destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
            srcMarker = (Remote.normalRoute "").srcMarker
            destMarker = (Remote.normalRoute "").destMarker
        case state.props.routeEndPoints of
          Just points -> do
            let sourceMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = srcMarker, primaryText = points.source.place, secondaryText = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, labelImage = sourceSpecialTagIcon, position{ lat = points.source.lat, lng = points.source.lng } }
                destMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = destMarker, primaryText = points.destination.place, labelImage = destSpecialTagIcon, position{ lat = points.destination.lat, lng = points.destination.lng } }
            lift $ lift $ liftFlow $ updateMarker sourceMarkerConfig
            lift $ lift $ liftFlow $ updateMarker destMarkerConfig
          Nothing -> pure unit
        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList) $ do
        updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
          void $ pure $ enableMyLocation false
          let selectedQuote = if state.props.isSpecialZone && state.data.currentSearchResultType == QUOTES then state.data.specialZoneSelectedQuote else state.props.selectedQuote
          logStatus "confirming_quote" selectedQuote
          if isJust selectedQuote then do
            updateLocalStage ConfirmingRide
            response  <- lift $ lift $ Remote.rideConfirm (fromMaybe "" selectedQuote)
            case response of
              Right (ConfirmRes resp) -> do
                let bookingId = resp.bookingId
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp}})
                homeScreenFlow
              Left err  -> do
                if not (err.code == 400 && (decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED") then pure $ toast (getString STR.ERROR_OCCURED_TRY_AGAIN) else pure unit
                void $ setValueToLocalStore AUTO_SELECTING "false"
                void $ pure $ updateLocalStage QuoteList
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = QuoteList,selectedQuote = Nothing, expiredQuotes = snoc state.props.expiredQuotes (fromMaybe "" state.props.selectedQuote)}, data {quoteListModelState = []}})
                homeScreenFlow
            else homeScreenFlow
    ONGOING_RIDE state -> do
      setValueToLocalStore TRACKING_ENABLED "True"
      setValueToLocalStore TRACKING_DRIVER "False"
      setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
      let srcLat = state.data.driverInfoCardState.sourceLat
          srcLon = state.data.driverInfoCardState.sourceLng
          dstLat = state.data.driverInfoCardState.destinationLat
          dstLon = state.data.driverInfoCardState.destinationLng
      updateLocalStage state.props.currentStage
      if spy "ONGOING_RIDEONGOING_RIDE CURRENT" state.props.currentStage == RideCompleted then
        do
          let sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag
              destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
              markers = Remote.normalRoute ""
              srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker }
              destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker }
          void $ Remote.drawMapRoute srcLat srcLon dstLat dstLon srcMarkerConfig destMarkerConfig "DRIVER_LOCATION_UPDATE" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon true getPolylineAnimationConfig) 
          homeScreenFlow
        else if state.props.currentStage == HomeScreen then
          do
            void $ pure $ removeAllPolylines ""
            void $ pure $ spy "INSIDE ELSE IF OF ONGOING" state.props.currentStage
            void $ updateLocalStage HomeScreen
            updateUserInfoToState state
            homeScreenFlow
          else do
            lift $ lift $ triggerRideStatusEvent "DRIVER_ASSIGNMENT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
            homeScreenFlow
    CANCEL_RIDE_REQUEST state -> do
      response <- lift $ lift $ Remote.cancelRide (Remote.makeCancelRequest state) (state.props.bookingId)
      case response of
        Right _ -> do
          void $ pure $ currentPosition ""
          void $ updateLocalStage HomeScreen
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_rider_cancellation" $ [ {key : "Reason code", value : unsafeToForeign state.props.cancelReasonCode},
                                                                                                          {key : "Additional info", value : unsafeToForeign state.props.cancelDescription},
                                                                                                          {key : "Pickup", value : unsafeToForeign state.data.driverInfoCardState.source},
                                                                                                          {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                          {key : "Night Ride", value : unsafeToForeign state.data.rateCard.nightCharges},
                                                                                                          {key : "BookingId", value : unsafeToForeign state.props.bookingId}]
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{autoScroll = false, isCancelRide = false,currentStage = HomeScreen, rideRequestFlow = false, isSearchLocation = NoView }})
          lift $ lift $ triggerRideStatusEvent "CANCELLED_PRODUCT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
          void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
          liftFlowBT $ logEvent logField_ "ny_user_ride_cancelled_by_user"
          liftFlowBT $ logEvent logField_ $ "ny_user_cancellation_reason: " <> state.props.cancelReasonCode
          removeChatService ""
          updateUserInfoToState state
          homeScreenFlow
        Left err -> do
          void $ pure $ toast $ getString STR.UNABLE_TO_CANCEL_RIDE
          homeScreenFlow
    FCM_NOTIFICATION notification state-> do
        let rideID = state.data.driverInfoCardState.rideId
            srcLat = state.data.driverInfoCardState.sourceLat
            srcLon = state.data.driverInfoCardState.sourceLng
            dstLat = state.data.driverInfoCardState.destinationLat
            dstLon = state.data.driverInfoCardState.destinationLng
        setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
        setValueToLocalStore FINDING_QUOTES_POLLING "false"
        setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          setValueToLocalStore TRACKING_ENABLED "False"
          pure unit
          else do
            setValueToLocalStore TRACKING_ENABLED "True"
            pure unit
        case notification of
            "TRIP_STARTED"        -> do -- OTP ENTERED'
                                      logStatus "trip_started_notification" ("bookingId : " <> state.props.bookingId)
                                      checkRideStatus true
                                      (GlobalState updatedState) <- getState
                                      let homeScreenState = updatedState.homeScreen
                                      void $ pure $ JB.exitLocateOnMap ""
                                      when (homeScreenState.props.currentStage == RideStarted) $ do 
                                        let shareAppCount = getValueToLocalStore SHARE_APP_COUNT
                                        if shareAppCount == "__failed" then do
                                          setValueToLocalStore SHARE_APP_COUNT "1"
                                        else if shareAppCount /= "-1" then do
                                          setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount))))+1))
                                        else pure unit
                                        void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
                                        let newState = homeScreenState{data{route = Nothing},props{isCancelRide = false,waitingTimeTimerIds = [], showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0, showChatNotification = false, cancelSearchCallDriver = false  }}
                                            currTrip = {sourceLat : srcLat, 
                                                        sourceLong : srcLon, 
                                                        destLat : dstLat, 
                                                        destLong : dstLon, 
                                                        source : state.data.driverInfoCardState.source,
                                                        destination  : state.data.driverInfoCardState.destination,
                                                        sourceAddress : state.data.driverInfoCardState.sourceAddress,
                                                        destinationAddress : state.data.driverInfoCardState.destinationAddress,
                                                        locationScore: Just 1.0,
                                                        recencyDate : Nothing,
                                                        frequencyCount : Just 1,
                                                        isSpecialZone : state.props.isSpecialZone,
                                                        vehicleVariant : Just state.data.driverInfoCardState.vehicleVariant,
                                                        serviceTierNameV2 : state.data.driverInfoCardState.serviceTierName
                                                        }
                                            currentSourceGeohash = runFn3 encodeGeohash srcLat srcLon state.data.config.suggestedTripsAndLocationConfig.geohashPrecision
                                            currentMap = getSuggestionsMapFromLocal FunctionCall
                                            updatedMap = addOrUpdateSuggestedTrips currentSourceGeohash currTrip false currentMap state.data.config.suggestedTripsAndLocationConfig false
                                        void $ pure $ setSuggestionsMap updatedMap
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> newState{data{suggestionsData{suggestionsMap = getSuggestionsMapFromLocal FunctionCall }}, props{showAcWorkingPopup = true}})
                                        lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      removeChatService ""
                                      homeScreenFlow
            "TRIP_FINISHED"       -> do -- TRIP FINISHED
                                      logStatus "trip_finished_notification" ("bookingId : " <> state.props.bookingId)
                                      void $ pure $ JB.exitLocateOnMap ""
                                      firstRideCompletedEvent ""
                                      let sourceSpecialTagIcon = zoneLabelIcon state.props.zoneType.sourceTag
                                          destSpecialTagIcon = zoneLabelIcon state.props.zoneType.destinationTag
                                      void $ pure $ metaLogEvent "ny_user_ride_completed"
                                      void $ updateLocalStage HomeScreen
                                      setValueToLocalStore IS_SOS_ACTIVE "false"
                                      deleteValueFromLocalStore SELECTED_VARIANT
                                      removeChatService ""
                                      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{data{sosId = ""}})
                                      if (state.props.bookingId /= "") then do
                                        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
                                        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                                            (RideBookingDetails contents) = bookingDetails.contents
                                            (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                                            finalAmount =  getFinalAmount (RideBookingRes resp)
                                            differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
                                            nightSafetyFlow = showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime
                                        lift $ lift $ triggerRideStatusEvent notification (Just finalAmount) (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                        setValueToLocalStore PICKUP_DISTANCE "0"
                                        liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_ride_completed" (rideCompletedDetails (RideBookingRes resp))
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{
                                          data{
                                            startedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime ) "h:mm A", 
                                            startedAtUTC = fromMaybe "" resp.rideStartTime ,
                                            endedAt = convertUTCtoISC (fromMaybe "" resp.rideEndTime ) "h:mm A", 
                                            finalAmount = finalAmount, 
                                            rideRatingState {driverName = ride.driverName, 
                                            rideId = ride.id , 
                                            distanceDifference = differenceOfDistance} , 
                                            ratingViewState { 
                                              rideBookingRes = (RideBookingRes resp), 
                                              issueFacedView = nightSafetyFlow
                                            }, 
                                            driverInfoCardState {initDistance = Nothing}, 
                                            vehicleVariant = ride.vehicleVariant
                                          },
                                          props{
                                            currentStage = RideCompleted, 
                                            estimatedDistance = contents.estimatedDistance, 
                                            nightSafetyFlow = nightSafetyFlow,
                                            showOfferedAssistancePopUp = (resp.hasDisability == Just true)
                                            }
                                          }
                                        )
                                        homeScreenFlow
                                        else homeScreenFlow
            "CANCELLED_PRODUCT"   -> do -- REMOVE POLYLINES
                                      logStatus "ride_cancelled_notification" ("bookingId : " <> state.props.bookingId)
                                      void $ pure $ JB.exitLocateOnMap ""
                                      void $ pure $ removeAllPolylines ""
                                      void $ updateLocalStage HomeScreen
                                      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{data{sosId = ""}})
                                      setValueToLocalStore IS_SOS_ACTIVE "false"
                                      removeChatService ""
                                      setValueToLocalStore PICKUP_DISTANCE "0"
                                      lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      updateUserInfoToState state
                                      void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
                                      permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
                                      permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
                                      if not (permissionConditionA && permissionConditionB) then do
                                        modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                                        permissionScreenFlow
                                      else homeScreenFlow
            "DRIVER_ASSIGNMENT"   -> do
                                      logStatus "ride_assigned_notification" ("bookingId : " <> state.props.bookingId)
                                      if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted )) then do
                                         setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
                                         void $ liftFlowBT $ logEvent logField_ "ny_fs_driver_assignment"
                                         lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                         checkRideStatus true
                                         homeScreenFlow
                                      else homeScreenFlow
            "REALLOCATE_PRODUCT"  -> do
                                      logStatus "ride_reallocated_notification" ("estimateId : " <> state.props.estimateId)
                                      void $ pure $ JB.exitLocateOnMap ""
                                      void $ pure $ removeAllPolylines ""
                                      removeChatService ""
                                      setValueToLocalStore PICKUP_DISTANCE "0"
                                      (GlobalState updatedState) <- getState 
                                      let homeScreenState = updatedState.homeScreen{data { quoteListModelState = [] }, props { isBanner = state.props.isBanner, currentStage = ReAllocated, estimateId = updatedState.homeScreen.props.estimateId, reAllocation { showPopUp = true }, tipViewProps { isVisible = updatedState.homeScreen.props.tipViewProps.activeIndex >= 0 }, selectedQuote = Nothing, isCancelRide = false, cancelSearchCallDriver = false}}
                                      let updatedState = case (getTipViewData "LazyCheck") of
                                                          Just (TipViewData tipView) -> homeScreenState{ props{ tipViewProps{ stage = tipView.stage , activeIndex = tipView.activeIndex , isVisible = tipView.activeIndex >= 0 } } }
                                                          Nothing -> homeScreenState{ props{ tipViewProps = HomeScreenData.initData.props.tipViewProps } }
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState)
                                      void $ pure $ clearTimerWithId <$> state.props.waitingTimeTimerIds
                                      void $ pure $ setValueToLocalNativeStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
                                      updateLocalStage ReAllocated
                                      homeScreenFlow
            _
              | any (_ == notification) [ "FOLLOW_RIDE", "SHARE_RIDE", "SOS_RESOLVED" ] -> do
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { followers = Nothing } })
                currentFlowStatus
            _
              | any (_ == notification) [ "SOS_MOCK_DRILL", "SOS_MOCK_DRILL_NOTIFY" ] -> do
              updateFollower true false $ Just notification
            "SOS_TRIGGERED" -> do
              logStatus "sos_triggered_notification" ""
              updateFollower true false Nothing
            "SAFETY_ALERT_DEVIATION" -> do
              logStatus "safety_alert_deviation_notification" ""
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { safetyAlertType = Just ST.DEVIATION } })
              homeScreenFlow
            _                     -> homeScreenFlow

    LOGOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      void $ pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      void $ pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
      void $ pure $ deleteValueFromLocalStore CUSTOMER_ID
      void $ pure $ deleteValueFromLocalStore CONTACTS
      void $ pure $ deleteValueFromLocalStore USER_EMAIL
      void $ pure $ deleteValueFromLocalStore CUSTOMER_FIRST_RIDE
      void $ pure $ factoryResetApp ""
      void $ pure $ clearCache ""
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_logout"
      void $ pure $ (setText (getNewIDWithTag "EnterMobileNumberEditText") "" )
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> EnterMobileNumberScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      enterMobileNumberScreenFlow -- Removed choose langauge screen
    SUBMIT_RATING state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_ride_give_feedback" $ [{key : "Rating", value : unsafeToForeign state.data.ratingViewState.selectedRating}]
      void $ lift $ lift $ fork $ HelpersAPI.callApi $ Remote.makeRideFeedBackReq state.data.rideRatingState.rideId state.data.rideRatingState.feedbackList
      void $ lift $ lift $ fork $ HelpersAPI.callApi $ getfeedbackReq state
      void $ updateLocalStage HomeScreen
      let finalAmount = if state.data.finalAmount == 0 then state.data.rideRatingState.finalAmount else state.data.finalAmount
      let bookingId = if state.props.bookingId == "" then state.data.rideRatingState.bookingId else state.props.bookingId
      pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
                                          event : "process_result"
                                        , payload : Just {
                                          action : "feedback_submitted"
                                        , trip_amount : Just finalAmount
                                        , trip_id : Just bookingId
                                        , ride_status : Nothing
                                        , screen : Just $ getScreenFromStage state.props.currentStage
                                        , exit_app : false
                                        }
                                        }
      updateUserInfoToState state
      if state.props.currentStage == RideCompleted then
        if (getSearchType unit) == "direct_search" then do
          void $ updateLocalStage SearchLocationModel
          checkAndUpdateLocations
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel}})
          else pure unit
        else pure unit
      if state.data.rideRatingState.rating == 5 then do
        void $ pure $ launchInAppRatingPopup unit
        pure unit
        else pure unit
      homeScreenFlow
    CANCEL -> homeScreenFlow
    RELOAD saveToCurrLocs -> do
      (GlobalState state) <- getState
      void $ liftFlowBT $ setMapPadding 0 0 0 0
      if state.homeScreen.props.currentStage == SearchLocationModel then do
        if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
        void $ pure $ toggleBtnLoader "" false
        homeScreenFlow
        else do
          if state.homeScreen.props.sourceLat/=0.0 && state.homeScreen.props.sourceLong /= 0.0 then do
            (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong)
            let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
            let pickUpPoints =  if null srcSpecialLocation.gatesInfo then
                                  filterHotSpots state.homeScreen sourceServiceabilityResp.hotSpotInfo state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong
                                else
                                  mapSpecialZoneGates srcSpecialLocation.gatesInfo
            if (sourceServiceabilityResp.serviceable ) then do
              let cityName = getCityNameFromCode sourceServiceabilityResp.city
                  geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
              (GlobalState currentState) <- getState
              fullAddress <- getPlaceName currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong HomeScreenData.dummyLocation false
              case fullAddress of
                Just (PlaceName address) -> do
                  modifyScreenState $ HomeScreenStateType 
                    (\homeScreen -> 
                      homeScreen
                        { data
                            { source = address.formattedAddress 
                            , sourceAddress = encodeAddress address.formattedAddress [] Nothing currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong 
                            }
                        , props
                            { isSrcServiceable = true
                            }
                        }
                    )
                Nothing -> pure unit
              void $ pure $ setCleverTapUserProp [{key : "Customer Location", value : unsafeToForeign $ show cityName}]
              setValueToLocalStore CUSTOMER_LOCATION $ show cityName
              modifyScreenState $ HomeScreenStateType 
                (\homeScreen -> 
                  homeScreen
                    { data
                        { polygonCoordinates = geoJson
                        , nearByPickUpPoints = pickUpPoints 
                        }
                    , props
                        { isSrcServiceable = true
                        , showlocUnserviceablePopUp = false
                        , city = cityName
                        , locateOnMapProps{ sourceLocationName = Just srcSpecialLocation.locationName }
                        }
                    }
                )
              if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then
                addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source
              else pure unit
            else do
              let isWhitelisted = any (_ == getValueFromWindow (show MOBILE_NUMBER)) (getNumbersToWhiteList "")
              void $ pure $ firebaseLogEvent "ny_loc_unserviceable"
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = isWhitelisted , showlocUnserviceablePopUp = (not isWhitelisted) }})
          else do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})

      homeScreenFlow
    RETRY  -> homeScreenFlow
    REALLOCATE_RIDE state -> do
      if DS.null state.props.estimateId then
        currentFlowStatus
      else homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long-> do
      let isWhitelisted = any (_ == getValueFromWindow (show MOBILE_NUMBER)) (getNumbersToWhiteList "")
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat long)
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation sourceServiceabilityResp.specialLocation
      let sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat
          sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong
          cityName = if sourceServiceabilityResp.serviceable then getCityNameFromCode sourceServiceabilityResp.city else updatedState.props.city
          srcServiceability = isWhitelisted || sourceServiceabilityResp.serviceable
      sourcePlaceName <- getPlaceName sourceLat sourceLong HomeScreenData.dummyLocation false
      setValueToLocalStore CUSTOMER_LOCATION (show cityName)
      void $ pure $ firebaseLogEvent $ "ny_loc_unserviceable_" <> show (not sourceServiceabilityResp.serviceable)
      modifyScreenState $ HomeScreenStateType 
        (\homeScreen -> 
        homeScreen
          { props
            { locateOnMapLocation
              { sourceLat = sourceLat
              , sourceLng = sourceLong
              , source = case sourcePlaceName of
                            Just (PlaceName placeDetails) -> placeDetails.formattedAddress
                            Nothing -> getString STR.CURRENT_LOCATION 
              }
            , sourceLat = sourceLat
            , sourceLong = sourceLong
            , isSrcServiceable = srcServiceability
            , showlocUnserviceablePopUp = not srcServiceability
            , city = cityName
            }
          }
        )
      homeScreenFlow
    HOME_SCREEN -> do
        (GlobalState state) <- getState
        when (isLocalStageOn FindingQuotes) $ do
          cancelEstimate state.homeScreen.props.estimateId
        void $ pure $ removeAllPolylines ""
        void $ lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
        void $ pure $ currentPosition ""
        void $ updateLocalStage HomeScreen
        updateUserInfoToState state.homeScreen
        homeScreenFlow
    CHECK_CURRENT_STATUS -> do
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes) $ do
        cancelEstimate state.homeScreen.props.estimateId
      void $ pure $ removeAllPolylines ""
      void $ lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
      void $ pure $ currentPosition ""
      void $ updateLocalStage HomeScreen
      updateUserInfoToState state.homeScreen
      currentFlowStatus
    UPDATE_LOCATION_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
          (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          cityName = getCityNameFromCode sourceServiceabilityResp.city
          pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
          gateAddress = fromMaybe HomeScreenData.dummyLocation (head pickUpPoints)
      setValueToLocalStore CUSTOMER_LOCATION $ show cityName
      checkForSpecialZoneAndHotSpots state (ServiceabilityRes sourceServiceabilityResp) lat lon
      let cachedLat = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLat else state.props.locateOnMapLocation.destinationLat)
          cachedLon = (if state.props.isSource == Just true then state.props.locateOnMapLocation.sourceLng else state.props.locateOnMapLocation.destinationLng)
          cachedLocation = (if state.props.isSource == Just true then state.props.locateOnMapLocation.source else state.props.locateOnMapLocation.destination)
          distanceBetweenLatLong = getDistanceBwCordinates lat lon cachedLat cachedLon
          isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius/1000.0) 
      modifyScreenState $ HomeScreenStateType (\homeScreen ->
          homeScreen { 
            props {
              city = if state.props.isSource == Just true then cityName else AnyCity, 
              sourcePlaceId = if state.props.isSource == Just true then Nothing else homeScreen.props.sourcePlaceId,
              destinationPlaceId = if state.props.isSource == Just false then Nothing else homeScreen.props.destinationPlaceId,
              destinationLat = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lat else state.props.destinationLat,
              destinationLong = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lon else state.props.destinationLong,
              sourceLat = if state.props.isSource == Just true then lat else state.props.sourceLat,
              sourceLong = if state.props.isSource == Just true then lon else state.props.sourceLong,
              confirmLocationCategory = getZoneType srcSpecialLocation.category
              }
            })
      (GlobalState globalState) <- getState
      let state = globalState.homeScreen
      if isMoreThan20Meters || cachedLocation == "" then do
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of 
          Just (PlaceName placeDetails) -> do
            let currentLocationItem = getCurrentLocationItem placeDetails state lat lon
            void $ liftFlowBT $ logEvent logField_ "ny_user_placename_api_lom_onDrag"
            modifyScreenState $ HomeScreenStateType (\homeScreen ->
            homeScreen {
              data {
                destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then placeDetails.formattedAddress else homeScreen.data.destination,
                selectedLocationListItem = currentLocationItem, 
                source = if state.props.isSource == Just true then placeDetails.formattedAddress else homeScreen.data.source,
                sourceAddress = case state.props.isSource , (state.props.currentStage /= ConfirmingLocation) of
                  Just true, true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing lat lon
                  _ , _-> encodeAddress homeScreen.data.source [] state.props.sourcePlaceId lat lon,
                destinationAddress = case state.props.isSource,(state.props.currentStage /= ConfirmingLocation) of
                  Just false , true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing lat lon
                  _ , _ -> encodeAddress homeScreen.data.destination [] state.props.destinationPlaceId lat lon
              }
              })
          Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN  
      else do
        void $ liftFlowBT $ logEvent logField_ "ny_user_placename_cache_lom_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
          homeScreen {
            data {
              destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then state.props.locateOnMapLocation.destination else homeScreen.data.destination,
              source = if state.props.isSource == Just true then state.props.locateOnMapLocation.source else homeScreen.data.source,
              sourceAddress = case state.props.isSource , (state.props.currentStage /= ConfirmingLocation) of
                Just true, true -> state.props.locateOnMapLocation.sourceAddress 
                _ , _-> state.data.sourceAddress, 
              destinationAddress = case state.props.isSource,(state.props.currentStage /= ConfirmingLocation) of
                Just false , true -> state.props.locateOnMapLocation.destinationAddress 
                _ , _ -> state.data.destinationAddress 
            }
            })
      let _ = spy "UPDATE_LOCATION_NAME" "UPDATE_LOCATION_NAME"
      homeScreenFlow
    UPDATE_PICKUP_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
          (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
          cityName = getCityNameFromCode sourceServiceabilityResp.city
          geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
      setValueToLocalStore CUSTOMER_LOCATION $ show cityName
      checkForSpecialZoneAndHotSpots state (ServiceabilityRes sourceServiceabilityResp) lat lon
      let distanceBetweenLatLong = getDistanceBwCordinates lat lon state.props.locateOnMapLocation.sourceLat state.props.locateOnMapLocation.sourceLng
          isMoreThan20Meters = distanceBetweenLatLong > (state.data.config.mapConfig.locateOnMapConfig.apiTriggerRadius/1000.0)
      modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          props {
            sourceLat = lat ,
            sourceLong = lon,
            confirmLocationCategory = getZoneType srcSpecialLocation.category,
            city = cityName
            }
          })
      if isMoreThan20Meters then do
        let gateAddress = fromMaybe HomeScreenData.dummyLocation (head pickUpPoints)
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName address) -> do
            void $ liftFlowBT $ logEvent logField_ "ny_user_placename_api_cpu_onDrag"
            modifyScreenState $ HomeScreenStateType (\homeScreen ->
            homeScreen {
              data {
                source = address.formattedAddress ,
                sourceAddress = encodeAddress address.formattedAddress address.addressComponents Nothing lat lon }
              })
          Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
      else do
        void $ liftFlowBT $ logEvent logField_ "ny_user_placename_cache_cpu_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            source = state.props.locateOnMapLocation.source,
            sourceAddress = state.props.locateOnMapLocation.sourceAddress
            }
          })
      let _ = spy "UPDATE_PICKUP_LOCATION_NAME" "UPDATE_PICKUP_LOCATION_NAME"
      homeScreenFlow
    GO_TO_FAVOURITES_  -> do
        void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_addresses"
        savedLocationFlow
    OPEN_GOOGLE_MAPS state -> do
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_gmaps"
      (GetDriverLocationResp resp) <- Remote.getDriverLocationBT (state.data.driverInfoCardState.rideId)
      let sourceLat = (resp^._lat)
          sourceLng = (resp^._lon)
          destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
          destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
      void $ pure $ openNavigation destLat destLng "DRIVE"
      homeScreenFlow
    IN_APP_TRACK_STATUS state -> do
      case state.props.currentStage of
          RideAccepted -> do
                          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_pickup_track_inapp"
                          pure unit
          RideStarted  -> do
                          void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_inapp"
                          pure unit
          _           -> pure unit
      if (spy "driver current Stage "isLocalStageOn RideAccepted) || (spy "driver current Stage " isLocalStageOn RideStarted) then do
        setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          setValueToLocalStore TRACKING_ENABLED "False"
          homeScreenFlow
          else do
            setValueToLocalStore TRACKING_ENABLED "True"
            homeScreenFlow
        else
          homeScreenFlow
    UPDATE_SAVED_LOCATION -> do
      (SavedLocationsListRes savedLocationResp) <- FlowCache.updateAndFetchSavedLocations false
      updateSourceLocation ""
      updateCurrentLocation ""
      fetchAndModifyLocationLists $ AddNewAddress.getSavedLocations savedLocationResp.list
      homeScreenFlow

    GO_TO_INVOICE_ updatedState -> do
      let prevRideState = updatedState.data.rideRatingState
      let finalAmount = show prevRideState.finalAmount
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen= true},data{totalAmount = ((getCurrency appConfig) <> " " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ((getCurrency appConfig) <> " " <> finalAmount), selectedItem {date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId,rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId,vehicleNumber = prevRideState.vehicleNumber,time = prevRideState.rideStartTime,source = prevRideState.source,destination = prevRideState.destination,driverName = prevRideState.driverName,totalAmount = ((getCurrency appConfig) <> " " <> finalAmount)}, config = updatedState.data.config}})
      invoiceScreenFlow

    CHECK_FOR_DUPLICATE_SAVED_LOCATION state -> do
      let recents = map
                    (\item -> item{postfixImageVisibility = false, postfixImageUrl = ""}
                      ) (differenceOfLocationLists (state.data.recentSearchs.predictionArray) state.data.savedLocations)

      case state.data.selectedLocationListItem of
        Nothing -> do
          modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { props
                { showSavePlaceView = false
                , fromHome = state.props.currentStage == HomeScreen
                , fromScreen = Screen.getScreen Screen.HOME_SCREEN
                , editLocation = false
                , editSavedLocation = false
                , isLocateOnMap = false
                , isBtnActive = true
                , isSearchedLocationServiceable = true
                , tagExists = false
                , placeNameExists = false }
              , data
                { addressSavedAs = ""
                , placeName = ""
                , savedLocations = state.data.savedLocations
                , locationList = recents
                , recentSearchs{predictionArray = recents}
                , selectedTag = state.props.tagType
                , savedTags = AddNewAddress.getSavedTagsFromHome state.data.savedLocations
                , address = ""
                , activeIndex = case state.props.tagType of
                                  Just tag -> case tag of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                _        -> Just 2
                                  Nothing  -> Nothing }})
          addNewAddressScreenFlow ""
        Just selectedLocationListItem -> do
          case selectedLocationListItem.locationItemType of
            Just RECENTS ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            Nothing ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            _ -> do
              (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedLocationListItem.title <> ", " <> selectedLocationListItem.subTitle) selectedLocationListItem.placeId (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon) selectedLocationListItem

              let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
              let (LatLong placeLatLong) = (placeName.location)

              (ServiceabilityResDestination serviceabilityRes) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest (placeLatLong.lat) (placeLatLong.lon))
              case (serviceabilityRes.serviceable) of
                false -> do
                          void $ pure $ toast ("Location Unserviceable")
                          homeScreenFlow
                _     ->   modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{ selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}})
              getDistanceDiff  state{data{ saveFavouriteCard{selectedItem{lat = Just (placeLatLong.lat), lon =Just (placeLatLong.lon) }},selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}} (placeLatLong.lat) (placeLatLong.lon)
    GO_TO_CALL_EMERGENCY_CONTACT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "EmergencyContact" state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo) state.data.driverInfoCardState.rideId false)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_POLICE state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "Police" "") state.data.driverInfoCardState.rideId false)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_SUPPORT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "CustomerCare" "") state.data.driverInfoCardState.rideId false)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_SOS_STATUS state -> do
        res <- Remote.userSosStatusBT state.props.emergencyHelpModelState.sosId (Remote.makeSosStatus state.props.emergencyHelpModelState.sosStatus "")
        homeScreenFlow
    GO_TO_FETCH_CONTACTS state-> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true,
          priority :fromMaybe 1 item.priority,
          enableForFollowing : fromMaybe false item.enableForFollowing,
          enableForShareRide : fromMaybe false item.enableForShareRide,
          onRide : fromMaybe false item.onRide
        }) res.defaultEmergencyNumbers
      let newContacts = transformContactList contacts
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{emergencyContactData = newContacts}}})
      homeScreenFlow
    SAVE_FAVOURITE state -> do
      let tag = case  (toLower state.data.saveFavouriteCard.tag) of
                  "work" -> "Work"
                  "home" -> "Home"
                  _      -> state.data.saveFavouriteCard.tag
      void $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      case state.data.saveFavouriteCard.selectedItem.lat , state.data.saveFavouriteCard.selectedItem.lon of
        Nothing , Nothing -> fetchLatAndLong state tag
        _ , _ -> do
          resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId state.data.saveFavouriteCard.selectedItem.lat state.data.saveFavouriteCard.selectedItem.lon [])
          pure unit
      void $  pure $ toast (getString STR.FAVOURITE_ADDED_SUCCESSFULLY)
      (SavedLocationsListRes savedLocationResp )<- FlowCache.updateAndFetchSavedLocations true
      let updatedLocationList = getUpdatedLocationList state.data.locationList state.data.saveFavouriteCard.selectedItem.placeId
      let updatedRecents = getUpdatedLocationList state.data.recentSearchs.predictionArray  state.data.saveFavouriteCard.selectedItem.placeId
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList = updatedLocationList, recentSearchs{predictionArray = updatedRecents},savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list)}})
      homeScreenFlow
    GO_TO_REFERRAL referralType -> do
      let referralCode = getValueToLocalStore CUSTOMER_REFERRAL_CODE
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { referralType = referralType, referralCode = if any (_ == referralCode) ["__failed", ""] then "" else referralCode })
      referralScreenFlow
    ON_CALL state callType exophoneNumber -> do
      (OnCallRes res) <- Remote.onCallBT (Remote.makeOnCallReq state.data.driverInfoCardState.rideId (show callType) exophoneNumber)
      homeScreenFlow
    TRIGGER_PERMISSION_FLOW flowType -> do 
      modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen{stage = flowType})
      permissionScreenFlow
    REPORT_ISSUE state -> do
       if isNothing state.data.ratingViewState.issueReason then do
        void $ Remote.callbackRequestBT FunctionCall
        void $ pure $ toast $ getString STR.WE_WILL_GIVE_YOU_CALLBACK
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false, selectedYesNoButton = -1} }})
        homeScreenFlow
       else do
        let bookingId = if DS.null state.props.bookingId then state.data.rideRatingState.bookingId else state.props.bookingId
            isNightSafety = Just $ state.props.nightSafetyFlow
        void $ Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just bookingId) (fromMaybe "" state.data.ratingViewState.issueReason) state.data.ratingViewState.issueDescription isNightSafety)
        void $ pure $ toast $ getString STR.YOUR_ISSUE_HAS_BEEN_REPORTED
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false, openReportIssue = false, selectedYesNoButton = -1} }, props {nightSafetyFlow = false}})
        homeScreenFlow
    RIDE_DETAILS_SCREEN state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = Home}})
      tripDetailsScreenFlow
    GO_TO_TICKET_BOOKING_FLOW state -> do 
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{props{currentStage = DescriptionStage, previousStage = DescriptionStage}})
      placeListFlow
    REPEAT_RIDE_FLOW_HOME state -> do
      updateRepeatRideDetails state
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.sourceLat state.sourceLong)
      let cityName = getCityNameFromCode sourceServiceabilityResp.city
          (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
          pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{city = cityName, locateOnMapProps{ sourceLocationName = Just srcSpecialLocation.locationName, sourceGeoJson = Just geoJson, sourceGates = Just pickUpPoints}, repeatRideServiceTierName = state.serviceTierNameV2}})
      setValueToLocalStore CUSTOMER_LOCATION $ show cityName
      when (state.isSpecialZone) $ do
        modifyScreenState $ HomeScreenStateType 
          (\homeScreen -> 
            homeScreen
              { props
                  { rideSearchProps { sourceSelectType = ST.REPEAT_RIDE } }
              , data
                  { polygonCoordinates = geoJson
                  , nearByPickUpPoints = pickUpPoints
                  }
              }
          )
      rideSearchFlow "REPEAT_RIDE_FLOW"
    EXIT_TO_TICKETING _ -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData{props{navigateToHome = true}})
      modifyScreenState $ TicketingScreenStateType (\_ -> PlaceListData.initData{ props { hideMyTickets = false }})
      placeListFlow
    GO_TO_METRO_BOOKING _ -> do
      modifyScreenState $ MetroTicketBookingScreenStateType (\_ -> MetroTicketBookingScreenData.initData)
      metroTicketBookingFlow
    GO_TO_HELP_AND_SUPPORT -> flowRouter HelpAndSupportScreenFlow
    GO_TO_RENTALS_FLOW -> do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
      searchLocationFlow --rentalsScreenFlow
    GO_TO_SCHEDULED_RIDES -> rideScheduledFlow
    GO_TO_NAMMASAFETY state triggerSos showtestDrill -> do
      modifyScreenState
        $ NammaSafetyScreenStateType
            ( \nammaSafetyScreen ->
                nammaSafetyScreen
                  { props
                    { triggeringSos = false
                    , timerValue = defaultTimerValue
                    , showTestDrill = false
                    , showShimmer = true
                    , confirmTestDrill = showtestDrill
                    , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                    , checkPastRide = state.props.currentStage == HomeScreen
                    , showCallPolice = if triggerSos then state.props.isSafetyCenterDisabled else false
                    }
                  , data
                    { rideId = state.data.driverInfoCardState.rideId
                    , vehicleDetails = state.data.driverInfoCardState.registrationNumber
                    }
                  }
            )
      case (triggerSos || showtestDrill) of
        true -> do 
          let isRideCompleted = state.props.currentStage == RideCompleted
          modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props{reportPastRide = isRideCompleted}, data{lastRideDetails = if isRideCompleted then Arr.head $ myRideListTransformer true [state.data.ratingViewState.rideBookingRes] else Nothing}})
          activateSafetyScreenFlow
        false -> safetySettingsFlow
    GO_TO_SAFETY_SETTING_SCREEN -> do
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { props{isOffUs = true } })
      safetySettingsFlow
    SAFETY_SUPPORT state isSafe -> do
      res <- lift $ lift $ Remote.sendSafetySupport $ Remote.makeAskSupportRequest state.props.bookingId isSafe $ "User need help - Ride on different route"
      case res of
        Right resp -> do
                        void $ pure $ setValueToLocalNativeStore SAFETY_ALERT_TYPE "false"
                        when (isSafe) $ do
                          void $ pure $ toast $ getString STR.GLAD_TO_KNOW_YOU_ARE_SAFE
                          pure unit
        Left err   -> do
                        void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                        pure unit
              
      homeScreenFlow
    GO_TO_SHARE_RIDE state -> do
      contacts <- getFormattedContacts 
      let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
      if null contacts then do
        void $ pure $ shareTextMessage "" $ getString $ STR.TRACK_RIDE_STRING appName state.data.driverInfoCardState.driverName (state.data.config.appData.website <> "t?i="<>state.data.driverInfoCardState.rideId) state.data.driverInfoCardState.registrationNumber
        void $ pure $ cleverTapCustomEvent "ny_user_share_ride_via_link"
      else do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{contactList = Just contacts}, props{showShareRide = true}})
      homeScreenFlow
    GO_TO_NOTIFY_RIDE_SHARE state -> do
      let req = ShareRideReq { emergencyContactNumbers : map (\item -> item.number) $ filter (\item -> item.isSelected) $ fromMaybe [] state.data.contactList }
      void $ lift $ lift $ Remote.shareRide req
      void $ pure $ toast $ getString STR.RIDE_SHARED_WITH_SELECTED_CONTACTS
      void $ pure $ cleverTapCustomEvent "ny_user_auto_share_ride"
      pure $ toggleBtnLoader "" false
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{showShareRide = false},data{contactList = Nothing}})
      homeScreenFlow
    EXIT_TO_FOLLOW_RIDE -> updateFollower false false Nothing
    GO_TO_REPORT_SAFETY_ISSUE state -> do
      let language = fetchLanguage $ getLanguageLocale languageKey 
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0" "" ""
      let getOptionsRes' = mapWithIndex (\index (Option optionObj) -> optionObj { option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option)}) getOptionsRes.options
          messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) "Bot" (getCurrentUTC "") "Text" (500*(index + 1))) getOptionsRes.messages
          chats' = map (\(Message currMessage) -> Chat {chatId : currMessage.id, 
                                                        chatType : "IssueMessage", 
                                                        timestamp : (getCurrentUTC "")} )getOptionsRes.messages
      void $ pure $ cleverTapCustomEvent "ny_user_report_safety_issue_activated"
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData { data {entryPoint = ReportIssueChatScreenData.HomeScreenEntry, chats = chats', tripId = Just state.data.rideRatingState.rideId, categoryName = "Safety Related Issue", categoryId = "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0", options = getOptionsRes', chatConfig { messages = messages' },  selectedRide = Nothing } } )
      flowRouter IssueReportChatScreenFlow
    GO_TO_MY_METRO_TICKETS -> do
      modifyScreenState $ MetroMyTicketsScreenStateType (\state -> state { props {entryPoint = ST.HomeScreenToMetroMyTickets}})
      metroMyTicketsFlow
    GO_TO_SAFETY_EDUCATION -> do
      let videoList = RC.safetyBannerVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
      modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = videoList}, props{showVideoView = true, educationViewIndex = Just 0, fromBannerLink = true}})
      safetyEducationFlow
    REPEAT_SEARCH state -> do
      cancelEstimate state.props.estimateId
      findEstimates state
    GOTO_CONFIRMING_LOCATION_STAGE finalState -> do
      if os == "IOS" && finalState.props.currentStage == HomeScreen then 
        pure unit 
      else do
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = finalState.props.sourceLat, lon = finalState.props.sourceLong, geoJson = finalState.data.polygonCoordinates, points = finalState.data.nearByPickUpPoints, labelId = getNewIDWithTag "LocateOnMapPin", locationName = fromMaybe "" finalState.props.locateOnMapProps.sourceLocationName, specialZoneMarkerConfig{ labelImage = zoneLabelIcon finalState.props.confirmLocationCategory }}
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingLocation,rideRequestFlow = true, locateOnMapLocation{sourceLat = finalState.props.sourceLat, sourceLng = finalState.props.sourceLong, source = finalState.data.source, sourceAddress = finalState.data.sourceAddress }, locateOnMapProps{ cameraAnimatedToSource = false } }})
        void $ pure $ updateLocalStage ConfirmingLocation
        void $ lift $ lift $ toggleLoader false
      homeScreenFlow
    UPDATE_REFERRAL_CODE referralCode -> do
      referralAppliedStatus <- applyReferralCode referralCode
      case referralAppliedStatus of
        REFERRAL_APPLIED -> do
          void $ pure $ hideKeyboardOnNavigation true
          _ <- UI.successScreen "" ""
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true, referralComponentProps{ stage = ST.APPLIED_POPUP } })
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{isReferred = true, referralComponentProps{ stage = NO_REFERRAL_STAGE }, referral{ referralStatus = NO_REFERRAL, showAddReferralPopup = false }} })
        REFERRAL_INVALID -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props{ referralComponentProps{ isInvalidCode = true } } })
        _ -> pure unit
      homeScreenFlow
    _ -> homeScreenFlow

findEstimates :: HomeScreenState -> FlowBT String Unit
findEstimates updatedState = do
  void $ liftFlowBT $ setMapPadding 0 0 0 0
  logField_ <- lift $ lift $ getLogFields
  if updatedState.data.source == getString STR.CURRENT_LOCATION then do
    fullAddress <- getPlaceName updatedState.props.sourceLat updatedState.props.sourceLong HomeScreenData.dummyLocation true
    case fullAddress of
      Just (PlaceName address) -> do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{ data{ source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing updatedState.props.sourceLat updatedState.props.sourceLong } })
      Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN   
  else
    pure unit
  (GlobalState globalState) <- getState
  let state = globalState.homeScreen
  liftFlowBT $  logEventWithTwoParams logField_ "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
  (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong)
  if (not sourceServiceabilityResp.serviceable) then do
    updateLocalStage SearchLocationModel
    setValueToLocalStore CUSTOMER_LOCATION $ show (getCityNameFromCode sourceServiceabilityResp.city)
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel ,rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false, city = getCityNameFromCode sourceServiceabilityResp.city }})
    homeScreenFlow
    else pure unit
  let currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")
      currentDate =  getCurrentDate ""
      markers = Remote.normalRoute ""
      srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker, primaryText = state.data.source, secondaryText = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, labelImage = zoneLabelIcon state.props.zoneType.sourceTag}
      destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker, primaryText = state.data.destination, labelImage = zoneLabelIcon state.props.zoneType.destinationTag }
  void $ pure $ setCleverTapUserProp [{key : "Latest Search From", value : unsafeToForeign ("lat: " <> (show updatedState.props.sourceLat) <> " long: " <> (show updatedState.props.sourceLong))},
                                      {key : "Latest Search", value : (unsafeToForeign $ currentDate <> " " <> currentTime)}]
  let rideSearchReq = Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress state.props.rideSearchProps.sourceManuallyMoved state.props.rideSearchProps.destManuallyMoved state.props.rideSearchProps.sessionId state.props.isSpecialZone
  (SearchRes rideSearchRes) <- Remote.rideSearchBT rideSearchReq
  void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
  routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong srcMarkerConfig destMarkerConfig "NORMAL" rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "" false getPolylineAnimationConfig) 
  logStatus "ride_search" rideSearchRes
  void $ pure $ deleteValueFromLocalStore TIP_VIEW_DATA
  case rideSearchRes.routeInfo of
    Just (Route response) -> do
      let distance = if response.distance < 1000 then toStringJSON(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
          duration = (show (response.duration / 60)) <> " min"
          Snapped points = response.points
      case head points, last points of
        Just (LatLong source), Just (LatLong dest) -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : state.data.source, address : Nothing, city : Nothing, isSpecialPickUp : Just false }, destination : { lat : dest.lat, lng : dest.lon, place : state.data.destination, address : Nothing, city : Nothing, isSpecialPickUp : Just false } }) } })
        _ , _ -> pure unit
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration, source = state.data.source, sourceAddress = state.data.sourceAddress}})
      let distanceBtwCurrentAndSource = getDistanceBwCordinates state.props.sourceLat state.props.sourceLong state.props.currentLocation.lat state.props.currentLocation.lng
          isDistMoreThanThreshold = state.props.currentLocation.lat /= 0.0 && state.props.currentLocation.lng /= 0.0 && distanceBtwCurrentAndSource > state.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold
      -- Commenting the below condition as it is not required now
      -- if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000) then do
      --   updateLocalStage DistanceOutsideLimits
      --   modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation, findingQuotesProgress = 0.0, isShorterTrip = false}})
      --   homeScreenFlow
      if ( (response.distance < 500  || isDistMoreThanThreshold )&& Arr.all (_ == false )  [ isLocalStageOn PickUpFarFromCurrentLocation , isLocalStageOn ShortDistance]) && not state.props.isSearchCancelled then do 
          let currentStage = if isDistMoreThanThreshold then PickUpFarFromCurrentLocation else ShortDistance
          updateLocalStage currentStage
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = currentStage ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance, isShorterTrip = response.distance < 500, findingQuotesProgress = 0.0}})
          homeScreenFlow
      else pure unit
      pure unit
    Nothing -> pure unit
  void $ liftFlowBT $ setFlowStatusData (FlowStatusData { source : {lat : state.props.sourceLat, lng : state.props.sourceLong, place : state.data.source, address : state.props.locateOnMapProps.sourceLocationName, city : getCityCodeFromCity state.props.city, isSpecialPickUp : Just false}
                                                  , destination : {lat : state.props.destinationLat, lng : state.props.destinationLong, place : state.data.destination, address : Nothing, city : Nothing, isSpecialPickUp : Just false}
                                                  , sourceAddress : state.data.sourceAddress
                                                  , destinationAddress : state.data.destinationAddress 
                                                  , sourceLabelIcon : Just $ zoneLabelIcon state.props.zoneType.sourceTag
                                                  , destLabelIcon : Just $ zoneLabelIcon state.props.zoneType.destinationTag
                                                  , sourceGeoJson : if DS.null state.data.polygonCoordinates then Nothing else Just $ state.data.polygonCoordinates 
                                                  , sourceGates : if null state.data.nearByPickUpPoints then Nothing else Just $ state.data.nearByPickUpPoints })
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, selectedQuote = Nothing, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, findingQuotesProgress = 0.0}, data{nearByDrivers = Nothing}})
  updateLocalStage FindingEstimate
  homeScreenFlow

updateFollower :: Boolean -> Boolean -> Maybe String -> FlowBT String Unit
updateFollower callFollowersApi callInitUi eventType = do
  (GlobalState allState) <- getState
  followers <- getFollowers allState
  let noOfFollowers = Arr.length followers
  setValueToLocalStore TRACKING_DRIVER "False"
  setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
  void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
  let currentFollowerName = getMockFollowerName ""
      currentFollower = if isJust eventType then 
                          Arr.find (\follower -> DS.contains (Pattern currentFollowerName) (fromMaybe "" follower.name) ) followers
                        else Nothing
  modifyScreenState 
    $ FollowRideScreenStateType 
      (\followRideScreen -> 
        followRideScreen{
          data{
            followers = followers, 
            currentFollower = case noOfFollowers == 1, isJust currentFollower of 
                                  true, _ -> Arr.head followers 
                                  _, true -> currentFollower
                                  _, _    -> followRideScreen.data.currentFollower,
            sosStatus = if eventType == Just "SOS_MOCK_DRILL" 
                          then Just Common.MockPending
                          else Nothing,
            currentStage = if isJust eventType && isNothing currentFollower then MockFollowRide
                           else if noOfFollowers > 1 && isNothing currentFollower then PersonList 
                           else FollowingRide},
          props {city = allState.homeScreen.props.city,
          currentUserOnRide = elem allState.homeScreen.props.currentStage [RideAccepted, RideStarted],
          isMock = elem  eventType [Just "SOS_MOCK_DRILL", Just "SOS_MOCK_DRILL_NOTIFY"]}
        })
  when (eventType == Just "SOS_MOCK_DRILL") $ liftFlowBT $ deleteDismisedMockDrills currentFollower
  followRideScreenFlow callInitUi
  where
    getFollowers allState = 
      if callFollowersApi then do
        resp <- lift $ lift $ Remote.getFollowRide ""
        case resp of
          Right (FollowRideRes response) -> 
            pure $ map (\(Followers follower) -> follower) response
          Left err -> pure $ fromMaybe [] allState.homeScreen.data.followers
      else do
        pure $ fromMaybe [] allState.homeScreen.data.followers

followRideScreenFlow :: Boolean -> FlowBT String Unit
followRideScreenFlow callInitUI = do
  hideLoaderFlow
  when callInitUI $ lift $ lift $ initUI
  flow <- UI.followRideScreen
  case flow of
    RESTART_TRACKING -> do
      void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
      followRideScreenFlow false
    GO_TO_HS_FROM_FOLLOW_RIDE -> do
      void $ liftFlowBT $ runEffectFn1 EHC.updateIdMap "FollowsRide"
      pure $ removeAllPolylines ""
      void $ liftFlowBT $ runEffectFn1 clearMap ""
      (GlobalState gs) <- getState
      let currentMapFragment = if os == "IOS" then "CustomerHomeScreenMap" 
                               else if gs.homeScreen.props.currentStage == HomeScreen then "CustomerHomeScreenMap" 
                               else "CustomerHomeScreen"
      void $ liftFlowBT $ reallocateMapFragment (getNewIDWithTag currentMapFragment)
      modifyScreenState $ FollowRideScreenStateType (\_  -> FollowRideScreenData.initData)
      currentFlowStatus
    OPEN_GOOGLE_MAPS_FOLLOW_RIDE state -> do
      case state.data.driverInfoCardState of
        Nothing -> followRideScreenFlow false
        Just ride -> do
          (GetDriverLocationResp resp) <- Remote.getDriverLocationBT ride.rideId
          let sourceLat = (resp^._lat)
              sourceLng = (resp^._lon)
              destLat =  ride.destinationLat
              destLng =  ride.destinationLng
          void $ pure $ openNavigation sourceLat sourceLng "DIRECTION"
          followRideScreenFlow false


getDistanceDiff :: HomeScreenState -> Number -> Number -> FlowBT String Unit
getDistanceDiff state lat lon = do
  distanceInfo <- getDistanceInfo (state.data.savedLocations) "" (lat) (lon) (fromMaybe "" state.data.saveFavouriteCard.selectedItem.placeId)
  case distanceInfo.locExistsAs of
    "" ->  modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{isSaveFavourite = true}})
    _  -> do
            void $ pure $ toast  (getString STR.ALREADY_EXISTS)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{saveFavouriteCard{selectedItem = locationListStateObj}}})
  homeScreenFlow


fetchLatAndLong :: HomeScreenState -> String -> FlowBT String Unit
fetchLatAndLong state tag  =
  case state.data.saveFavouriteCard.selectedItem.placeId of
    Just placeID -> do
      (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (state.data.saveFavouriteCard.selectedItem.title <> ", " <> state.data.saveFavouriteCard.selectedItem.subTitle) (Just placeID) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lat) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lon) state.data.saveFavouriteCard.selectedItem
      let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
      let (LatLong placeLatLong) = (placeName.location)
      resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents)
      void $ FlowCache.updateAndFetchSavedLocations true
      pure unit
    Nothing -> pure unit

rideSearchFlow :: String -> FlowBT String Unit
rideSearchFlow flowType = do
  void $ pure $ hideKeyboardOnNavigation true
  logField_ <- lift $ lift $ getLogFields
  (GlobalState homeScreenModifiedState) <- getState
  void $ liftFlowBT $ setMapPadding 0 0 0 0
  let finalState = homeScreenModifiedState.homeScreen -- bothLocationChangedState{props{isSrcServiceable =homeScreenModifiedState.homeScreen.props.isSrcServiceable, isDestServiceable = homeScreenModifiedState.homeScreen.props.isDestServiceable, isRideServiceable = homeScreenModifiedState.homeScreen.props.isRideServiceable }}
  if (finalState.props.sourceLat /= 0.0 && finalState.props.sourceLong /= 0.0) && (finalState.props.destinationLat /= 0.0 && finalState.props.destinationLong /= 0.0) && (finalState.data.source /= "") && (finalState.data.destination /= "")
    then do
      let searchWithoutConfirmPickup = any (_ == finalState.props.rideSearchProps.sourceSelectType) [ST.MAP, ST.RETRY_SEARCH, ST.REPEAT_RIDE] || (finalState.props.rideSearchProps.sourceSelectType == ST.FAVOURITE && not finalState.props.isSpecialZone)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{flowWithoutOffers = flowWithoutOffers WithoutOffers}})
      case searchWithoutConfirmPickup of
        false -> do
          pure $ removeAllPolylines ""
          liftFlowBT $ setMapPadding 0 0 0 0
          if finalState.data.source == (getString STR.CURRENT_LOCATION) then void $ pure $ currentPosition "" else pure unit
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = GoToConfirmLocation}})
          void $ pure $ updateLocalStage GoToConfirmLocation
        true -> do
          let currentTime = (convertUTCtoISC (getCurrentUTC "") "h:mm:ss A")
              currentDate =  getCurrentDate ""
          void $ pure $ setCleverTapUserProp [{key : "Latest Search From", value : unsafeToForeign ("lat: " <> (show finalState.props.sourceLat) <> " long: " <> (show finalState.props.sourceLong))},
                                              {key : "Latest Search", value : unsafeToForeign (currentDate <> " " <> currentTime)}]
          let rideSearchReq = Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong finalState.data.sourceAddress finalState.data.destinationAddress finalState.props.rideSearchProps.sourceManuallyMoved finalState.props.rideSearchProps.destManuallyMoved finalState.props.rideSearchProps.sessionId finalState.props.isSpecialZone
          (SearchRes rideSearchRes) <- Remote.rideSearchBT rideSearchReq
          void $ pure $ setValueToLocalStore STARTED_ESTIMATE_SEARCH "FALSE"
          void $ pure $ deleteValueFromLocalStore TIP_VIEW_DATA
          void $ liftFlowBT $ setFlowStatusData 
            ( FlowStatusData 
              { source : 
                { lat : finalState.props.sourceLat
                , lng : finalState.props.sourceLong
                , place : finalState.data.source
                , address : Nothing
                , city : getCityCodeFromCity finalState.props.city
                , isSpecialPickUp : Just false
                }
              , destination : 
                { lat : finalState.props.destinationLat
                , lng : finalState.props.destinationLong
                , place : finalState.data.destination
                , address : Nothing
                , city : Nothing
                , isSpecialPickUp : Just false
                }
              , sourceAddress : finalState.data.sourceAddress
              , destinationAddress : finalState.data.destinationAddress 
              , sourceLabelIcon : Just $ zoneLabelIcon finalState.props.zoneType.sourceTag
              , destLabelIcon : Just $ zoneLabelIcon finalState.props.zoneType.destinationTag
              , sourceGeoJson : finalState.props.locateOnMapProps.sourceGeoJson
              , sourceGates : finalState.props.locateOnMapProps.sourceGates
              }
            )
          case finalState.props.currentStage of
            TryAgain -> do
              when (finalState.props.customerTip.enableTips) $ do
                cancelEstimate finalState.props.estimateId
              void $ pure $ updateLocalStage TryAgain
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true}, data{nearByDrivers = Nothing}})
            _        -> do
              let sourceSpecialTagIcon = zoneLabelIcon finalState.props.zoneType.sourceTag
                  destSpecialTagIcon = zoneLabelIcon finalState.props.zoneType.destinationTag
                  markers = Remote.normalRoute ""
                  srcMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.srcMarker, primaryText = finalState.data.source }
                  destMarkerConfig = defaultMarkerConfig{ pointerIcon = markers.destMarker, primaryText = finalState.data.destination }
              routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong srcMarkerConfig destMarkerConfig "NORMAL" rideSearchRes.routeInfo "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon false getPolylineAnimationConfig) 
              case rideSearchRes.routeInfo of
                Just (Route response) -> do
                  let distance = if response.distance < 1000 then toStringJSON(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
                      duration = (show (response.duration / 60)) <> " min"
                      Snapped points = response.points
                  case head points, last points of
                    Just (LatLong source), Just (LatLong dest) -> do
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : finalState.data.source, address : Nothing, city : Nothing, isSpecialPickUp : Just false }, destination : { lat : dest.lat, lng : dest.lon, place : finalState.data.destination, address : Nothing, city : Nothing, isSpecialPickUp : Just false } }) } })
                    _ , _ -> pure unit
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration,source = finalState.data.source, sourceAddress = finalState.data.sourceAddress }})
                  let distanceBtwCurrentAndSource = getDistanceBwCordinates finalState.props.sourceLat finalState.props.sourceLong finalState.props.currentLocation.lat finalState.props.currentLocation.lng
                      isDistMoreThanThreshold = finalState.props.currentLocation.lat /= 0.0 && finalState.props.currentLocation.lng /= 0.0 && (distanceBtwCurrentAndSource > finalState.data.config.mapConfig.locateOnMapConfig.pickUpToSourceThreshold) && flowType == "NORMAL_FLOW"
                  if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000 )then do
                    void $ pure $ updateLocalStage DistanceOutsideLimits
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
                    else if ( (response.distance < 500  || isDistMoreThanThreshold )&& Arr.all (_ == false ) [ isLocalStageOn PickUpFarFromCurrentLocation , isLocalStageOn ShortDistance]) then do 
                      let currentStage = if isDistMoreThanThreshold then PickUpFarFromCurrentLocation else ShortDistance
                      void $ pure $ updateLocalStage currentStage
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = currentStage ,rideRequestFlow = true, isSearchLocation = SearchLocation, isShorterTrip = response.distance < 500 , distance = response.distance, findingQuotesProgress = 0.0}})
                    else do
                      if flowType == "REPEAT_RIDE_FLOW" then liftFlowBT $ logEventWithParams logField_ "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                      void $ pure $ updateLocalStage FindingEstimate
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{isRepeatRide = (flowType == "REPEAT_RIDE_FLOW"), searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, isShorterTrip = false}, data {source = finalState.data.source, sourceAddress = finalState.data.sourceAddress, nearByDrivers = Nothing}})
                  void $ lift $ lift $ toggleLoader false

                Nothing -> pure unit
    else
      let updatedLocationList = updateLocListWithDistance finalState.data.destinationSuggestions finalState.props.sourceLat finalState.props.sourceLong true finalState.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
      in modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data {locationList = updatedLocationList}, props{isSource = Just false, isRideServiceable = true, isSrcServiceable = true, isDestServiceable = true, currentStage = SearchLocationModel}})
  homeScreenFlow

getfeedbackReq :: HomeScreenState -> FeedbackReq
getfeedbackReq state = (Remote.makeFeedBackReq (state.data.rideRatingState.rating) (state.data.rideRatingState.rideId) (state.data.rideRatingState.feedback) (state.data.ratingViewState.wasOfferedAssistance))

dummyAddressGeometry :: AddressGeometry
dummyAddressGeometry = AddressGeometry {
  geometry : Geometry{
    location : LocationS{
      lat: 0.0,
      lng: 0.0
    }
  }
 }

getFinalAmount :: RideBookingRes -> Int
getFinalAmount (RideBookingRes resp) =
    let rideList = resp.rideList
        (RideAPIEntity ride) = (fromMaybe dummyRideAPIEntity (rideList !! 0))
    in INT.round $ fromMaybe 0.0 $ fromString (show (fromMaybe 0 ride.computedPrice))

tripDetailsScreenFlow ::  FlowBT String Unit
tripDetailsScreenFlow = do
  (GlobalState globalState) <- getState
  logField_ <- lift $ lift $ getLogFields
  expiryTime <- pure $ getExpiryTime globalState.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{ canConnectWithDriver = (expiryTime <= 86400)}}) -- expiryTime < 24hrs or 86400 seconds
  flow <- UI.tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> flowRouter HelpAndSupportScreenFlow
    GO_TO_RIDES -> myRidesScreenFlow 
    ON_SUBMIT state -> do
      liftFlowBT $ logEventWithParams logField_ "ny_user_issue_reported" "Description" (state.data.message)
      let bookingId = if state.props.fromMyRides == Home then Just globalState.homeScreen.data.rideRatingState.bookingId else Just state.data.selectedItem.bookingId
          isNightSafety = Just $ globalState.homeScreen.props.nightSafetyFlow
      void $ Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing bookingId state.data.message state.data.message isNightSafety)
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = true, fromMyRides = state.props.fromMyRides}})
      when globalState.homeScreen.props.nightSafetyFlow $ do modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {data {ratingViewState {issueFacedView = false, selectedYesNoButton = -1}}, props{nightSafetyFlow = false}})
      tripDetailsScreenFlow 
    GO_TO_INVOICE updatedState -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_invoice_clicked" $ [ { key : "Pickup", value : unsafeToForeign updatedState.data.selectedItem.source},
                                                                                                          { key : "Destination", value : unsafeToForeign updatedState.data.selectedItem.destination},
                                                                                                          { key : "Fare", value : unsafeToForeign updatedState.data.selectedItem.totalAmount},
                                                                                                          { key : "Status", value : unsafeToForeign updatedState.data.selectedItem.status},
                                                                                                          { key : "Ride completion timestamp", value : unsafeToForeign updatedState.data.selectedItem.rideEndTime},
                                                                                                          { key : "Rating", value : (unsafeToForeign $ updatedState.data.selectedItem.rating)}]
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen = false},data{totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem}})
      invoiceScreenFlow
    GO_TO_HOME state -> do
      if state.props.fromMyRides == Home then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
        updateLocalStage HomeScreen
        else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      config <- getAppConfigFlowBT appConfig
      pure $ toast (getString STR.REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON)
      void $ Remote.sendIssueBT (Remote.makeSendIssueReq  (Just config.appData.supportMail) (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" $ Just false)
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = updatedState.props.fromMyRides}})
      tripDetailsScreenFlow 
    GET_CATEGORIES_LIST updatedState -> do 
      let language = fetchLanguage $ getLanguageLocale languageKey
          categoryOrder = ["LOST_AND_FOUND", "DRIVER_RELATED", "RIDE_RELATED", "APP_RELATED"]
          compareByOrder a b = compare (fromMaybe (length categoryOrder) $ elemIndex a.categoryAction categoryOrder) (fromMaybe (length categoryOrder) $ elemIndex b.categoryAction categoryOrder)
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let unsortedCatagory = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : catObj.label, categoryImageUrl : catObj.logoUrl}) response.categories
          categories' = sortBy compareByOrder unsortedCatagory
      modifyScreenState $ TripDetailsScreenStateType (\helpAndSupportScreen -> updatedState { data {categories = categories' }, props { fromMyRides = updatedState.props.fromMyRides} } )
      tripDetailsScreenFlow 
    GO_TO_ISSUE_CHAT_SCREEN updatedState selectedCategory -> do
      let language = fetchLanguage $ getLanguageLocale languageKey
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" ""
      let options' = mapWithIndex (\index (Option optionObj) -> optionObj{ option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option)}) getOptionsRes.options
          messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) "Bot" (getCurrentUTC "") "Text" (500 * (index + 1)))getOptionsRes.messages
          chats' = map (\(Message currMessage) -> Chat {
                      chatId : currMessage.id,
                      chatType : "IssueMessage",
                      timestamp : getCurrentUTC ""
                    }) getOptionsRes.messages
          categoryName = getTitle selectedCategory.categoryAction
          merchantExoPhone' = if updatedState.data.selectedItem.merchantExoPhone == "" then Nothing else Just updatedState.data.selectedItem.merchantExoPhone
      modifyScreenState $ ReportIssueChatScreenStateType (\ reportIssueChatState -> reportIssueChatState { data { merchantExoPhone = merchantExoPhone',  selectedRide = Just updatedState.data.selectedItem, entryPoint = ReportIssueChatScreenData.TripDetailsScreenEntry, chats = chats', categoryName = categoryName, categoryId = selectedCategory.categoryId, options = options', chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{messages = messages'} }})
      flowRouter IssueReportChatScreenFlow


invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  flow <- UI.invoiceScreen
  (GlobalState newState) <- getState
  case flow of
    InvoiceScreenOutput.GoBack -> do 
      tripDetailsScreenFlow 
    InvoiceScreenOutput.GoToHome -> homeScreenFlow
  pure unit

contactUsScreenFlow :: FlowBT String Unit
contactUsScreenFlow = do
  flow <- UI.contactUsScreen
  case flow of
    GO_TO_HOME_FROM_CONTACT state -> do
      void $ Remote.sendIssueBT (Remote.makeSendIssueReq (Just state.data.email) Nothing state.data.description state.data.subject $ Just false)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
  pure unit


      

flowRouter :: FlowState -> FlowBT String Unit
flowRouter flowState = 
  case flowState of 
    HelpAndSupportScreenFlow -> do 
      nextFlow <- UI.helpAndSupportScreen
      flowRouter nextFlow
    IssueReportChatScreenFlow -> do 
      nextFlow <- UI.reportIssueChatScreen 
      flowRouter nextFlow
    RideSelectionScreenFlow -> do 
      nextFlow <- UI.rideSelection
      flowRouter nextFlow
    HomeScreenFlow -> homeScreenFlow
    ActivateSafetyScreenFlow -> activateSafetyScreenFlow
    TripDetailsScreenFlow -> tripDetailsScreenFlow
    ContactUsScreenFlow -> contactUsScreenFlow
    MyRidesScreenFlow -> myRidesScreenFlow


myRidesScreenFlow :: FlowBT String Unit
myRidesScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState globalState) <- getState
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data{isSrcServiceable = globalState.homeScreen.props.isSrcServiceable}})
  flow <- UI.myRidesScreen
  case flow of
    REFRESH state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state{props{fromNavBar =  state.props.fromNavBar }})
      myRidesScreenFlow
    TRIP_DETAILS state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_view_details" $ [ { key : "Pickup", value : unsafeToForeign state.data.selectedItem.source},
                                                                                                                  { key : "Destination", value : unsafeToForeign state.data.selectedItem.destination},
                                                                                                                  { key : "Fare", value : unsafeToForeign state.data.selectedItem.totalAmount},
                                                                                                                  { key : "Status", value : unsafeToForeign state.data.selectedItem.status},
                                                                                                                  { key : if state.data.selectedItem.status == "CANCELLED" then "Time" else "Ride completion timestamp",
                                                                                                                    value : unsafeToForeign $ if state.data.selectedItem.status == "CANCELLED" then state.data.selectedItem.time else state.data.selectedItem.rideEndTime},
                                                                                                                  { key : "Rating", value : (unsafeToForeign $ state.data.selectedItem.rating)}]
      modifyScreenState $ TripDetailsScreenStateType (\tripDetails -> tripDetails{data{vehicleVariant = state.data.selectedItem.vehicleVariant}, props{fromMyRides = MyRides}})
      tripDetailsScreenFlow 
    LOADER_OUTPUT state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state{data{offsetValue = state.data.offsetValue + 8}, props {fromNavBar = state.props.fromNavBar}})
      myRidesScreenFlow 
    BOOK_RIDE -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    GO_TO_NAV_BAR -> homeScreenFlow
    GO_TO_HELP_SCREEN -> flowRouter HelpAndSupportScreenFlow
    REPEAT_RIDE_FLOW state -> do
      let trip = getTripFromRideHistory state
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{ rideHistoryTrip = Just trip, settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
                                liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_lang_selected" $[{ key : "Previous language", value : unsafeToForeign $ getLanguageLocale languageKey},
                                                                                                                          { key : "New language", value : unsafeToForeign state.props.selectedLanguage}]
                                void $ pure $ setLanguageLocale state.props.selectedLanguage
                                void $ lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_lang_selec" "language" (state.props.selectedLanguage)
                                let langVal =  case (state.props.selectedLanguage) of
                                                                                     "HI_IN" -> "HINDI"
                                                                                     "EN_US" -> "ENGLISH"
                                                                                     "KN_IN" -> "KANNADA"
                                                                                     "BN_IN" -> "BENGALI"
                                                                                     "ML_IN" -> "MALAYALAM"
                                                                                     _ -> state.data.config.defaultLanguage
                                void $ pure $ setCleverTapUserProp [{key : "Preferred Language", value : unsafeToForeign langVal}]
                                resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
                                modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> SelectLanguageScreenData.initData) 
                                modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data{categories = []}})
                                modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen {data {categories = []}, props {needIssueListApiCall = true}})
                                homeScreenFlow
    GO_TO_HOME_SCREEN     -> homeScreenFlow

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
  flow <- UI.emergencyContactsScreen
  case flow of
    POST_CONTACTS state shouldGoToSafetyScreen -> do
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq state.data.selectedContacts
      when (not shouldGoToSafetyScreen)
        $ if state.props.showInfoPopUp then
            pure $ toast $ getString STR.CONTACT_REMOVED_SUCCESSFULLY
          else
            pure $ toast $ getString STR.EMERGENCY_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $ EmergencyContactsScreenStateType (\_ -> state { data{emergencyContactsList = state.data.selectedContacts}, props { showInfoPopUp = false} })
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.selectedContacts }, props{setupStage = ST.SetDefaultEmergencyContacts, showShimmer = true} })
      (GlobalState globalState) <- getState
      case globalState.nammaSafetyScreen.data.hasCompletedSafetySetup, shouldGoToSafetyScreen, state.props.fromSosFlow of 
        _, _, true -> activateSafetyScreenFlow
        false, true, _ -> setupSafetySettingsFlow
        true, true, _ -> safetySettingsFlow
        _, _, _ -> emergencyScreenFlow

    GET_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = getDefaultPriorityList $ map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true,
          enableForFollowing: fromMaybe false item.enableForFollowing,
          enableForShareRide: fromMaybe false item.enableForShareRide,
          onRide: fromMaybe false item.onRide,
          priority: fromMaybe 1 item.priority
        }) res.defaultEmergencyNumbers
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{data{emergencyContactsList = contacts}})
      emergencyScreenFlow
    REFRESH_EMERGECY_CONTACTS_SCREEN state -> do
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state)
      emergencyScreenFlow

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  flow <- UI.aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

permissionScreenFlow :: FlowBT String Unit
permissionScreenFlow = do
  void $ pure $ hideKeyboardOnNavigation true
  flow <- UI.permissionScreen
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case flow of
    REFRESH_INTERNET -> do
      if internetCondition then
          currentFlowStatus
        else do
          modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = INTERNET_ACTION})
          permissionScreenFlow 
    TURN_ON_GPS -> if not internetCondition then do  
                      modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = INTERNET_ACTION})
                      permissionScreenFlow
                    else do
                      setValueToLocalStore PERMISSION_POPUP_TIRGGERED "true"
                      currentFlowStatus
    TURN_ON_INTERNET -> case (getValueToLocalStore USER_NAME == "__failed") of
                            true -> pure unit
                            _ -> if os == "IOS" && not permissionConditionB then do 
                                  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DENIED})
                                  permissionScreenFlow
                                 else if not (permissionConditionA && permissionConditionB) then do 
                                  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen {stage = LOCATION_DISABLED})
                                  permissionScreenFlow 
                                 else currentFlowStatus

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  disabilityListT <- updateDisabilityList "My_Profile_Screen"
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{data{disabilityOptions{disabilityOptionList = disabilityListT }, editedDisabilityOptions{disabilityOptionList = disabilityListT}}})
  flow <- UI.myProfileScreen
  case flow of
    UPDATE_USER_PROFILE state -> do
      void $ pure $ toggleBtnLoader "" false
      void $ pure $ spy "profile_updated_state" state
      let stringName = seperateByWhiteSpaces(state.data.editedName)
          name = split (Pattern " ") stringName
          nameLength = length name
          gender = getGenderValue state.data.editedGender
          email = if state.data.editedEmailId == state.data.emailId || (state.data.editedEmailId == Just "") then Nothing else state.data.editedEmailId
          disability = case state.data.editedDisabilityOptions.selectedDisability of 
            Just disability -> if (state.data.editedDisabilityOptions.activeIndex == 1) 
                                  then Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.editedDisabilityOptions.otherDisabilityReason))
                                  else Nothing
            _ -> Nothing
          hasDisability = if state.props.changeAccessibility then (Just (isJust disability)) else Nothing
      resp <- if nameLength > 2 then
                lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender hasDisability disability)
                else if nameLength == 2 then
                  lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender hasDisability disability)
                  else if nameLength == 1 then
                    lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender hasDisability disability)
                    else
                      lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender hasDisability disability)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME stringName
          let tag = case disability of
                      Just (Disability value) -> value.tag
                      Nothing -> ""
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{ disability = Just {id : "", tag : tag, description : "" }}})
          case gender of
            Just gender -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just gender}}, props{isBanner = false}})
            _ -> pure unit
          case email of
            Just email -> do
              setValueToLocalStore USER_EMAIL email
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just email}}})
            _ -> pure unit
          modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData)
          myProfileScreenFlow
        Left err -> do
          let errResponse = err.response
          let codeMessage = decodeError errResponse.errorMessage "errorCode"
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              pure $ setText (getNewIDWithTag "EmailEditText") ""
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{props{isEmailValid = false, updateProfile = true}, data{emailErrorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender}})
            _ -> pure $ toast (getString STR.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          myProfileScreenFlow
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

savedLocationFlow :: FlowBT String Unit
savedLocationFlow = do
  void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
  flow <- UI.savedLocationScreen
  (SavedLocationsListRes savedLocationResp )<- FlowCache.updateAndFetchSavedLocationsBT SavedLocationReq false
  case flow of
    ADD_NEW_LOCATION state-> do
      (GlobalState newState) <- getState
      resp <- lift $ lift $ getRecentSearches newState.addNewAddressScreen
      let currentGeoHash = runFn3 encodeGeohash (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT) (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON) newState.homeScreen.data.config.suggestedTripsAndLocationConfig.geohashPrecision
          suggestionsMap = getSuggestionsMapFromLocal FunctionCall
          suggestionsObject = (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations currentGeoHash suggestionsMap newState.homeScreen.data.config.suggestedTripsAndLocationConfig.geohashLimitForMap))
          suggestedDestinationsArr = (differenceOfLocationLists suggestionsObject.destinationSuggestions (AddNewAddress.getSavedLocations savedLocationResp.list))
      let recents = map
                    (\item -> item{postfixImageUrl = "", postfixImageVisibility = false}) suggestedDestinationsArr
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{savedLocations = getSavedLocationForAddNewAddressScreen state.data.savedLocations ,locationList = recents ,placeName = "",address = "",addressSavedAs="", recentSearchs{predictionArray = recents},savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)}, props{showSavePlaceView = false, editLocation = false, isLocationServiceable = true, isSearchedLocationServiceable = true, isLocateOnMap = false, fromHome = false, fromScreen = Screen.getScreen Screen.SAVED_LOCATION_SCREEN}})
      case (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "HOME" ""), (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "WORK" "") of
          false   , false    -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = (Just 2), selectedTag = (Just OTHER_TAG) }, props{editSavedLocation = false}})
          _ , _ -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = Nothing, selectedTag = Nothing}, props{editSavedLocation = false}})
      addNewAddressScreenFlow "dummy"
    DELETE_LOCATION tagName -> do
      resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim tagName))
      void $ FlowCache.updateAndFetchSavedLocations true
      pure $ toast (getString STR.FAVOURITE_REMOVED_SUCCESSFULLY)
      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      savedLocationFlow
    EDIT_LOCATION cardState -> do
      (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq (fromMaybe 0.0 cardState.lat) (fromMaybe 0.0 cardState.lon))
      let savedLocs = AddNewAddress.getSavedLocations savedLocationResp.list
      updateSavedLocations savedLocs 
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
        addNewAddressScreen
          { props
              { tagExists = false
              , showSavePlaceView = true
              , editLocation= true
              , editSavedLocation = true
              , isBtnActive = false
              , isLocateOnMap = false
              , isLocationServiceable = (serviceabilityRes.serviceable)
              , fromHome = false
              , fromScreen = Screen.getScreen Screen.SAVED_LOCATION_SCREEN
              }
          , data
              { existsAs = ""
              , selectedTag = getCardType (fromMaybe "" (cardState.cardType))
              , placeName = cardState.tagName
              , savedLocations = savedLocs
              , address = cardState.savedLocation
              , addressSavedAs = cardState.tagName
              , selectedItem
                  { title = (fromMaybe "" ((split (Pattern ",") (cardState.savedLocation)) !! 0))
                  , description = cardState.savedLocation
                  , lat = cardState.lat
                  , lon = cardState.lon
                  , placeId = cardState.placeId
                  , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (cardState.savedLocation))) + 2) (cardState.savedLocation))
                  }
              , savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)
              , lat = fromMaybe 0.0 cardState.lat
              , lon = fromMaybe 0.0 cardState.lon
              , latSelectedFromMap = fromMaybe 0.0 cardState.lat
              , lonSelectedFromMap = fromMaybe 0.0 cardState.lon
              , locSelectedFromMap = ""
              , activeIndex = case (getCardType (fromMaybe "" (cardState.cardType))) of
                                Just card -> case card of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                OTHER_TAG-> Just 2
                                Nothing   -> Nothing}})

      addNewAddressScreenFlow "edit Location"

    GO_BACK_FROM_SAVED_LOCATION -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag if os == "IOS" then "CustomerHomeScreenMap" else "CustomerHomeScreen")
      homeScreenFlow
  pure unit

addNewAddressScreenFlow ::String -> FlowBT String Unit
addNewAddressScreenFlow input = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.addNewAddressScreen
  case flow of
    SEARCH_ADDRESS input state -> do
      (GlobalState newState) <- getState
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input newState.homeScreen.props.sourceLat newState.homeScreen.props.sourceLong (EHC.getMapsLanguageFormat (getLanguageLocale languageKey) ) "" defaultCityConfig.geoCodeConfig Nothing "")
      let sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions
          predictionList = AddNewAddress.getLocationList sortedByDistanceList
          recentLists = state.data.recentSearchs.predictionArray
          filteredRecentsList = filterRecentSearches recentLists predictionList
          filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> state{  data  { locationList = map
                                                                                                                (\item -> item{ postfixImageVisibility = (not (checkPrediction item state.data.savedLocations))
                                                                                                                              , postfixImageUrl = if (checkPrediction item state.data.savedLocations) then "" else fetchImage FF_ASSET "ny_ic_fav_red"
                                                                                                                              , isClickable = (checkPrediction item state.data.savedLocations)
                                                                                                                              , alpha = if (checkPrediction item state.data.savedLocations) then 1.0 else 0.5 }) (filteredPredictionList <> filteredRecentsList) }})
      addNewAddressScreenFlow ""

    ADD_LOCATION state -> do
      if (state.props.editSavedLocation) then do
        void $ Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim state.data.placeName))
        pure unit
      else pure unit
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_favourite_added" $ [{ key : "Address", value : unsafeToForeign state.data.address},
                                                                                                                { key : "Tag", value : unsafeToForeign state.data.selectedTag}]
      (GetPlaceNameResp sourcePlace) <- getPlaceNameResp (state.data.selectedItem.title <> ", " <> state.data.selectedItem.subTitle) state.data.selectedItem.placeId (fromMaybe 0.0 state.data.selectedItem.lat) (fromMaybe 0.0 state.data.selectedItem.lon)  state.data.selectedItem
      let source = state.data.selectedItem.description
          (PlaceName sourceAddressGeometry) = (fromMaybe HomeScreenData.dummyLocationName (sourcePlace!!0))
          (LatLong sourceLocation) = (sourceAddressGeometry.location)
          lat = sourceLocation.lat
          lng = sourceLocation.lon
          newstate = state { data { lat =lat, lon=lng, selectedItem
                                                        { description = source
                                                        , lat = Just lat
                                                        , lon = Just lng
                                                        }
                                    , addressComponents = sourceAddressGeometry.addressComponents
                                    }
                              }

      resp <- Remote.addSavedLocationBT (AddNewAddress.encodeAddressDescription newstate)
      void $ FlowCache.updateAndFetchSavedLocations true
      if state.props.editSavedLocation then pure $ toast (getString STR.FAVOURITE_UPDATED_SUCCESSFULLY)
        else pure $ toast (getString STR.FAVOURITE_ADDED_SUCCESSFULLY)

      setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      if state.props.fromHome || state.props.fromScreen == (Screen.getScreen Screen.HOME_SCREEN) then do
        (GlobalState globalState) <- getState
        (SavedLocationsListRes savedLocationResp )<- FlowCache.updateAndFetchSavedLocations false
        let updatedLocationList = getUpdatedLocationList globalState.homeScreen.data.locationList state.data.selectedItem.placeId
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
                                                    homeScreen
                                                      { data
                                                          { settingSideBar {opened = SettingSideBarController.CLOSED}
                                                          , locationList = updatedLocationList
                                                          , savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list)
                                                          }
                                                        } )
        when (not state.props.fromHome) $ do
          void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag if os == "IOS" then "CustomerHomeScreenMap" else "CustomerHomeScreen")
        homeScreenFlow
      else savedLocationFlow

    UPDATE_LOCATION_NAME_ADDRESS state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let isServiceable = sourceServiceabilityResp.serviceable
          (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
          pickUpPoints =  mapSpecialZoneGates srcSpecialLocation.gatesInfo
          geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
          gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      if not (DS.null geoJson) && not (null pickUpPoints) && (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) then do
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data { polygonCoordinates = geoJson
                                                                                                             , nearByPickUpPoints = pickUpPoints
                                                                                                             }
                                                                                                      , props{ isSpecialZone = not (DS.null geoJson) 
                                                                                                             , isServiceable = isServiceable
                                                                                                             }
                                                                                                      })
        void $ pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = geoJson, points = pickUpPoints, labelId = getNewIDWithTag "AddAddressPin", locationName = srcSpecialLocation.locationName }
        addNewAddressScreenFlow ""
      else do
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName address) -> do 
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data  { locSelectedFromMap = address.formattedAddress
                                                                                                                  , latSelectedFromMap = lat
                                                                                                                  , lonSelectedFromMap = lon
                                                                                                                  }
                                                                                                          , props { isServiceable = isServiceable }
                                                                                                          } )
          Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
        addNewAddressScreenFlow ""
    GO_TO_FAVOURITES -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      savedLocationFlow

    CHECK_LOCATION_SERVICEABILITY state locItemType-> do
      void $ pure $ spy "Inside CHECK_LOCATION_SERVICEABILITY" state
      let item  = state.data.selectedItem
      if item.locationItemType /= Just RECENTS then do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (item.title <> ", " <> item.subTitle) item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) item
        let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
        let (LatLong placeLatLong) = (placeName.location)
        (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon)
        case (serviceabilityRes.serviceable) , (state.props.editLocation) of
          true , isEditLocation ->  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if isEditLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case isEditLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = isEditLocation
                  }
                } )
          _    ,  _     -> do
            pure $ setText (getNewIDWithTag "SavedLocationEditText") item.description
            void $ pure $ hideKeyboardOnNavigation true
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { isSearchedLocationServiceable = false
                  , isLocateOnMap = false
                  , showSavePlaceView = false
                  }
                , data
                  { recentSearchs { predictionArray = state.data.recentSearchs.predictionArray }
                  , address = item.description
                  }
                } )
            addNewAddressScreenFlow ""
        updateDistanceInfo state (Just placeLatLong.lat) (Just placeLatLong.lon)
      else do
        let recentItem = (fromMaybe dummyLocationListItemState ( (filter (\ ( recent) -> (recent.placeId) == (item.placeId))(state.data.recentSearchs.predictionArray)) !! 0))
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if state.props.editLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case state.props.editLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = state.props.editLocation
                  }
                } )
        updateDistanceInfo state recentItem.lat recentItem.lon
    GO_TO_HOME_SCREEN_FLOW -> do
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow
    GO_TO_SEARCH_LOC_SCREEN -> do 
      void $ lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "SearchLocationScreenMap")
      searchLocationFlow
  pure unit



referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { referralCode = getValueToLocalStore CUSTOMER_REFERRAL_CODE })
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      referralAppliedStatus <- applyReferralCode referralCode
      case referralAppliedStatus of
        REFERRAL_APPLIED -> do
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true, referralComponentProps{ stage = ST.APPLIED_POPUP } })
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{isReferred = true} })
        REFERRAL_INVALID -> modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { isInvalidCode = true, referralComponentProps{ isInvalidCode = true } })
        _ -> pure unit
      referralScreenFlow
    BACK_TO_HOME -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> ReferralScreen.initData)
      void $ lift $ lift $ liftFlow $ adjustViewWithKeyboard "true"
      homeScreenFlow

isForLostAndFound :: Boolean
isForLostAndFound = true



checkAndUpdateSavedLocations :: HomeScreenState -> FlowBT String Unit
checkAndUpdateSavedLocations state = do
  when ((getValueToLocalStore RELOAD_SAVED_LOCATION == "true") || state.props.currentStage == HomeScreen) $ do
    (SavedLocationsListRes savedLocationResp )<- FlowCache.updateAndFetchSavedLocations false
    fetchAndModifyLocationLists $ AddNewAddress.getSavedLocations savedLocationResp.list
    pure unit
  pure unit

addLocationToRecents :: LocationListItemState -> HomeScreenState -> Boolean -> Boolean -> FlowBT String Unit
addLocationToRecents item state srcServiceable destServiceable = do
  (GlobalState currentState) <- getState
  let serviceable = if (state.props.isSource == Just false) then destServiceable else srcServiceable
      lat = if (state.props.isSource == Just false) then state.props.destinationLat else state.props.sourceLat
      lon = if (state.props.isSource == Just false) then state.props.destinationLong else state.props.sourceLong
      latLong = case item.locationItemType of 
                  Just PREDICTION -> {latitude : lat , longitude : lon }
                  _ -> {latitude : (fromMaybe 0.0 item.lat) , longitude : (fromMaybe 0.0 item.lon) }
  saveToRecents item latLong.latitude latLong.longitude serviceable
  when (state.props.isSource == Just false) $ do
    setSuggestionsMapInLocal item currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong latLong.latitude latLong.longitude serviceable state.data.config
  pure unit

saveToRecents :: LocationListItemState -> Number -> Number -> Boolean -> FlowBT String Unit
saveToRecents item lat lon serviceability = do
  (GlobalState currentState) <- getState
  recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
  when (serviceability && lat /= 0.0 && lon /= 0.0) $ do
    modifyScreenState $ GlobalPropsType (\globalProps -> globalProps{recentSearches = addToRecentSearches item{lat = Just lat, lon = Just lon, locationScore = Just 0.0} recentPredictionsObject.predictionArray})
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data { recentSearchs { predictionArray = addToRecentSearches item{lat = Just lat, lon = Just lon, locationScore = Just 0.0} recentPredictionsObject.predictionArray}}})
    (GlobalState modifiedState) <- getState
    void $ pure $ saveObject "RECENT_SEARCHES" modifiedState.homeScreen.data.recentSearchs
    pure unit

setSuggestionsMapInLocal :: LocationListItemState -> Number -> Number -> Number -> Number -> Boolean -> AppConfig -> FlowBT String Unit
setSuggestionsMapInLocal item srcLat srcLon lat lon serviceability config = do
  when (serviceability && lat /= 0.0 && lon /= 0.0) $ do
    let currentSourceGeohash = runFn3 encodeGeohash srcLat srcLon config.suggestedTripsAndLocationConfig.geohashPrecision
        destinationWithLatLong = item{lat = Just lat, lon = Just lon}
        currentMap = getSuggestionsMapFromLocal FunctionCall
        updatedMap = addOrUpdateSuggestedDestination currentSourceGeohash destinationWithLatLong currentMap config.suggestedTripsAndLocationConfig
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{suggestionsData {suggestionsMap=updatedMap}}})
    void $ pure $ setSuggestionsMap updatedMap
    pure unit


fetchAndModifyLocationLists :: Array (LocationListItemState) -> FlowBT String Unit
fetchAndModifyLocationLists savedLocationResp = do 
    (GlobalState currentState) <- getState
    let state = currentState.homeScreen
        suggestionsConfig = state.data.config.suggestedTripsAndLocationConfig
    recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
    let {savedLocationsWithOtherTag, recents, suggestionsMap, tripArrWithNeighbors, updateFavIcon} = getHelperLists savedLocationResp recentPredictionsObject currentState.homeScreen
        sortedTripList =  
          Arr.take 30 
            $ filter 
                (\item -> isPointWithinXDist item state state.data.config.suggestedTripsAndLocationConfig.tripWithinXDist) 
            $ Arr.reverse 
                (Arr.sortWith (\d -> fromMaybe 0.0 d.locationScore) tripArrWithNeighbors)
        
        updatedLocationList = updateLocListWithDistance updateFavIcon state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
        correctedList = map (\item -> transformTrip item) sortedTripList
    updateSavedLocations savedLocationResp 
    modifyScreenState $ GlobalPropsType (\globalProps -> globalProps{cachedSearches = updatedLocationList , recentSearches = recents, savedLocations = savedLocationResp})
    modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState{data{locationList = updatedLocationList}})
    modifyScreenState $ 
      HomeScreenStateType 
        (\homeScreen -> 
          homeScreen
            { data
              { savedLocations = savedLocationResp
              , recentSearchs {predictionArray = recents}
              , locationList = updatedLocationList
              , destinationSuggestions = updatedLocationList
              , suggestionsData{suggestionsMap = suggestionsMap}
              , tripSuggestions = removeDuplicateTrips correctedList suggestionsConfig.destinationGeohashPrecision
              }
            })
    where

      removeDuplicateTrips :: Array Trip -> Int -> Array Trip
      removeDuplicateTrips trips precision = 
        let 
          grouped = groupBy 
            (\trip1 trip2 -> 
              (getGeoHash trip1.destLat trip1.destLong precision) 
              == 
              (getGeoHash trip2.destLat trip2.destLong precision)
              && trip1.serviceTierNameV2 == trip2.serviceTierNameV2
            ) 
            trips

          maxScoreTrips = map 
            (maximumBy (comparing (\trip -> trip.locationScore))) 
            grouped
        in 
          catMaybes maxScoreTrips

      isPointWithinXDist :: Trip -> HomeScreenState -> Number -> Boolean
      isPointWithinXDist item state thresholdDist =
        let sourceLat = if state.props.sourceLat == 0.0 then fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT else state.props.sourceLat
            sourceLong = if state.props.sourceLong == 0.0 then fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON else state.props.sourceLong
        in
          getDistanceBwCordinates 
            item.sourceLat 
            item.sourceLong 
            sourceLat
            sourceLong
            <= thresholdDist
      
      locationEquality :: LocationListItemState -> LocationListItemState -> Boolean
      locationEquality a b = a.lat == b.lat && a.lon == b.lon


      getMapValuesArray :: forall k v. Map.Map k v -> Array v
      getMapValuesArray = foldMap singleton

      getHelperLists savedLocationLists recentPredictionsObject state = 
        let suggestionsConfig = state.data.config.suggestedTripsAndLocationConfig
            homeWorkImages = [fetchImage FF_ASSET "ny_ic_home_blue", fetchImage FF_ASSET "ny_ic_work_blue"]
            isHomeOrWorkImage = \listItem -> any (_ == listItem.prefixImageUrl) homeWorkImages
            savedLocationWithHomeOrWorkTag = filter isHomeOrWorkImage savedLocationResp
            recents = differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag
            savedLocationsWithOtherTag = filter (not <<< isHomeOrWorkImage) savedLocationResp
            suggestionsMap = getSuggestionsMapFromLocal FunctionCall
            currentGeoHash = getGeoHash state.props.sourceLat state.props.sourceLong suggestionsConfig.geohashPrecision
            geohashNeighbors = Arr.cons currentGeoHash $ geohashNeighbours currentGeoHash
            currentGeoHashDestinations = fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations currentGeoHash suggestionsMap suggestionsConfig.geohashLimitForMap)
            arrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).destinationSuggestions) geohashNeighbors)
            tripArrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).tripSuggestions) geohashNeighbors)
            sortedDestinationsList = Arr.take 30 (Arr.reverse (Arr.sortWith (\d -> fromMaybe 0.0 d.locationScore) arrWithNeighbors))
            suggestedDestinationsArr = differenceOfLocationLists sortedDestinationsList savedLocationWithHomeOrWorkTag

            allValuesFromMap = concat $ map (\item -> item.tripSuggestions)(getMapValuesArray suggestionsMap)
            sortedValues = Arr.sortWith (\d -> fromMaybe 0.0 d.locationScore) allValuesFromMap
            reversedValues = Arr.reverse sortedValues
            topValues = Arr.take 30 reversedValues
            topTripDestinatiions = map (\item -> getLocationFromTrip Destination item state.props.sourceLat state.props.sourceLong) topValues
            
            recentSearchesWithoutSuggested =  differenceOfLocationLists recents suggestedDestinationsArr
            topTripDestinatiionsWoutSuggested = differenceOfLocationLists (differenceOfLocationLists topTripDestinatiions suggestedDestinationsArr) savedLocationWithHomeOrWorkTag
            smartSuggestions = if null suggestedDestinationsArr then topTripDestinatiionsWoutSuggested else suggestedDestinationsArr
            sugestedFinalList =  nubByEq locationEquality $ smartSuggestions <> (Arr.take (suggestionsConfig.locationsToBeStored - (length smartSuggestions)) recentSearchesWithoutSuggested)
            

            updateFavIcon = 
              map (\item ->
                  item { postfixImageUrl =  
                          if not (checkPrediction item savedLocationsWithOtherTag) 
                            then fetchImage FF_ASSET "ny_ic_fav_red"
                            else fetchImage FF_ASSET "ny_ic_fav" 
                      }
                  ) sugestedFinalList
        in {savedLocationsWithOtherTag, recents, suggestionsMap, tripArrWithNeighbors, updateFavIcon}


addLocToCurrLoc :: Number -> Number -> String -> FlowBT String Unit
addLocToCurrLoc lat lon name = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations{ pastCurrentLocations = addToPrevCurrLoc {lat: lat, lon:lon, placeName : name} homeScreen.data.previousCurrentLocations.pastCurrentLocations}}})
  (GlobalState modifiedState) <- getState
  void $ pure $ saveCurrentLocations "PREVIOUS_CURRENT_LOCATION" modifiedState.homeScreen.data.previousCurrentLocations
  pure unit

getDistanceInfo :: Array LocationListItemState -> String -> Number -> Number -> String -> FlowBT String {tagExists :: Boolean, locExistsAs :: String }
getDistanceInfo savedLocations excludeLocation lat lon placeId = do
  distArr <- pure $ ((AddNewAddress.calculateDistance savedLocations excludeLocation lat lon))
  rslt <- pure $ ((AddNewAddress.isValidLocation savedLocations excludeLocation placeId))
  let placeIdExists =(fromMaybe {locationName : "" , distanceDiff : 1.0} ((rslt)!!0))
      minDist = ((fromMaybe {locationName : "" , distanceDiff : 1.0} ((distArr)!!0)))
      locExistsAs = case placeIdExists.locationName /= "" , minDist.distanceDiff <= 0.020 of
                      true , _ -> placeIdExists.locationName
                      false    , true -> minDist.locationName
                      _ , _ -> ""
      tagExists = ((length rslt) > 0 || minDist.distanceDiff <= 0.020)
  pure $ { tagExists, locExistsAs }



updateDistanceInfo :: AddNewAddressScreenState ->Maybe Number ->Maybe Number -> FlowBT String Unit
updateDistanceInfo state lat lon = do

            distanceInfo <- getDistanceInfo state.data.savedLocations  (if state.props.editLocation then state.data.placeName else "") (fromMaybe 0.0 lat) (fromMaybe 0.0 lon) (fromMaybe "" state.data.selectedItem.placeId)
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { tagExists = distanceInfo.tagExists
                  , isLocateOnMap = false
                  , showSavePlaceView = true
                  , isBtnActive = case state.data.activeIndex of
                                    Just 2 -> if state.data.addressSavedAs /= "" then true else false
                                    Just index -> true
                                    Nothing -> false
                  }
                , data
                  { selectedTag = state.data.selectedTag
                  , activeIndex = state.data.activeIndex
                  , existsAs = distanceInfo.locExistsAs
                  }
                } )
            addNewAddressScreenFlow ""

dummyLocationListItemState :: LocationListItemState
dummyLocationListItemState = locationListStateObj{locationItemType = Just PREDICTION}


removeChatService :: String -> FlowBT String Unit -- TODO:: Create a chat service and remove this
removeChatService _ = do
  let state = HomeScreenData.initData.data
  void $ lift $ lift $ liftFlow $ stopChatListenerService
  void $ pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  modifyScreenState $ HomeScreenStateType (\homeScreen -> 
    homeScreen{
      props{sendMessageActive = false, chatcallbackInitiated = false, unReadMessages = false, openChatScreen = false, showChatNotification = false, canSendSuggestion = true, isChatNotificationDismissed = false, isNotificationExpanded = false, removeNotification = true, enableChatWidget = false},
      data{messages = [], messagesSize = "-1", chatSuggestionsList = [], messageToBeSent = "", lastMessage = state.lastMessage, waitTimeInfo = false, lastSentMessage = state.lastSentMessage, lastReceivedMessage = state.lastReceivedMessage}})

setFlowStatusData :: Encode FlowStatusData => FlowStatusData -> Effect Unit
setFlowStatusData object = void $ pure $ setValueToLocalStore FLOW_STATUS_DATA (encodeJSON object)

updateFlowStatus :: NotifyFlowEventType -> FlowBT String Unit
updateFlowStatus eventType = do
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  case flowStatus.currentStatus of
    RIDE_ASSIGNED _ -> do
      checkRideStatus true
      homeScreenFlow
    _               -> do
      res <- lift $ lift $ Remote.notifyFlowEvent (Remote.makeNotifyFlowEventReq (show eventType))
      hideLoaderFlow
      case res of
        Right _  -> homeScreenFlow
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorCode"
          when ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") $ do
            currentFlowStatus

getTicketBookings :: Array TicketBookingItem -> Array TicketBookingItem -> TicketBookings
getTicketBookings bookedRes pendingRes = {
  pendingBooking : pendingRes,
  booked : bookedRes
}


cancelEstimate :: String -> FlowBT String Unit
cancelEstimate bookingId = do
  logField_ <- lift $ lift $ getLogFields
  res <- lift $ lift $ Remote.cancelEstimate bookingId
  if bookingId == ""
    then do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = HomeScreen, autoScroll = false}})
    else do
      case res of
        Right res -> do
          -- TODO : to be removed after new bundle is 100% available (replace with pure unit)
          let (CancelEstimateRes resp) = res
          case resp.result of
            "Success" -> do
              if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
                void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_driver_assign"
                pure unit
                else do
                  void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_quotes"
                  pure unit
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{isSearchCancelled = true}})
            _ -> do
              void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_right"
              currentFlowStatus
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorCode"
          if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
            void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_left"
            currentFlowStatus
          else do
            void $ pure $ toast $ getString STR.CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
            void $ liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_left"
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{autoScroll = false, currentStage = HomeScreen}}) 
            homeScreenFlow

getGenderValue :: Maybe Gender.Gender -> Maybe String
getGenderValue gender =
  case gender of
    Just value -> case value of
      Gender.MALE -> Just "MALE"
      Gender.FEMALE -> Just "FEMALE"
      Gender.OTHER -> Just "OTHER"
      _ -> Just "PREFER_NOT_TO_SAY"
    Nothing -> Nothing

getPlaceCoordinates :: String -> FlowBT String { latitude :: Number, longitude :: Number }
getPlaceCoordinates address =
  let {latitude, longitude} = runFn1 getLatLonFromAddress address
  in pure {latitude, longitude}
  
isAddressFieldsNothing :: Address -> Boolean
isAddressFieldsNothing address = all isNothing [address.area, address.state, address.country, address.building, address.door, address.street, address.city, address.areaCode, address.ward, address.placeId]

getPlaceName :: Number -> Number -> Location -> Boolean -> FlowBT String (Maybe PlaceName)
getPlaceName lat long location getApiResponse = do
  case location.address of
    Just address -> do
      let addressComponent = mkAddressComponent location "sublocality"
      pure $ Just $ mkPlaceName lat long address (Just addressComponent)
    Nothing -> do
      let address = runFn2 getLocationNameV2 lat long
      config <- getAppConfigFlowBT appConfig
      logField_ <- lift $ lift $ getLogFields
      if address /= "NO_LOCATION_FOUND" && config.geoCoder.enableLLtoAddress then do
        liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_found"
        pure $ Just $ mkPlaceName lat long address Nothing
      else do
        if getApiResponse then do
          (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat long $ EHC.getMapsLanguageFormat $ getLanguageLocale languageKey)
          liftFlowBT $ logEvent logField_ "ny_geocode_ll_address_fallback"
          pure $ Just $ (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
        else do
          pure Nothing
  where 
    mkPlaceName :: Number -> Number -> String -> Maybe AddressComponents -> PlaceName
    mkPlaceName lat long address addressComponent = 
      PlaceName {
        formattedAddress : address
      , location : LatLong { lat : lat, lon : long }
      , plusCode : Nothing
      , addressComponents : [] <> catMaybes [addressComponent]
      , placeId : Nothing
      }
    mkAddressComponent :: Location -> String -> AddressComponents
    mkAddressComponent location addressType =
      AddressComponents {
          longName : location.place
        , shortName : location.place
        , types : [addressType]
      }

dummyLocationData :: LocationData
dummyLocationData = LocationData {
    lat : 0.0
  , lon : 0.0
  , name : Nothing
}

checkAndUpdateLocations :: FlowBT String Unit
checkAndUpdateLocations = do
  let mBPayload = getGlobalPayload Constants.globalPayload
  maybe 
    (pure unit) 
    (\(GlobalPayload payload) -> do
      void $ pure $ spy "inside right" payload
      let (Payload innerPayload) = payload.payload
      case isNothing innerPayload.search_type of
        true -> pure unit
        false -> do
          let searchType = fromMaybe "normal_search" $ innerPayload.search_type
          if searchType /= "normal_search" then do
            let (LocationData source) = fromMaybe dummyLocationData innerPayload.source
            let (LocationData destination) = fromMaybe dummyLocationData innerPayload.destination
            modifyScreenState $ HomeScreenStateType (\homescreen -> homescreen {
              data {
                source = (fromMaybe "" source.name)
              , destination = (fromMaybe "" destination.name)
              , sourceAddress = encodeAddress (fromMaybe "" source.name) [] Nothing source.lat source.lon
              , destinationAddress = encodeAddress (fromMaybe "" destination.name) [] Nothing destination.lat destination.lon
              }, props {
                  sourceLat = source.lat
                , sourceLong = source.lon
                , destinationLat = destination.lat
                , destinationLong = destination.lon
                , isSource = Just false
                , isSearchLocation = SearchLocation
              }
            })
          else pure unit)
    mBPayload



rideCompletedDetails :: RideBookingRes -> Array ClevertapEventParams
rideCompletedDetails (RideBookingRes resp) = do
  let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
      (RideBookingDetails contents) = bookingDetails.contents
      (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
      differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
      finalAmount =  getFinalAmount (RideBookingRes resp)
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
      actualTollCharge = maybe 0 (\obj ->  obj^._amount) $ Arr.find (\entity  -> entity ^._description == "TOLL_CHARGES") (resp.fareBreakup)

  [ {key : "Estimate ride distance (km)", value : unsafeToForeign (fromMaybe 0 contents.estimatedDistance/1000)},
          {key : "Actual ride distance (km)", value : unsafeToForeign ((fromMaybe 0 ride.chargeableRideDistance)/1000)},
          {key : "Difference between estimated and actual ride distance (km)" , value : unsafeToForeign (differenceOfDistance/1000)},
          {key : "Total Estimated fare (₹)", value : unsafeToForeign (resp.estimatedFare)},
          {key : "Total Actual fare (₹)", value : unsafeToForeign (finalAmount)},
          {key : "Difference between estimated and actual fares (₹)", value : unsafeToForeign (resp.estimatedFare - finalAmount)},
          {key : "Driver pickup charges (₹)", value : unsafeToForeign "10"},
          {key : "Night ride", value : unsafeToForeign nightChargesVal},
          {key : "Actual Toll Charges", value : unsafeToForeign actualTollCharge},
          {key : "Has Toll", value : unsafeToForeign (maybe false (\charge -> charge /= 0) (Just actualTollCharge))}]

personStatsData :: PersonStatsRes -> GetProfileRes -> Array ClevertapEventParams
personStatsData (PersonStatsRes resp) (GetProfileRes response) = [{key : "First ride taken" , value : unsafeToForeign if response.hasTakenRide then "true" else "false"},
                                                                  {key : "Common App Use Case",value : unsafeToForeign resp.commonAppUseCase},
                                                                  {key : "Emergency Contacts Num", value : unsafeToForeign resp.emergencyContactsNum },
                                                                  {key : "Favourite Locations Num", value : unsafeToForeign resp.favoriteLocationsNum},
                                                                  {key : "Frequency Category" , value : unsafeToForeign resp.frequencyCategory},
                                                                  {key : "Is Blocked", value : unsafeToForeign resp.isBlocked},
                                                                  {key : "Is Churned User", value : unsafeToForeign resp.isChurnedUser},
                                                                  {key : "Is WhatsApp Opt-In Status" , value : unsafeToForeign resp.isWhatsAppOptInStatus},
                                                                  {key : "total_rider_trips" , value : unsafeToForeign resp.lifetimeRides},
                                                                  {key : "Last Ride Taken" , value : unsafeToForeign (fromMaybe "" resp.lastRideTaken)},
                                                                  {key : "Latest Search", value : unsafeToForeign (fromMaybe "" resp.latestSearch)},
                                                                  {key : "Off Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.offPeakRidesRate)},
                                                                  {key : "Overall Cancellation Rate", value : unsafeToForeign (fromMaybe 0.0 resp.overalCancellationRate)},
                                                                  {key : "Sign-up Date", value : unsafeToForeign resp.signupDate},
                                                                  {key : "Status" , value : unsafeToForeign (fromMaybe "" resp.status)},
                                                                  {key : "User Cancellation Rate", value : unsafeToForeign (fromMaybe 0.0 resp.userCancellationRate)},
                                                                  {key : "User Category", value : unsafeToForeign resp.userCategory},
                                                                  {key : "Weekday Evening Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayEveningPeakRidesRate)},
                                                                  {key : "Weekday Morning Peak Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayMorningPeakRidesRate)},
                                                                  {key : "Weekday Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekdayRidesRate)},
                                                                  {key : "Weekend Peak Ride Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekendPeakRideRate)},
                                                                  {key : "Weekend Rides Rate", value : unsafeToForeign (fromMaybe 0.0 resp.weekendRidesRate)}
                                                                  ]

updateSourceLocation :: String ->  FlowBT String Unit
updateSourceLocation _ = do 
  (GlobalState currentState) <- getState
  let disabled = case currentState.homeScreen.data.disability of 
                      Just val -> Just val.tag
                      Nothing -> Just ""
  when (disabled == Just "BLIND_LOW_VISION" ) $ do
    fullAddress <- getPlaceName currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong HomeScreenData.dummyLocation true
    case fullAddress of
      Just (PlaceName address) -> do 
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data{ source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing currentState.homeScreen.props.sourceLat currentState.homeScreen.props.sourceLong } })
      Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
    pure unit
  pure unit 

updateCurrentLocation :: String -> FlowBT String Unit
updateCurrentLocation _ = do
  (GlobalState currentState) <- getState
  currentAddress <- getPlaceName currentState.homeScreen.props.currentLocation.lat currentState.homeScreen.props.currentLocation.lng HomeScreenData.dummyLocation false
  case currentAddress of
    Just (PlaceName address) -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentLocation{place = address.formattedAddress}}})
    Nothing -> pure unit
  pure unit

updateUserInfoToState :: HomeScreenState -> FlowBT String Unit
updateUserInfoToState state =
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            HomeScreenData.initData
              { data
                { disability = state.data.disability
                , settingSideBar
                  { gender = state.data.settingSideBar.gender
                  , email = state.data.settingSideBar.email
                  , hasCompletedSafetySetup = state.data.settingSideBar.hasCompletedSafetySetup
                  }
                  , destinationSuggestions = state.data.destinationSuggestions
                  , tripSuggestions = state.data.tripSuggestions
                  , followers = state.data.followers
                }
              , props { 
                  isBanner = state.props.isBanner
                , sosBannerType = state.props.sosBannerType 
                , followsRide = state.props.followsRide
                , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                }
              }
        )

placeListFlow :: FlowBT String Unit 
placeListFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET PLACE LIST CALLED" currentState
  (GlobalState state) <- getState
  uiAction <- lift $ lift $ runScreen $ PlaceListS.screen state.ticketingScreen
  case uiAction of
    PlaceListC.ExitToHomeScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> homeScreenFlow)
    PlaceListC.ExitToMyTicketsScreen updatedState -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData{props{navigateToHome = false, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes)}})            
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> ticketListFlow)
    PlaceListC.BookTickets updatedState selectedPlace -> do
      modifyScreenState $ TicketingScreenStateType (\_ -> updatedState)
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{props{navigateToHome = false, currentStage = DescriptionStage, previousStage = DescriptionStage}, data { totalAmount = 0, placeInfo = Just selectedPlace}})
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> placeDetailsFlow)
    _ -> App.BackT $ pure App.GoBack

placeDetailsFlow :: FlowBT String Unit
placeDetailsFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET PLACE DETAILS CALLED" currentState
  liftFlowBT $ hideLoader
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})             
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ PlaceDetailsS.screen state.ticketBookingScreen
  case action of
    PlaceDetailsC.GoToHomeScreen updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> if updatedState.props.navigateToHome then homeScreenFlow else placeListFlow)
    PlaceDetailsC.GoToTicketPayment state -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> ticketPaymentFlow state.data)
    PlaceDetailsC.GoToOpenGoogleMaps state lat2 long2 -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState -> state)
      (App.BackT $ App.BackPoint <$> pure unit) >>= (\_ -> openGoogleMaps lat2 long2)
    PlaceDetailsC.BookTickets updatedState -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      (App.BackT $ App.NoBack <$> pure unit) >>= (\_ -> if updatedState.props.navigateToHome then homeScreenFlow else placeListFlow)
  where
    openGoogleMaps lat long = do
      void $ pure $ openNavigation lat long "DRIVE"
      placeDetailsFlow
 
ticketStatusFlow :: FlowBT String Unit
ticketStatusFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET STATUS CALLED" currentState
  liftFlowBT $ hideLoader
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})       
  flow <- UI.ticketStatusScreen 
  case flow of
    GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{focussedBottomIcon = MOBILITY}})
      if state.props.navigateToHome then homeScreenFlow else placeListFlow
    REFRESH_PAYMENT_STATUS state -> do
      (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
      updatePaymentStatusData ticketStatus state.props.selectedBookingId
      setValueToLocalStore PAYMENT_STATUS_POOLING "false"
      ticketStatusFlow
    GO_TO_TICKET_LIST state -> do
      (GetAllBookingsRes bookedRes) <- Remote.getAllBookingsBT Booked
      (GetAllBookingsRes pendingRes) <- Remote.getAllBookingsBT Pending
      modifyScreenState $ TicketBookingScreenStateType (\_ -> TicketBookingScreenData.initData{props{navigateToHome = false, currentStage = ViewTicketStage, previousStage = ViewTicketStage, ticketBookingList = getTicketBookings (buildBookingDetails bookedRes) (buildBookingDetails pendingRes)}})
      ticketListFlow
    GET_BOOKING_INFO_SCREEN state bookingStatus -> do
      (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
      if bookingStatus == Pending
        then do
          modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
          setValueToLocalStore PAYMENT_STATUS_POOLING "true"
          fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
          ticketStatusFlow
        else do
          let ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
          let dummyListItem = TicketBookingScreenData.dummyServiceDetails
          modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
          zooTicketInfoFlow
    _ -> ticketStatusFlow

metroTicketBookingFlow :: FlowBT String Unit
metroTicketBookingFlow = do
  (GlobalState currentState) <- getState
  config <- getAppConfigFlowBT appConfig
  metroStationsList <- lift $ lift $ getMetroStationsObjFromLocal ""
  let currentCity = getValueToLocalStore CUSTOMER_LOCATION
      stationsForCurrentCity = (findMetroStationsForCity currentCity metroStationsList)
  parsedMetroStation <- case stationsForCurrentCity of
    Just stations -> do
      let diffSec = runFn2 differenceBetweenTwoUTCInMinutes (getCurrentUTC "") stations.lastUpdatedAt
          cityMetroConfig = getMetroConfigFromAppConfig config currentCity 
          metroStationValidTill = cityMetroConfig.metroStationTtl
      if diffSec > metroStationValidTill
        then fetchMetroStations currentCity metroStationsList
        else pure $ parseMetroStations stations.stations
    Nothing -> fetchMetroStations currentCity metroStationsList
  flow <- UI.metroTicketBookingScreen
  case flow of
    GO_TO_HOME_SCREEN_FROM_METRO_TICKET state -> homeScreenFlow
    GO_TO_METRO_STATION_SEARCH srcdest -> do
      let searchLocationState = currentState.searchLocationScreen
          textFieldFocus = case srcdest of
                      Src -> Just SearchLocPickup
                      Dest -> Just SearchLocDrop
      if null searchLocationState.data.metroStations then do
        modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState{props{actionType = MetroStationSelectionAction, canSelectFromFav = false, focussedTextField = textFieldFocus}, data { fromScreen = getScreen Screen.METRO_TICKET_BOOKING_SCREEN, metroStations = parsedMetroStation, updatedMetroStations = parsedMetroStation}})
      else do
        modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState{props{actionType = MetroStationSelectionAction, canSelectFromFav = false, focussedTextField = textFieldFocus}, data { srcLoc = searchLocationState.data.srcLoc, destLoc = searchLocationState.data.destLoc, fromScreen = getScreen Screen.METRO_TICKET_BOOKING_SCREEN, metroStations = searchLocationState.data.metroStations, updatedMetroStations = searchLocationState.data.metroStations}})
      searchLocationFlow
    METRO_FARE_AND_PAYMENT state -> do
      if state.props.currentStage == MetroTicketSelection then do
        (SearchMetroResp searchMetroResp) <- Remote.searchMetroBT (Remote.makeSearchMetroReq state.data.srcCode state.data.destCode state.data.ticketCount)
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{data{ searchId = searchMetroResp.searchId}, props { currentStage = GetMetroQuote }})
      else if state.props.currentStage == ConfirmMetroQuote then do
        (MetroTicketBookingStatus metroBookingStatus) <- Remote.confirmMetroQuoteBT state.data.quoteId
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> 
          state{data{ bookingId = metroBookingStatus.bookingId}
              , props {currentStage = ST.PaymentSDKPooling }})
        setValueToLocalStore METRO_PAYMENT_SDK_POLLING "true"
      else pure unit 
      metroTicketBookingFlow  
    GO_TO_MY_METRO_TICKET_SCREEN -> do
      modifyScreenState $ MetroMyTicketsScreenStateType (\metroMyTicketsScreen -> metroMyTicketsScreen{props{entryPoint = ST.MetroTicketBookingToMetroMyTickets}})
      metroMyTicketsFlow
    GO_TO_METRO_ROUTE_MAP -> do
      let currentCity = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      modifyScreenState $ MetroTicketDetailsScreenStateType (\_ -> MetroTicketDetailsScreenData.initData)
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState{data{city = currentCity}, props{stage = ST.MetroMapStage, previousScreenStage = ST.MetroTicketSelectionStage}})
      metroTicketDetailsFlow
    REFRESH_METRO_TICKET_SCREEN state -> do
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{props { currentStage = ConfirmMetroQuote }})
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusScreen{data{quoteId = state.data.quoteId}})
      metroTicketBookingFlow
    GO_TO_HOME_FROM_METRO_TICKET -> homeScreenFlow 
    GO_TO_METRO_PAYMENT_PAGE orderResp bookingId -> do 
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{props { currentStage = ConfirmMetroQuote }})
      metroTicketPaymentFlow orderResp bookingId
    where
      fetchMetroStations currentCity metroStationsList = do
        (GetMetroStationResponse getMetroStationResp) <- Remote.getMetroStationBT currentCity
        void $ pure $ saveObject "METRO_STATIONS" (getMetroStations currentCity getMetroStationResp metroStationsList)
        let parsedStations_ = parseMetroStations getMetroStationResp
        modifyScreenState $ SearchLocationScreenStateType (\_ -> SearchLocationScreenData.initData)
        modifyScreenState $ SearchLocationScreenStateType (\slsState -> slsState{data {metroStations = parsedStations_, updatedMetroStations = parsedStations_}})
        pure parsedStations_

      getMetroStations :: String -> Array GetMetroStationResp -> Array MetroStations -> Array ST.MetroStations
      getMetroStations city metroStationsResp metroStationArr = 
        let  
          currentCity = getCityFromString city
          filteredStationArr = filter (\station -> station.city /= currentCity) metroStationArr
          currentCityStations = 
              [{ city : currentCity
              , stations : metroStationsResp
              , lastUpdatedAt : getCurrentUTC ""
              }]
        in filteredStationArr <> currentCityStations
      
      findMetroStationsForCity :: String -> Array MetroStations -> Maybe MetroStations
      findMetroStationsForCity city stations =
        let
          currentCity = getCityFromString city
        in
          if null stations 
            then Nothing
            else Arr.find (\station -> station.city == currentCity) stations

      parseMetroStations :: Array GetMetroStationResp -> Array {stationName :: String, stationCode :: String}
      parseMetroStations stations = 
        map (\(GetMetroStationResp item) -> {
          stationName : item.name,
          stationCode : item.code
        }) stations


metroTicketPaymentFlow :: CreateOrderRes ->  String -> FlowBT String Unit
metroTicketPaymentFlow (CreateOrderRes orderResp) bookingId= do
  liftFlowBT $ initiatePaymentPage
  let (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
      (PayPayload innerpayload) = sdk_payload.payload
      finalPayload = PayPayload $ innerpayload{ language = Just (getPaymentPageLangKey (getLanguageLocale languageKey)) }
      sdkPayload = PaymentPagePayload $ sdk_payload{payload = finalPayload}
      shortOrderID = orderResp.order_id
  lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
  void $ paymentPageUI sdkPayload
  void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
  void $ lift $ lift $ toggleLoader true
  setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "true"
  modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusScreen{data{bookingId = bookingId}})
  (GetMetroBookingStatusResp getMetroStatusResp) <- Remote.getMetroStatusBT bookingId
  let (MetroTicketBookingStatus metroTicketStatusResp2) = getMetroStatusResp
  void $ pure $ toggleBtnLoader "" false
  void $ lift $ lift $ toggleLoader false
  case metroTicketStatusResp2.payment of
      Just (FRFSBookingPaymentAPI paymentInfo) -> do
        if paymentInfo.status == "NEW" then metroTicketBookingFlow
        else do
          modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer getMetroStatusResp metroTicketDetailsState)
          modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer getMetroStatusResp metroTicketStatusScreen)
          metroTicketStatusFlow
      Nothing -> do
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{ props { currentStage = MetroTicketSelection }})
        metroTicketBookingFlow

ticketListFlow :: FlowBT String Unit
ticketListFlow = do
  (GlobalState currentState) <- getState
  void $ pure $ spy "ZOO TICKET TICKET LIST CALLED" currentState
  liftFlowBT $ hideLoader
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})             
  flow <- UI.ticketListScreen
  case flow of
    GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
    GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW dstLat1 dstLon2  -> do
      void $ pure $ openNavigation dstLat1 dstLon2 "DRIVE"
      ticketListFlow
    GET_BOOKING_INFO_SCREEN state bookingStatus -> do
      (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
      if bookingStatus == Pending
        then do
          modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
          setValueToLocalStore PAYMENT_STATUS_POOLING "true"
          fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
          ticketStatusFlow
        else do
          let ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
          let dummyListItem = TicketBookingScreenData.dummyServiceDetails
          modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
          zooTicketInfoFlow
    GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{focussedBottomIcon = MOBILITY}})
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      if state.props.navigateToHome then homeScreenFlow else placeListFlow
    RESET_SCREEN_STATE -> do
      modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
      ticketListFlow
    REFRESH_PAYMENT_STATUS state -> do
      (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
      updatePaymentStatusData ticketStatus state.props.selectedBookingId
      setValueToLocalStore PAYMENT_STATUS_POOLING "false"
      ticketListFlow
    _ -> ticketListFlow

-- zooTicketBookingFlow :: FlowBT String Unit -- NOTE :: KEEPING IT FOR REFERENCE
-- zooTicketBookingFlow = do
--   (GlobalState currentState) <- getState
--   void $ pure $ spy "ZOO TICKET BOOKING FLOW CALLED" currentState
--   liftFlowBT $ hideLoader
--   modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})             
--   flow <- UI.ticketBookingScreen
--   case flow of
--     GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
--     GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW dstLat1 dstLon2  -> do
--       void $ pure $ openNavigation dstLat1 dstLon2 "DRIVE"
--       zooTicketBookingFlow
--     GET_BOOKING_INFO_SCREEN state bookingStatus -> do
--       (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId
--       if bookingStatus == Pending
--         then do
--           modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
--           setValueToLocalStore PAYMENT_STATUS_POOLING "true"
--           fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
--           zooTicketBookingFlow
--         else do
--           let ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
--           let dummyListItem = TicketBookingScreenData.dummyServiceDetails
--           modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
--           zooTicketInfoFlow
--     GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       if state.props.navigateToHome then homeScreenFlow else placeListFlow
--     RESET_SCREEN_STATE -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       zooTicketBookingFlow
--     REFRESH_PAYMENT_STATUS state -> do
--       (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
--       updatePaymentStatusData ticketStatus state.props.selectedBookingId
--       setValueToLocalStore PAYMENT_STATUS_POOLING "false"
--       zooTicketBookingFlow
--     _ -> zooTicketBookingFlow

ticketPaymentFlow :: TicketBookingScreenData -> FlowBT String Unit
ticketPaymentFlow screenData = do
  liftFlowBT $ initiatePaymentPage
  let ticketPlaceID = maybe "" (\(TicketPlaceResp ticketPlaceResp) -> ticketPlaceResp.id) screenData.placeInfo
  (CreateOrderRes orderResp) <- Remote.bookTicketsBT (Remote.mkBookingTicketReq screenData) ticketPlaceID
  let (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
      (PayPayload innerpayload) = sdk_payload.payload
      finalPayload = PayPayload $ innerpayload{ language = Just (getPaymentPageLangKey (getLanguageLocale languageKey)) }
      sdkPayload = PaymentPagePayload $ sdk_payload{payload = finalPayload}
      shortOrderID = orderResp.order_id
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{shortOrderId = shortOrderID}, props{selectedBookingId = shortOrderID}})
  lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
  void $ paymentPageUI sdkPayload
  void $ lift $ lift $ toggleLoader true
  void $ pure $ toggleBtnLoader "" false
  (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT shortOrderID
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
  updatePaymentStatusData ticketStatus shortOrderID
  void $ lift $ lift $ toggleLoader false
  ticketStatusFlow

updatePaymentStatusData :: String -> String -> FlowBT String Unit
updatePaymentStatusData ticketStatus shortOrderID =
  case ticketStatus of
    "Booked" -> do
      infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
      fillBookingDetails infoRes shortOrderID ticketStatus
    "Pending" -> do
      void $ pure $ toast $ "Fetching the status"
      infoRes <- Remote.getTicketBookingDetailsBT shortOrderID
      setValueToLocalStore PAYMENT_STATUS_POOLING "true"
      fillBookingDetails infoRes shortOrderID ticketStatus
    "Failed" -> do
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { paymentStatus = PP.Failed } })
    _ -> do
      void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
      modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = ticketBookingScreen.props.previousStage} }) -- temporary fix - will remove once 500 INTERNAL_SERVER_ERROR is solved.
      pure unit

zooTicketInfoFlow :: FlowBT String Unit
zooTicketInfoFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.ticketInfoScreen
  modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreenState ->  ticketBookingScreenState{props{currentStage = MyTicketsStage}})
  case flow of
    GO_TO_HOME_SCREEN_FROM_TICKET_INFO -> currentFlowStatus
    _ -> pure unit

fillBookingDetails :: TicketBookingDetails-> String -> String -> FlowBT String Unit
fillBookingDetails (TicketBookingDetails resp) shortOrderID ticketStatus = do
  let
    serv = resp.services !! 0
  modifyScreenState
    $ TicketBookingScreenStateType
        ( \ticketBookingScreen ->
            ticketBookingScreen
              { props
                { paymentStatus = if ticketStatus == "Booked" then PP.Success else PP.Pending
                }
              , data
                { zooName = resp.ticketPlaceName
                , keyValArray =
                  [ { key: "Date", val: convertUTCtoISC resp.visitDate "Do MMM YYYY"}
                  , { key: "Booking For", val: "" }
                  , { key: "Total Paid", val: ("₹" <> show resp.amount) }
                  , { key: "Booking ID", val: resp.ticketShortId }
                  , { key: "Transaction ID", val: shortOrderID }
                  ]
                    <> case serv of
                        Nothing -> []
                        Just (TicketBookingServiceDetails serviceDetails) -> 
                          if isJust serviceDetails.expiryDate then 
                            [ { key: "Valid until",
                                val: (maybe (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "hh:mm A") (\sl -> fromMaybe "" (convertUTCToISTAnd12HourFormat sl)) serviceDetails.slot )  
                                     <> ", " <> (convertUTCtoISC (fromMaybe "" serviceDetails.expiryDate) "Do MMM YYYY") } ] 
                          else []
                , bookedForArray = (map (\(TicketBookingServiceDetails item) ->  getTicketBookingForName item) resp.services)
                }
              }
        )
  where
    getTicketBookingForName ticket = ticket.ticketServiceName <> (DS.joinWith "" $ (map (\(TicketBookingCategoryDetails cat) ->  if cat.name /= "all" then " ( " <> cat.name <> " ) " else "") ticket.categories))


getCurrentLocationItem :: LocationDetails -> HomeScreenState -> Number -> Number ->  Maybe LocationListItemState
getCurrentLocationItem placeDetails state lat lon = 
  let latLon = getCoordinates state
  in
  Just (locationListStateObj {
    prefixImageUrl = fetchImage FF_ASSET "ny_ic_recent_search"
  , postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav"
  , title = getTitle placeDetails
  , subTitle = getSubTitle placeDetails
  , placeId = placeDetails.placeId
  , lat = Just latLon.lat
  , lon = Just latLon.lon
  , description = placeDetails.formattedAddress
  , tagType = Just $ show LOC_LIST
  , locationItemType = Just SUGGESTED_DESTINATIONS
  , fullAddress = encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing latLon.lat latLon.lon
  }) 
  where
    getTitle :: LocationDetails -> String
    getTitle placeDetails = (fromMaybe "" ((split (Pattern ",") (placeDetails.formattedAddress)) !! 0))

    getSubTitle :: LocationDetails -> String
    getSubTitle placeDetails =  let formattedAddress = placeDetails.formattedAddress
                                    index = fromMaybe 0 (indexOf (Pattern ",") formattedAddress)
                                in drop (index + 2) formattedAddress

    getCoordinates :: HomeScreenState -> { lat :: Number, lon :: Number }
    getCoordinates state =
      { lat: if state.props.currentStage /= ConfirmingLocation
                then lat
                else if state.props.isSource == Just true
                    then state.props.sourceLat
                    else state.props.destinationLat
      , lon: if state.props.currentStage /= ConfirmingLocation
                then lon
                else if state.props.isSource == Just true
                    then state.props.sourceLong
                    else state.props.destinationLong
      }

rideScheduledFlow :: FlowBT String Unit
rideScheduledFlow = do
  (GlobalState currentState) <- getState
  action <- lift $ lift $ runScreen $ UI.rideScheduledScreen currentState.rideScheduledScreen 
  case action of
    RideScheduledScreenOutput.GoToHomeScreen -> homeScreenFlow
    _ -> pure unit

metroTicketDetailsFlow :: FlowBT String Unit
metroTicketDetailsFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.metroTicketDetailsScreen 
  case flow of 
    BACK_TO_SEARCH_METRO_LOCATION -> metroTicketBookingFlow
    GO_BACK_TO_HOME_SCREEN -> homeScreenFlow
    GO_TO_MY_METRO_TICKETS_FLOW ->  metroMyTicketsFlow
    SOFT_CANCEL_BOOKING state -> do
      (MetroBookingSoftCancelResp result) <- Remote.metroBookingSoftCancelBT state.data.bookingId
      modifyScreenState $ MetroTicketDetailsScreenStateType (\state -> state{props { stage = MetroSoftCancelStatusStage, showLoader = true }})
      metroTicketDetailsFlow
    HARD_CANCEL_BOOKING state -> do
      (MetroBookingHardCancelResp result) <- Remote.metroBookingHardCancelBT state.data.bookingId
      modifyScreenState $ MetroTicketDetailsScreenStateType (\state -> state{props { stage = MetroHardCancelStatusStage , showLoader = true , isBookingCancellable = Nothing}})
      metroTicketDetailsFlow
    _ -> metroTicketDetailsFlow

metroMyTicketsFlow :: FlowBT String Unit
metroMyTicketsFlow = do
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.metroMyTicketsScreen 
  case flow of 
    GO_TO_METRO_TICKET_DETAILS_FLOW  bookingId -> do
      (GetMetroBookingStatusResp resp) <- Remote.getMetroStatusBT bookingId
      let (MetroTicketBookingStatus metroTicketBookingStatus) = resp
      if(metroTicketBookingStatus.status == "CONFIRMED") then do
        modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> 
          let transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
          in transformedState {props{previousScreenStage = ST.MetroMyTicketsStage}}
        )
        metroTicketDetailsFlow
      else if (metroTicketBookingStatus.status == "CANCELLED") then do
        modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> 
          let transformedState = metroTicketDetailsTransformer resp metroTicketDetailsState
          in transformedState {props{previousScreenStage = ST.MetroMyTicketsStage, stage = MetroHardCancelStatusStage}}
        )
        metroTicketDetailsFlow
      else if (any (_ == metroTicketBookingStatus.status) ["PAYMENT_PENDING", "FAILED"]) then do
        modifyScreenState $ MetroTicketStatusScreenStateType (\ metroTicketStatusScreen -> 
          let transformedState = metroTicketStatusTransformer resp metroTicketStatusScreen 
          in transformedState {props { entryPoint = MyMetroTicketsToMetroTicketStatus}}
        )
        setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
        metroTicketStatusFlow
      else
        metroMyTicketsFlow
    GO_TO_METRO_TICKET_STAUS_FLOW bookingStatusResp -> do
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer bookingStatusResp metroTicketStatusScreen)
      metroTicketStatusFlow
    GO_HOME_FROM_METRO_MY_TICKETS -> homeScreenFlow
    GO_METRO_BOOKING_FROM_METRO_MY_TICKETS -> metroTicketBookingFlow
    _ -> metroMyTicketsFlow

metroTicketStatusFlow :: FlowBT String Unit
metroTicketStatusFlow = do
  (GlobalState currentState) <- getState
  flow <- UI.metroTicketStatusScreen
  case flow of 
    GO_TO_METRO_TICKET_DETAILS state resp -> do
      modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer resp MetroTicketDetailsScreenData.initData) 
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState{props{previousScreenStage = ST.MetroTicketStatusStage}})
      metroTicketDetailsFlow
    REFRESH_STATUS_AC state -> do
      void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (GetMetroBookingStatusResp metroStatusResp) <- Remote.getMetroStatusBT state.data.bookingId 
      let (MetroTicketBookingStatus metroTicketStatusResp) = metroStatusResp
          paymentOrder = metroTicketStatusResp.payment >>= (\(FRFSBookingPaymentAPI payment') ->  payment'.paymentOrder)
          shortOrderID = case paymentOrder of 
                            Just (CreateOrderRes orderResp) -> orderResp.order_id
                            Nothing -> ""
      void $ lift $ lift $ toggleLoader false
      modifyScreenState $ MetroTicketDetailsScreenStateType (\metroTicketDetailsState -> metroTicketDetailsTransformer metroStatusResp metroTicketDetailsState)
      modifyScreenState $ MetroTicketStatusScreenStateType (\metroTicketStatusScreen -> metroTicketStatusTransformer metroStatusResp metroTicketStatusScreen)
      metroTicketStatusFlow
    GO_TO_TRY_AGAIN_PAYMENT state -> do 
      modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{props {currentStage = ST.MetroTicketSelection}})
      metroTicketBookingFlow
    GO_TO_HOME_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN -> homeScreenFlow
    GO_TO_METRO_TICKETS_SCREEN_FROM_METRO_TICKET_STATUS_SCREEN -> metroMyTicketsFlow


searchLocationFlow :: FlowBT String Unit
searchLocationFlow = do 
  (GlobalState globalState) <- getState
  action <- lift $ lift $ runScreen $ UI.searchLocationScreen globalState.searchLocationScreen globalState.globalProps
  case action of 
    SearchLocationController.AddStop state -> do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      searchLocationFlow
    SearchLocationController.UpdateLocName state lat lon -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      handleUpdateLocNameFlow state lat lon
    SearchLocationController.Reload state -> do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      searchLocationFlow 
    SearchLocationController.GoToMetroRouteMap state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let currentCity = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
      modifyScreenState $ MetroTicketDetailsScreenStateType (\_ -> MetroTicketDetailsScreenData.initData)
      modifyScreenState $ MetroTicketDetailsScreenStateType (\slsState -> slsState{data{city = currentCity}, props{stage = ST.MetroMapStage, previousScreenStage = ST.SearchMetroLocationStage}})
      metroTicketDetailsFlow
    SearchLocationController.NoOutput state -> modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    SearchLocationController.SearchPlace searchString state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      searchPlaceFlow searchString state
    SearchLocationController.SaveFavLoc state savedLoc -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      checkRedundantFavLocFlow state savedLoc    
    SearchLocationController.ConfirmAndSaveFav state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      confirmAndSaveLocFlow state
    SearchLocationController.PredictionClicked prediction state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      predictionClickedFlow prediction state
    SearchLocationController.AddFavLoc state tag -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      addFavLocFlow state tag
    SearchLocationController.HomeScreen state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      homeScreenFlow 
    SearchLocationController.RentalsScreen state -> modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
    SearchLocationController.LocSelectedOnMap state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      locSelectedOnMapFlow state 
    SearchLocationController.MetroTicketBookingScreen state -> do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      metroTicketBookingFlow
    _ -> pure unit
  where 
  
    locSelectedOnMapFlow :: SearchLocationScreenState -> FlowBT String Unit
    locSelectedOnMapFlow state = do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let latNum = fromMaybe 0.0 state.data.latLonOnMap.lat 
          lonNum = fromMaybe 0.0 state.data.latLonOnMap.lon
          focussedField = fromMaybe SearchLocPickup state.props.focussedTextField
      {pickUpPoints, locServiceable , city, geoJson, specialLocCategory} <- getServiceability latNum lonNum $ fromMaybe SearchLocPickup state.props.focussedTextField
      if locServiceable then 
        -- we already have the data inside srcLoc and destLoc
        pure unit
        else 
          modifyScreenState 
            $ SearchLocationScreenStateType (\slsState -> 
              slsState  { props{locUnserviceable = true, searchLocStage = PredictionsStage}
                        , data {
                            latLonOnMap = SearchLocationScreenData.dummyLocationInfo
                          , srcLoc = if focussedField == SearchLocPickup then Nothing else slsState.data.srcLoc 
                          , destLoc = if focussedField == SearchLocDrop then Nothing else slsState.data.destLoc
                          }
                        })
      searchLocationFlow 

    handleUpdateLocNameFlow :: SearchLocationScreenState -> String -> String -> FlowBT String Unit
    handleUpdateLocNameFlow state lat lon =  do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let latNum = fromMaybe 0.0 (fromString lat)
          lonNum = fromMaybe 0.0 (fromString lon)
      {pickUpPoints, locServiceable , city, geoJson, specialLocCategory} <- getServiceability latNum lonNum $ fromMaybe SearchLocPickup state.props.focussedTextField
      let cityName = getCityNameFromCode city
          isSpecialZone = (geoJson /= "") && (geoJson /= state.data.specialZoneCoordinates) &&
                          pickUpPoints /= state.data.nearByGates
          locOnMap = state.data.latLonOnMap
          updatedState = { lat : fromString lat, lon : fromString lon, placeId : locOnMap.placeId, address : locOnMap.address, addressComponents : locOnMap.addressComponents , city : Just cityName , metroInfo : Nothing, stationCode : ""} 
      modifyScreenState 
        $ SearchLocationScreenStateType 
            (\slsState -> slsState{data{ latLonOnMap = updatedState}})
      if isSpecialZone then 
        specialLocFlow geoJson pickUpPoints specialLocCategory latNum lonNum
        else 
          updateLocDetailsFlow state latNum lonNum pickUpPoints cityName

    updateLocDetailsFlow :: SearchLocationScreenState -> Number -> Number  -> Array Location -> City -> FlowBT String Unit
    updateLocDetailsFlow state lat lon pickUpPoints cityName = do
      let latLonOnMap = state.data.latLonOnMap
      let {mapLat , mapLon} = {mapLat : fromMaybe 0.0 latLonOnMap.lat, mapLon : fromMaybe 0.0 latLonOnMap.lon}
          distanceBwLatLon = getDistanceBwCordinates lat lon mapLat mapLon 
          isDistMoreThanThreshold = distanceBwLatLon > (state.appConfig.mapConfig.locateOnMapConfig.apiTriggerRadius/1000.0)
          pickUpPoint = filter ( \item -> (item.place == state.data.defaultGate)) pickUpPoints
          gateAddress = fromMaybe HomeScreenData.dummyLocation (head pickUpPoint)
      when (isDistMoreThanThreshold ) do  
        fullAddress <- getPlaceName lat lon gateAddress true
        case fullAddress of
          Just (PlaceName address) -> do
            let updatedAddress = {address : address.formattedAddress, lat : Just lat , lon : Just lon, placeId : Nothing, city : Just cityName ,addressComponents : encodeAddress address.formattedAddress [] Nothing lat lon , metroInfo : Nothing, stationCode : ""}
            modifyScreenState 
              $ SearchLocationScreenStateType 
                  (\ slsState -> slsState { data  {latLonOnMap = updatedAddress, confirmLocCategory = NOZONE} }) 
          Nothing -> void $ pure $ toast $ getString STR.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
      searchLocationFlow

    specialLocFlow :: String -> Array Location -> String -> Number -> Number -> FlowBT String Unit
    specialLocFlow geoJson pickUpPoints category lat lon = do
      modifyScreenState
          $ SearchLocationScreenStateType 
              (\searchLocScreen -> searchLocScreen 
                  { data {
                      specialZoneCoordinates = geoJson ,
                      nearByGates = pickUpPoints ,
                      confirmLocCategory = getZoneType category
                  }
                  })
      void $ pure $ removeAllPolylines "" 
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = geoJson, points = pickUpPoints, labelId = getNewIDWithTag "LocateOnMapSLSPin"}
      searchLocationFlow 

    searchPlaceFlow :: String -> SearchLocationScreenState -> FlowBT String Unit
    searchPlaceFlow searchString state = do
      modifyScreenState $ SearchLocationScreenStateType ( \_ -> state) 
      (GlobalState globalState) <- getState
      savedLoc <- fetchGlobalSavedLocations
      let { currentLat, currentLng } = maybe { currentLat: 0.0, currentLng: 0.0 } (\loc -> { currentLat: fromMaybe 0.0 loc.lat, currentLng: fromMaybe 0.0 loc.lon }) (state.data.currentLoc)
          { lat, lng } = maybe { lat : currentLat, lng : currentLng } (\loc -> { lat: fromMaybe 0.0 loc.lat, lng: fromMaybe 0.0 loc.lon }) $ maybe Nothing (\currField -> if currField == SearchLocPickup then (state.data.srcLoc) else  (state.data.destLoc)) $ state.props.focussedTextField
          cityConfig = case state.props.focussedTextField of
                          Just SearchLocDrop -> 
                            let config = getCityConfig state.appConfig.cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
                            in config{ geoCodeConfig{ strictBounds = true }}
                          _ -> defaultCityConfig
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq searchString lat lng (EHC.getMapsLanguageFormat $ getLanguageLocale languageKey) "" cityConfig.geoCodeConfig Nothing "")
      let sortedByDistanceList = sortPredictionByDistance searchLocationResp.predictions
          predictionList = getLocationList sortedByDistanceList
          sortedRecentsList =  updateLocListWithDistance globalState.globalProps.recentSearches lat lng true state.appConfig.suggestedTripsAndLocationConfig.locationWithinXDist
          filteredRecentsList = filterRecentSearches sortedRecentsList predictionList
          filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList     
          updatedLocList = map 
                            (\item -> do
                              let savedLocation = getPrediction item savedLoc 
                                  locInSavedLoc = checkPrediction item savedLoc
                              if (not locInSavedLoc) then 
                                item { lat = savedLocation.lat, lon = savedLocation.lon, locationItemType = Just SAVED_LOCATION, postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav_red"}
                              else
                                item { lat = item.lat, lon = item.lon, locationItemType = item.locationItemType, postfixImageUrl = fetchImage FF_ASSET "ny_ic_fav"}
                            )
                            (filteredRecentsList <> filteredPredictionList)

      modifyScreenState   
        $ SearchLocationScreenStateType   
          ( \searchLocationScreen -> 
              searchLocationScreen { data {locationList = updatedLocList}
                                   , props {showLoader = false }})

      searchLocationFlow

    checkRedundantFavLocFlow :: SearchLocationScreenState -> Array LocationListItemState -> FlowBT String Unit
    checkRedundantFavLocFlow state savedLoc  = do
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let selectedItem = state.data.saveFavouriteCard.selectedItem
      case selectedItem.locationItemType of 
        Just RECENTS -> getDistDiff savedLoc (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) (fromMaybe "" selectedItem.placeId)
        Nothing -> getDistDiff savedLoc (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) (fromMaybe "" selectedItem.placeId)
        _ -> do 
          (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedItem.title <> "," <> selectedItem.subTitle) selectedItem.placeId (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) selectedItem
          let (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head placeNameResp
              (LatLong placeLatLong) = placeName.location 
          (ServiceabilityResDestination serviceabilityRes) <- Remote.destServiceabilityBT $ Remote.makeServiceabilityReqForDest placeLatLong.lat placeLatLong.lon
          case serviceabilityRes.serviceable of 
            false -> do
              void $ pure $ toast $ getString STR.LOCATION_UNSERVICEABLE
              searchLocationFlow
            _ -> modifyScreenState $ SearchLocationScreenStateType (\_ -> state{data{saveFavouriteCard { selectedItem{lat = Just placeLatLong.lat, lon = Just placeLatLong.lon}}}})
          getDistDiff savedLoc placeLatLong.lat placeLatLong.lon (fromMaybe "" selectedItem.placeId)
      pure unit
      
    confirmAndSaveLocFlow :: SearchLocationScreenState -> FlowBT String Unit
    confirmAndSaveLocFlow state = do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      let saveFavouriteCard = state.data.saveFavouriteCard
          selectedItem = saveFavouriteCard.selectedItem 
          tag = case (toLower saveFavouriteCard.tag) of 
            "work" -> "Work"
            "home" -> "Home"
            _ -> saveFavouriteCard.tag 
      void $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      { lat, long, addressComponents } <-  case selectedItem.lat , selectedItem.lon of 
          Nothing , Nothing -> fetchLatLong selectedItem tag
          _       , _ -> pure $ {lat : selectedItem.lat, long : selectedItem.lon, addressComponents : []}

      when (isJust lat && isJust long) $ do
        resp <- Remote.addSavedLocationBT (encodeAddressDescription saveFavouriteCard.address tag selectedItem.placeId lat long addressComponents )
        void $ pure $ toast $ getString STR.FAVOURITE_ADDED_SUCCESSFULLY 
      savedLocResp <- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocResp of 
        Right (SavedLocationsListRes savedLocs) -> do 
          let updatedLocList = getUpdatedLocationList state.data.locationList selectedItem.placeId
              savedLocList = AddNewAddress.savedLocTransformer savedLocs.list
          modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState{data{locationList = updatedLocList}})
          updateSavedLocations savedLocList
          searchLocationFlow
        Left (err) -> searchLocationFlow

    addFavLocFlow :: SearchLocationScreenState -> String -> FlowBT String Unit
    addFavLocFlow state tag = do 
      modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
      savedLoc <- fetchGlobalSavedLocations
      (GlobalState globalState) <- getState
      let recents = globalState.globalProps.recentSearches
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
        addNewAddressScreen
          { props
            { showSavePlaceView = false
            , fromScreen = Screen.getScreen Screen.SEARCH_LOCATION_SCREEN
            , editLocation = false
            , editSavedLocation = false
            , isLocateOnMap = false
            , isBtnActive = true
            , isSearchedLocationServiceable = true
            , tagExists = false
            , placeNameExists = false }
          , data
            { addressSavedAs = ""
            , placeName = ""
            , savedLocations = savedLoc
            , locationList = recents
            , recentSearchs{predictionArray = recents}
            , selectedTag = getCardType tag
            , savedTags = getExistingTags savedLoc
            , address = ""
            , activeIndex = case tag of
                              "HOME_TAG" -> Just 0
                              "WORK_TAG" -> Just 1
                              _        -> Just 2  }})
      addNewAddressScreenFlow ""

predictionClickedFlow :: LocationListItemState -> SearchLocationScreenState -> FlowBT String Unit
predictionClickedFlow prediction state = do 
  modifyScreenState $ SearchLocationScreenStateType (\_ -> state)
  if state.props.actionType == AddingStopAction then do 
    void $ lift $ lift $ loaderText (getString STR.LOADING) (getString STR.PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
    void $ lift $ lift $ toggleLoader true
    let {lat, lon, placeId} = {lat : fromMaybe 0.0 prediction.lat, lon : fromMaybe 0.0 prediction.lon, placeId : prediction.placeId}
    (GetPlaceNameResp resp) <- getPlaceNameResp (prediction.title <> ", " <> prediction.subTitle) placeId lat lon prediction
    let (PlaceName placeName) = maybe SearchLocationScreenData.dummyLocationName identity $ head resp
        (LatLong placeLatLong) = placeName.location
        {placeLat , placeLon} = {placeLat : placeLatLong.lat, placeLon : placeLatLong.lon}
    maybe 
      (pure unit) 
      (\ currTextField -> onPredictionClicked placeLat placeLon currTextField prediction) 
      state.props.focussedTextField
    else if state.props.actionType == MetroStationSelectionAction then do
      if isJust state.data.srcLoc && isJust state.data.destLoc then do
        -- TicketBookingScreenStateType
        let src = maybe "" (\(loc) -> loc.address) state.data.srcLoc
            dest = maybe "" (\(loc) -> loc.address) state.data.destLoc
            srcCode = maybe "" (\(loc) -> loc.stationCode) state.data.srcLoc
            destCode = maybe "" (\(loc) -> loc.stationCode) state.data.destLoc
        modifyScreenState $ MetroTicketBookingScreenStateType (\state -> state{data{ srcLoc = src, destLoc = dest , srcCode = srcCode, destCode = destCode}, props {isButtonActive = true}})
        metroTicketBookingFlow
      else do
        void $ pure $ spy "else do" state
        if state.props.focussedTextField == Just SearchLocPickup then 
          void $ pure $ showKeyboard (getNewIDWithTag (show SearchLocPickup))
          else do
          void $ pure $ showKeyboard (getNewIDWithTag (show SearchLocDrop))
        modifyScreenState $ SearchLocationScreenStateType (\state -> state{data{ updatedMetroStations = state.data.metroStations}})
        searchLocationFlow
    else do 
      searchLocationFlow

  where 

    onPredictionClicked :: Number -> Number -> SearchLocationTextField -> LocationListItemState -> FlowBT String Unit
    onPredictionClicked placeLat placeLon currTextField prediction = do
      {pickUpPoints , locServiceable, city, geoJson, specialLocCategory} <- getServiceability placeLat placeLon currTextField
      let focussedField = show currTextField
      pure $ setText (getNewIDWithTag focussedField) $ prediction.description
      if locServiceable then do 
        let {sourceLoc, destinationLoc, updatedState} = mkSrcAndDestLoc placeLat placeLon state currTextField prediction city
        liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = placeLat, lon = placeLon, geoJson = geoJson, points = pickUpPoints, labelId = getNewIDWithTag "LocateOnMapSLSPin" }
        modifyScreenState 
          $ SearchLocationScreenStateType 
              (\slsScreen -> slsScreen{ props {searchLocStage = ConfirmLocationStage}
                              , data { srcLoc = sourceLoc, destLoc = destinationLoc, latLonOnMap = updatedState, confirmLocCategory = getZoneType specialLocCategory }
                              })
        void $ lift $ lift $ toggleLoader false
        updateCachedLocation prediction placeLat placeLon state locServiceable
        searchLocationFlow 
        else do 
          modifyScreenState $ SearchLocationScreenStateType (\state -> state{props{ locUnserviceable = true}})
          void $ lift $ lift $ toggleLoader false
          searchLocationFlow

    mkSrcAndDestLoc :: Number -> Number -> SearchLocationScreenState -> SearchLocationTextField -> LocationListItemState -> Maybe String -> {sourceLoc :: Maybe LocationInfo, destinationLoc :: Maybe LocationInfo, updatedState :: LocationInfo}
    mkSrcAndDestLoc placeLat placeLon state currTextField prediction city = 
      let updatedState = {lat : Just placeLat, lon : Just placeLon, city : Just (getCityNameFromCode city ), addressComponents : encodeAddress prediction.description [] Nothing placeLat placeLon , placeId : prediction.placeId, address : prediction.description, metroInfo : Nothing, stationCode : ""} 
          sourceLoc = if currTextField == SearchLocPickup then Just updatedState else state.data.srcLoc
          destinationLoc = if currTextField == SearchLocPickup then state.data.destLoc else Just updatedState
      in {sourceLoc, destinationLoc, updatedState}

    updateCachedLocation :: LocationListItemState -> Number -> Number -> SearchLocationScreenState -> Boolean -> FlowBT String Unit
    updateCachedLocation prediction placeLat placeLon state locServiceable = do 
      saveToRecents prediction placeLat placeLon locServiceable
      let { currLat , currLon } = maybe ({currLat : 0.0 , currLon : 0.0}) (\ loc -> {currLat : fromMaybe 0.0 loc.lat, currLon : fromMaybe 0.0 loc.lon}) state.data.currentLoc  
          { srcLat , srcLon } = maybe ({srcLat : currLat , srcLon : currLon}) (\ loc -> {srcLat : fromMaybe currLat loc.lat, srcLon : fromMaybe currLon loc.lon}) state.data.srcLoc  
      when (state.props.focussedTextField == Just SearchLocDrop) $ do 
        setSuggestionsMapInLocal prediction srcLat srcLon placeLat placeLon locServiceable state.appConfig
      pure unit
      
getServiceability :: Number -> Number -> SearchLocationTextField -> FlowBT String {pickUpPoints :: Array Location, locServiceable :: Boolean, city :: Maybe String, geoJson :: String, specialLocCategory :: String}
getServiceability placeLat placeLon currTextField = do
  (ServiceabilityRes pickUpServiceability) <- Remote.originServiceabilityBT $ Remote.makeServiceabilityReq placeLat placeLon
  (ServiceabilityResDestination dropServiceability) <- Remote.destServiceabilityBT $ Remote.makeServiceabilityReqForDest placeLat placeLon
  let serviceabilityRes = if currTextField == SearchLocPickup then pickUpServiceability else dropServiceability
      city = serviceabilityRes.city
      locServiceable = serviceabilityRes.serviceable
      (SpecialLocation specialLoc) = fromMaybe HomeScreenData.specialLocation (serviceabilityRes.specialLocation)
      geoJson = fromMaybe "" specialLoc.geoJson
      specialLocCategory = specialLoc.category
      pickUpPoints = mapSpecialZoneGates specialLoc.gatesInfo
  pure $ {pickUpPoints , locServiceable, city, geoJson, specialLocCategory}

fetchLatLong :: LocationListItemState -> String -> FlowBT String {lat :: Maybe Number, long :: Maybe Number, addressComponents :: Array AddressComponents}
fetchLatLong selectedItem tag = do
  case selectedItem.placeId of 
    Just placeId -> do
      (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedItem.title <> "," <> selectedItem.subTitle) (Just placeId) (fromMaybe 0.0 selectedItem.lat) (fromMaybe 0.0 selectedItem.lon) selectedItem
      let (PlaceName placeName) =  maybe SearchLocationScreenData.dummyLocationName identity $ head placeNameResp
          (LatLong placeLatLong) = placeName.location
      pure {lat : Just placeLatLong.lat, long : Just placeLatLong.lon, addressComponents : placeName.addressComponents}
    Nothing -> pure {lat : Nothing, long : Nothing, addressComponents : []}

getDistDiff :: Array LocationListItemState -> Number -> Number -> String -> FlowBT String Unit
getDistDiff savedLoc lat lon placeId = do
  let distanceInfo = getDistInfo savedLoc "" lat lon placeId
  case distanceInfo.locExistsAs of
    "" ->  modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState{props{showSaveFavCard = true}})
    _  -> do
            void $ pure $ toast  (getString STR.ALREADY_EXISTS)
            modifyScreenState $ SearchLocationScreenStateType (\searchLocScreenState -> searchLocScreenState{data{saveFavouriteCard{selectedItem = locationListStateObj}}})
  searchLocationFlow

updateSavedLocations :: Array LocationListItemState -> FlowBT String Unit
updateSavedLocations savedLocs = do 
  modifyScreenState $ 
    GlobalPropsType 
      ( \globalProps -> globalProps { savedLocations = savedLocs} )


fetchGlobalSavedLocations :: FlowBT String (Array LocationListItemState) 
fetchGlobalSavedLocations = do
  (GlobalState globalState) <- getState
  pure $ (globalState.globalProps.savedLocations)

activateSafetyScreenFlow :: FlowBT String Unit
activateSafetyScreenFlow = do
  flow <- UI.activateSafetyScreen
  case flow of
    ActivateSafetyScreen.GoBack state -> homeScreenFlow
    ActivateSafetyScreen.GoToEmergencyContactScreen state -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen{props{fromSosFlow = true, appName = state.props.appName}, data {emergencyContactsList = state.data.emergencyContactsList}})
      emergencyScreenFlow
    ActivateSafetyScreen.CreateSos state isPoliceFlow -> do
      let rideId = case state.data.lastRideDetails of
                    Nothing -> state.data.rideId
                    Just ride -> ride.rideId
          flowType = if isPoliceFlow then "Police" else "SafetyFlow"
      (GlobalState gState) <- getState
      if state.props.showTestDrill
        then do
          void $ lift $ lift $ Remote.createMockSos (gState.homeScreen.props.currentStage == RideStarted) false
          void $ pure $ cleverTapCustomEvent "ny_user_test_drill"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{sosBannerType = Nothing}})
          pure unit
        else do
          (UserSosRes res) <- Remote.userSosBT $ Remote.makeUserSosReq (Remote.createUserSosFlow flowType "") rideId state.props.reportPastRide
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{contactList = Nothing}})
          if (not isPoliceFlow) then do
            modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{data {sosId = res.sosId}})
            void $ pure $ cleverTapCustomEventWithParams "ny_user_sos_activated" "current_time" (getCurrentUTC "")
          else 
            void $ pure $ cleverTapCustomEvent "ny_user_call_police_activated"
          setValueToLocalStore IS_SOS_ACTIVE "true"
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{props{shouldCallAutomatically = true}, data{sosType = Just $ if isPoliceFlow then Police else SafetyFlow}})
      if isPoliceFlow then activateSafetyScreenFlow else sosActiveFlow
    ActivateSafetyScreen.GoToSosScreen state -> sosActiveFlow
    ActivateSafetyScreen.GoToEducationScreen state -> safetyEducationFlow
    ActivateSafetyScreen.GoToIssueScreen state -> do
      let language = fetchLanguage $ getLanguageLocale languageKey 
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0" "" ""
      let getOptionsRes' = mapWithIndex (\index (Option optionObj) -> optionObj { option = (show (index + 1)) <> ". " <> (reportIssueMessageTransformer optionObj.option) }) getOptionsRes.options
          messages' = mapWithIndex (\index (Message currMessage) -> makeChatComponent' (reportIssueMessageTransformer currMessage.message) "Bot" (getCurrentUTC "") "Text" (500*(index + 1))) getOptionsRes.messages
          chats' = map (\(Message currMessage) -> Chat {chatId : currMessage.id, 
                                                        chatType : "IssueMessage", 
                                                        timestamp : (getCurrentUTC "")} )getOptionsRes.messages
      void $ pure $ cleverTapCustomEvent "ny_user_report_safety_issue_activated"
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueChatScreenData.initData { data {entryPoint = ReportIssueChatScreenData.SafetyScreen, chats = chats', tripId = Just state.data.rideId, categoryName = "Safety Related Issue", categoryId = "f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0", options = getOptionsRes', chatConfig { messages = messages' },  selectedRide = Nothing } } )
      flowRouter IssueReportChatScreenFlow
    ActivateSafetyScreen.NotifyMockDrill state -> do
      _ <- lift $ lift $ Remote.createMockSos (not $ DS.null state.data.rideId) true
      activateSafetyScreenFlow

safetySettingsFlow :: FlowBT String Unit
safetySettingsFlow = do
  flow <- UI.safetySettingsScreen
  case flow of
    SafetySettingsScreen.GoBack updatedState -> homeScreenFlow
    SafetySettingsScreen.PostEmergencySettings state -> do
      updateEmergencySettings state
      safetySettingsFlow
    SafetySettingsScreen.GoToEmergencyContactScreen updatedState -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen{props{fromSosFlow = false, appName = updatedState.props.appName}, data{emergencyContactsList = updatedState.data.emergencyContactsList}})
      emergencyScreenFlow
    SafetySettingsScreen.GoToEducationScreen updatedState -> safetyEducationFlow
    SafetySettingsScreen.GoToSetupScreen updatedState -> setupSafetySettingsFlow
    SafetySettingsScreen.GoToActivateSosScreen state -> activateSafetyScreenFlow
    SafetySettingsScreen.PostContacts state -> do
      void $ Remote.emergencyContactsBT (Remote.postContactsReq $ getDefaultPriorityList state.data.emergencyContactsList)
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen { data { emergencyContactsList = state.data.emergencyContactsList } })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{contactList = Nothing}})
      safetySettingsFlow


setupSafetySettingsFlow :: FlowBT String Unit
setupSafetySettingsFlow = do
  flow <- UI.setupSafetySettingsScreen
  logField_ <- lift $ lift $ getLogFields
  case flow of 
    SetupSafetySettingsScreen.GoBack state -> safetySettingsFlow
    SetupSafetySettingsScreen.PostContacts state  -> do
      void $ Remote.emergencyContactsBT (Remote.postContactsReq $ getDefaultPriorityList state.data.emergencyContactsList)
      if state.props.showInfoPopUp 
        then pure $ toast $ getString STR.CONTACT_REMOVED_SUCCESSFULLY
        else pure $ toast $ getString STR.EMERGENCY_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> state{props{showInfoPopUp = false}})
      setupSafetySettingsFlow
    SetupSafetySettingsScreen.Refresh state  -> pure unit
    SetupSafetySettingsScreen.PostEmergencySettings state  -> do
        updateEmergencySettings state
        void $ liftFlowBT $ logEvent logField_ "finish_safety_setup_tapped"
        safetySettingsFlow
    SetupSafetySettingsScreen.GoToEmergencyContactScreen state  -> do
      modifyScreenState $ EmergencyContactsScreenStateType (\emergencyContactScreen -> emergencyContactScreen{props{fromSosFlow = false, appName = state.props.appName},data {emergencyContactsList = state.data.emergencyContactsList}})
      emergencyScreenFlow

sosActiveFlow :: FlowBT String Unit
sosActiveFlow = do
  flow <- UI.sosActiveScreen
  case flow of
    SosActiveScreen.UpdateAsSafe state -> do
      let sosId = if state.props.showTestDrill then "mock-sos" else state.data.sosId
      void $ lift $ lift $ Remote.markRideAsSafe sosId state.props.showTestDrill state.props.reportPastRide
      when (not $ DS.null state.data.rideId) $ do
        void $ pure $ cleverTapCustomEventWithParams "ny_user_sos_marked_safe" "current_time" (getCurrentUTC "")
        pure unit
      modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyScreen -> nammaSafetyScreen{data{sosId = ""}})
      setValueToLocalStore IS_SOS_ACTIVE "false"
      homeScreenFlow
    SosActiveScreen.UpdateAction state comment -> do
      res <- Remote.userSosStatusBT state.data.sosId (Remote.makeSosStatus "Pending" comment)
      sosActiveFlow
    SosActiveScreen.GoToEducationScreen state -> safetyEducationFlow
    SosActiveScreen.GoBack state -> homeScreenFlow
    _ -> sosActiveFlow
  pure unit

safetyEducationFlow :: FlowBT String Unit
safetyEducationFlow = do
  let videoList = RC.safetyVideoConfigData (DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION) $ fetchLanguage $ getLanguageLocale languageKey
  modifyScreenState $ NammaSafetyScreenStateType (\safetyScreen -> safetyScreen { data { videoList = if null safetyScreen.data.videoList then videoList else safetyScreen.data.videoList }})
  void $ pure $ cleverTapCustomEvent "ny_user_safety_learn_more_clicked"
  flow <- UI.safetyEducationScreen
  case flow of
    SafetyEducationScreen.Refresh _ -> safetyEducationFlow
    SafetyEducationScreen.GoToHomeScreen _ -> currentFlowStatus
    _ -> safetyEducationFlow
  pure unit

updateEmergencySettings :: ST.NammaSafetyScreenState -> FlowBT String Unit
updateEmergencySettings state = do
  let
    req =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts: Just state.data.shareToEmergencyContacts
        , shareTripWithEmergencyContactOption: Just state.data.shareTripWithEmergencyContactOption
        , nightSafetyChecks: Just state.data.nightSafetyChecks
        , hasCompletedSafetySetup: Just true
        }

    wasSetupAlreadyDone = state.data.hasCompletedSafetySetup
  void $ lift $ lift $ Remote.updateEmergencySettings req
  if not wasSetupAlreadyDone 
    then do
      pure $ toast $ getString STR.NAMMA_SAFETY_IS_SET_UP
      void $ Remote.emergencyContactsBT $ Remote.postContactsReq $ map (\item -> item{enableForFollowing = true}) state.data.emergencyContactsList
    else pure unit
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { props
                { sosBannerType = if not state.data.hasCompletedMockSafetyDrill 
                                    then Just ST.MOCK_DRILL_BANNER 
                                    else Nothing
                }
              , data { settingSideBar { hasCompletedSafetySetup = true } }
              }
        )
  
checkForSpecialZoneAndHotSpots :: HomeScreenState -> ServiceabilityRes -> Number -> Number -> FlowBT String Unit
checkForSpecialZoneAndHotSpots state (ServiceabilityRes serviceabilityResp) lat lon = do
  let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation serviceabilityResp.specialLocation
      pickUpPoints = mapSpecialZoneGates srcSpecialLocation.gatesInfo
      geoJson = transformGeoJsonFeature srcSpecialLocation.geoJson srcSpecialLocation.gatesInfo
      zoneType = getZoneType srcSpecialLocation.category
      canUpdateHotSpots = maybe true (\point -> (getDistanceBwCordinates lat lon point.lat point.lng) * 1000.0 > state.data.config.mapConfig.locateOnMapConfig.hotSpotConfig.updateHotSpotOutSideRange) state.props.hotSpot.centroidPoint
      locationName = srcSpecialLocation.locationName

  if not (DS.null geoJson) && not (null pickUpPoints) then do
    if (geoJson /= state.data.polygonCoordinates || pickUpPoints /= state.data.nearByPickUpPoints) then do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data  { polygonCoordinates = geoJson
                                                                                , nearByPickUpPoints = pickUpPoints}
                                                                        , props { city = getCityNameFromCode serviceabilityResp.city
                                                                                , isSpecialZone =  not (DS.null geoJson) 
                                                                                , confirmLocationCategory = zoneType
                                                                                , hotSpot{ centroidPoint = Nothing } }})
      void $ pure $ removeAllPolylines ""
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat
                                                              , lon = lon
                                                              , geoJson = geoJson
                                                              , points = pickUpPoints
                                                              , zoomLevel = zoomLevel
                                                              , labelId = getNewIDWithTag "LocateOnMapPin"
                                                              , locationName = locationName
                                                              , specialZoneMarkerConfig{ labelImage = zoneLabelIcon zoneType }}
      homeScreenFlow
    else pure unit
  else if not (null serviceabilityResp.hotSpotInfo) && canUpdateHotSpots && state.data.config.mapConfig.locateOnMapConfig.hotSpotConfig.enableHotSpot then do
    let points = filterHotSpots state serviceabilityResp.hotSpotInfo lat lon
    if (state.data.nearByPickUpPoints /= points && not (null points)) then do
      void $ pure $ removeAllPolylines ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data {  polygonCoordinates = ""
                                                                                , nearByPickUpPoints = points}
                                                                        , props { isSpecialZone = false 
                                                                                , defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (points!!0)).place
                                                                                , confirmLocationCategory = zoneType
                                                                                , hotSpot{ centroidPoint = Just { lat : lat, lng : lon } } }})
      liftFlowBT $ runEffectFn1 locateOnMap locateOnMapConfig { points = points, zoomLevel = zoomLevel, labelId = getNewIDWithTag "LocateOnMapPin"}
    else pure unit
  else pure unit

firstRideCompletedEvent :: String -> FlowBT String Unit
firstRideCompletedEvent str = do
  logField_ <- lift $ lift $ getLogFields
  let appName = fromMaybe "" $ runFn3 getAnyFromWindow "appName" Nothing Just
      eventPrefix = case appName of
                  "Mana Yatri" -> "my_"
                  "Yatri" -> "y_"
                  "Namma Yatri" -> "ny_"
                  _ -> fromMaybe "" $ (DS.split (DS.Pattern " ") appName) !! 0
      firstRideEventCheck = getValueToLocalStore CUSTOMER_FIRST_RIDE
  if firstRideEventCheck == "false" then do
    let clientId = getValueToLocalStore CUSTOMER_CLIENT_ID
    rideBookingListResponse <- lift $ lift $ HelpersAPI.callApi $ Remote.makeRideBookingListWithStatus "2" "0" "COMPLETED" (Just clientId)
    case rideBookingListResponse of
      Right (RideBookingListRes  listResp) -> do
        let arraySize = Arr.length listResp.list
        if (arraySize == 1) then do
          void $ liftFlowBT $ logEvent logField_ $ eventPrefix <> "user_first_ride_completed"
          setValueToLocalStore CUSTOMER_FIRST_RIDE "true"
        else if arraySize > 1 then do
          setValueToLocalStore CUSTOMER_FIRST_RIDE "true"
        else setValueToLocalStore CUSTOMER_FIRST_RIDE "false"
      Left (err) -> pure unit
  else pure unit