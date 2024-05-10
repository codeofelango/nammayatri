{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Accessor (_estimatedFare, _estimateId, _vehicleVariant, _status, _estimateFareBreakup, _title, _priceWithCurrency, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _selectedQuotes, _specialLocationTag, _contents, _toLocation, _lat, _lon, _otpCode, _list)
import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), OptionButtonList, Payload(..), RateCardType(..), FeedbackAnswer(..), ProviderType(..))
import Components.Banner as Banner
import Components.MessagingView as MessagingView
import Components.MessagingView.Controller as MessagingView
import Components.ChooseVehicle as ChooseVehicleController
import Components.ChooseYourRide as ChooseYourRide
import Components.ChooseYourRide.Controller as ChooseYourRideController
import Components.DriverInfoCard.Controller as DriverInfoCardController
import Components.EmergencyHelp as EmergencyHelpController
import Components.ErrorModal.Controller as ErrorModalController
import Components.FavouriteLocationModel as FavouriteLocationModelController
import Components.GenericHeader.Controller as GenericHeaderController
import Components.LocationListItem.Controller as LocationListItemController
import Components.LocationTagBar as LocationTagBarController
import Components.LocationTagBarV2 as LocationTagBarV2Controller
import Components.MenuButton as MenuButton
import Components.MenuButton as MenuButton
import Components.RideCompletedCard.Controller as RideCompletedCard
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PopUpModal.Controller as PopUpModal
import Components.PricingTutorialModel.Controller as PricingTutorialModelController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.TipsView as TipsView
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.QuoteListItem.Controller as QuoteListItemController
import Components.QuoteListModel.Controller as QuoteListModelController
import Components.QuoteListModel.View (dummyQuoteList)
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.SaveFavouriteCard as SaveFavouriteCardController
import Components.SavedLocationCard.Controller as SavedLocationCardController
import Components.SearchLocationModel.Controller as SearchLocationModelController
import Components.SelectListModal.Controller as CancelRidePopUp
import Components.SettingSideBar.Controller as SettingSideBarController
import Components.SourceToDestination.Controller as SourceToDestinationController
import Components.Referral as ReferralComponent
import Constants (defaultDensity, languageKey)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array ((!!), filter, null, any, snoc, length, head, last, sortBy, union, elem, findIndex, reverse, sortWith, foldl, index, mapWithIndex, find, updateAt, insert, delete, tail)
import Data.Function.Uncurried (runFn3)
import Data.Int (toNumber, round, fromString, fromNumber, ceil)
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString, round) as NUM
import Data.String as STR
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons
import Engineering.Helpers.Events as Events
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey, emChatSuggestion, chatSuggestion)
import Foreign (unsafeToForeign)
import Foreign.Class (encode)
import Helpers.Utils (addToRecentSearches, getCurrentLocationMarker, getDistanceBwCordinates, getLocationName, getScreenFromStage, getSearchType, parseNewContacts, performHapticFeedback, setText, terminateApp, withinTimeRange, toStringJSON, secondsToHms, updateLocListWithDistance, getPixels, getDeviceDefaultDensity, getDefaultPixels, getAssetsBaseUrl, getCityConfig)
import JBridge (addMarker, animateCamera, currentPosition, exitLocateOnMap, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getCurrentPosition, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, minimizeApp, openNavigation, openUrlInApp, removeAllPolylines, removeMarker, requestKeyboardShow, requestLocation, shareTextMessage, showDialer, toast, toggleBtnLoader, goBackPrevWebPage, stopChatListenerService, sendMessage, getCurrentLatLong, isInternetAvailable, emitJOSEvent, startLottieProcess, getSuggestionfromKey, scrollToEnd, lottieAnimationConfig, methodArgumentCount, getChatMessages, scrollViewFocus, getLayoutBounds, updateInputString, checkAndAskNotificationPermission, locateOnMapConfig, addCarouselWithVideoExists, pauseYoutubeVideo, cleverTapCustomEvent, getKeyInSharedPrefKeys, generateSessionId, enableMyLocation)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppTextInput, trackAppScreenEvent, logInfo, logStatus)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Applicative, class Show, Unit, Ordering, bind, compare, discard, map, negate, pure, show, unit, not, ($), (&&), (-), (/=), (<>), (==), (>), (||), (>=), void, (<), (*), (<=), (/), (+), when, (<<<))
import Control.Monad (unless)
import Presto.Core.Types.API (ErrorResponse)
import PrestoDOM (BottomSheetState(..), Eval, update, ScrollState(..), Visibility(..), continue, continueWithCmd, defaultPerformLog, exit, payload, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (encodeAddress, getAddressFromBooking, decodeAddress, DecodeAddress(..), emergencyContactInitialChatSuggestionId)
import Constants (defaultDensity)
import Screens (ScreenName(..), getScreen)
import Screens.AddNewAddressScreen.Controller (validTag, getSavedTagsFromHome)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyZoneType)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getEstimateList, getQuoteList, getSpecialZoneQuotes, transformContactList, getNearByDrivers, dummyEstimateEntity)
import Screens.RideBookingFlow.HomeScreen.Config
import Screens.SuccessScreen.Handler as UI
import Screens.Types (CallType(..), CardType(..), CurrentLocationDetails, CurrentLocationDetailsWithDistance(..), HomeScreenState, LocationItemType(..), LocationListItemState, PopupType(..), RatingCard, SearchLocationModelType(..), SearchResultType(..), SheetState(..), SpecialTags, Stage(..), TipViewStage(..), ZoneType(..), Trip, BottomNavBarIcon(..), City(..), ReferralStatus(..), NewContacts(..), City(..))
import Services.API (EstimateAPIEntity(..), FareRange, GetDriverLocationResp, GetQuotesRes(..), GetRouteResp, LatLong(..), OfferRes, PlaceName(..), QuoteAPIEntity(..), RideBookingRes(..), SelectListRes(..), SelectedQuotes(..), RideBookingAPIDetails(..), GetPlaceNameResp(..), RideBookingListRes(..), FollowRideRes(..), Followers(..))
import Services.Backend as Remote
import Services.Config (getDriverNumber, getSupportNumber)
import Storage (KeyStore(..), isLocalStageOn, updateLocalStage, getValueToLocalStore, setValueToLocalStore, getValueToLocalNativeStore, setValueToLocalNativeStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Types.App (defaultGlobalState)
import Screens.RideBookingFlow.HomeScreen.Config (setTipViewData, reportIssueOptions, metersToKm, safetyIssueOptions)
import Screens.Types (TipViewData(..) , TipViewProps(..), RateCardDetails, PermissionScreenStage(..), SuggestionsMap(..), SosBannerType(..), ReferralType(..), ReferralStage(..))
import Screens.Types (AutoCompleteReqType(..), LocationSelectType(..)) as ST
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import PrestoDOM.Properties (sheetState) as PP
import Screens.RideBookingFlow.HomeScreen.Config(reportIssueOptions)
import Data.Function (const)
import Data.List ((:))
import Common.Resources.Constants (zoomLevel, pickupZoomLevel)
import Screens.RideBookingFlow.HomeScreen.Config
import Data.Function.Uncurried
import Data.Function.Uncurried (Fn3, runFn3, Fn1, runFn1)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Timers (clearTimerWithId)
import Mobility.Prelude (boolToInt, toBool)
import SuggestionUtils
import Data.Tuple (Tuple(..))
import PrestoDOM.Core (getPushFn)
import Components.BannerCarousel as BannerCarousel
import PrestoDOM.List
import PrestoDOM.Core
import Locale.Utils (getLanguageLocale)
import RemoteConfig as RC
import Screens.RideBookingFlow.HomeScreen.BannerConfig (getBannerConfigs, getDriverInfoCardBanners)
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import DecodeUtil (getAnyFromWindow)
import JBridge as JB
import Helpers.SpecialZoneAndHotSpots (zoneLabelIcon,getSpecialTag)
import Engineering.Helpers.Utils as EHU
import Components.ServiceTierCard.View as ServiceTierCard
import Components.ProviderModel as PM
import Common.Types.App as CTP

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog
  -- performLog action appId = case action of
  --   AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
  --   BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
  --   CancelSearch -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancel_search"
  --   RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_location"
  --   SidebarCloseAnimationCompleted -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "side_bar_close"
  --   OpenSettings -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_settings"
  --   OpenPricingTutorial -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_pricing_tutorial"
  --   OpenSearchLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "open_search_modal"
  --   UpdateSource lat lon name -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_source_address"
  --   HideLiveDashboard val -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "hide_live_stats_dashboard"
  --   LiveDashboardAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "live_Dashboard_action"
  --   PrimaryButtonActionController act -> case act of
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "onclick"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "no_action"
  --   SettingSideBarActionController act -> case act of
  --     SettingSideBarController.PastRides -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "past_rides"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnHelp -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_help"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.ChangeLanguage -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "change_language"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToAbout -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_about"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.EditProfile -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "edit_profile"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnClosed -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_closed"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnClose -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_close"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.OnLogout -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "on_logout"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.ShareAppLink -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "share_app_link"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToFavourites -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_favourites"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToMyProfile -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_profile"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToEmergencyContacts -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_emergency_contacts_onclick"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.LiveStatsDashboard -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_live_stats_dashboard"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.GoToMyTickets -> do
  --       trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "go_to_my_tickets"
  --       trackAppEndScreen appId (getScreen HOME_SCREEN)
  --     SettingSideBarController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "setting_side_bar" "no_action"
  --   PricingTutorialModelActionController (PricingTutorialModelController.Close) -> trackAppActionClick appId (getScreen HOME_SCREEN) "pricing_tutorial" "close_icon"
  --   SearchLocationModelActionController act -> case act of
  --     SearchLocationModelController.LocationListItemActionController act -> case act of
  --       LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item"
  --       LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "location_list_item_onclick_current_location"
  --       LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "favourite"
  --     SearchLocationModelController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "primary_button_no_action"
  --     SearchLocationModelController.SourceChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_changed"
  --     SearchLocationModelController.DestinationChanged input -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_changed"
  --     SearchLocationModelController.EditTextFocusChanged textType -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "edit_text_focus_changed"
  --     SearchLocationModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "go_back"
  --     SearchLocationModelController.SetCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location"
  --     SearchLocationModelController.SetLocationOnMap -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_location_on_map"
  --     SearchLocationModelController.UpdateSource lat lng name -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "set_current_location_update_source"
  --     SearchLocationModelController.SourceClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "source_clear"
  --     SearchLocationModelController.DestinationClear -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "destination_clear_options"
  --     SearchLocationModelController.DebounceCallBack searchString arg -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "debounce_callback_search"
  --     SearchLocationModelController.UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "update_current_location"
  --     SearchLocationModelController.RecenterCurrentLocation -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "recenter_location"
  --     SearchLocationModelController.SavedAddressClicked act -> case act of
  --       LocationTagBarController.TagClick savedAddressType arrItem -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
  --     SearchLocationModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "search_location_modal" "no_action"
  --   QuoteListModelActionController act -> case act of
  --     QuoteListModelController.QuoteListItemActionController act -> case act of
  --       QuoteListItemController.Click quote -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_click"
  --       QuoteListItemController.CountDown seconds status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "quote_list_item_count_down"
  --       QuoteListItemController.ConfirmRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_ride"
  --       QuoteListItemController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
  --       QuoteListItemController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
  --     QuoteListModelController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "confirm_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.GoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "on_goback"
  --     QuoteListModelController.CancelAutoAssigning -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "auto_assign_cancel"
  --     QuoteListModelController.HomeButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "home_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.TryAgainButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "try_again_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --     QuoteListModelController.HidePopUp -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "hide_popup"
  --     QuoteListModelController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "no_action"
  --     QuoteListModelController.TipBtnClick arg1 arg2-> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "tip_button_click"
  --     QuoteListModelController.TipViewPrimaryButtonClick act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "quote_list_modal" "primary_button_no_action"
  --   DriverInfoCardActionController act -> case act of
  --     DriverInfoCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "no_action"
  --     DriverInfoCardController.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "call_primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "primary_button_no_action"
  --     DriverInfoCardController.SourceToDestinationAC  act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "source_to_destination"
  --     DriverInfoCardController.CancelRide infoCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "cancel_ride"
  --     DriverInfoCardController.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "driver_info_card" "location_tracking"
  --     DriverInfoCardController.MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "open_in_app_messaging"
  --     DriverInfoCardController.OnNavigate -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "on_navigate"
  --     DriverInfoCardController.CallDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "call_driver"
  --     DriverInfoCardController.OnNavigateToZone -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "on_navigate_to_zone"
  --     DriverInfoCardController.ToggleBottomSheet -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "toggle_bottom_sheet"
  --     DriverInfoCardController.CollapseBottomSheet -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "driver_info_card" "collapse_bottom_sheet"
  --   UpdateLocation key lat lon ->  trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_location"
  --   CancelRidePopUpAction act -> case act of
  --     CancelRidePopUp.Button1 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_declined"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
  --     CancelRidePopUp.Button2 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "cancel_ride_accepted"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "primary_button_no_action"
  --     CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "update_index"
  --     CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "go_back"
  --     CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "clear_options"
  --     CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "cancelling_reason_text_changed" "cancel_ride_popup"
  --     CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride_popup" "no_action"
  --   PopUpModalAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_cancel"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "secondary_text_click"
  --   RatingCardAC act -> case act of
  --     RatingCard.Rating index -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "star"
  --     RatingCard.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "primary_button_no_action"
  --     RatingCard.FeedbackChanged value -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "feedback_changed"
  --     RatingCard.BackPressed -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "rating_card" "back_pressed"
  --     RatingCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "no_action"
  --     RatingCard.SelectPill arg1 arg2 -> trackAppActionClick appId (getScreen HOME_SCREEN) "rating_card" "select_pill"
  --   CloseLocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_location_tracking"
  --   StartLocationTracking item -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "start_location_tracking"
  --   DistanceOutsideLimitsActionController act -> case act of
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location"
  --     PopUpModal.OnButton1Click -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "change_drop_location_cancel"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_distance_outside_limit" "secondary_text_click"
  --   ShortDistanceActionController act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "book_ride"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "go_back"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_short_distance_action" "secondary_text_click"
  --   SourceUnserviceableActionController act -> case act of
  --     ErrorModalController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unserviceable_error" "primary_button_change_location"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
  --   GoBackToSearchLocationModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_back_search_location_modal"
  --   SkipButtonActionController act -> case act of
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "primary_button" "skip"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "source_unservice_error_modal" "primary_button_no_action"
  --   EstimateChangedPopUpController act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "go_to_home"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "continue"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_estimate_changed" "secondary_text_click"
  --   RateCardAction act -> case act of
  --     RateCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "close_click"
  --     RateCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card" "back_click"
  --     RateCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "no_action"
  --     RateCard.GoToDefaultStart -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "back_click"
  --     RateCard.GoToDriverAddition -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_driver_addition"
  --     RateCard.GoToFareUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_fare_update"
  --     RateCard.PrimaryButtonAC act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_primary_button" "on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_primary_button" "no_action"
  --     RateCard.GoToWaitingCharges -> trackAppActionClick appId (getScreen HOME_SCREEN) "rate_card_action" "go_to_waiting_charges"
  --   ShowRateCard -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_rate_card"
  --   PredictionClickedAction act -> case act of
  --     LocationListItemController.OnClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction"
  --     LocationListItemController.FavClick item -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_list_item" "prediction_fav_click"
  --     LocationListItemController.SelectedCurrentLocation lat lng name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "location_list_item" "selected_current_location"
  --   SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem) -> trackAppActionClick appId (getScreen HOME_SCREEN) "location_tag_bar" "tag"
  --   FavouriteLocationModelAC act -> case act of
  --     FavouriteLocationModelController.GenericHeaderAC act -> case act of
  --       GenericHeaderController.PrefixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_back_icon"
  --       GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "generic_header_forward_icon"
  --     FavouriteLocationModelController.FavouriteLocationAC act -> case act of
  --       SavedLocationCardController.CardClicked item -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "saved_loc_card"
  --       SavedLocationCardController.DeleteLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "delete_location"
  --       SavedLocationCardController.EditLocation act -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "edit_location_modal"
  --       SavedLocationCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "fav_location_modal" "no_action"
  --     FavouriteLocationModelController.ErrorModalAC act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "fav_location_modal" "error_modal_action"
  --   SaveFavouriteCardAction act -> case act of
  --     SaveFavouriteCardController.OnClose -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "on_close_click"
  --     SaveFavouriteCardController.SaveFavourite -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "save_fav"
  --     SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "primary_edit_text"
  --     SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.FocusChanged id) -> trackAppTextInput appId (getScreen HOME_SCREEN) "save_fav_card_text_changed" "focus_changed"
  --     SaveFavouriteCardController.TagSelected act -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "tag_selected"
  --     SaveFavouriteCardController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "save_fav_card" "no_action"
  --   UpdateCurrentLocation lat lng -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "update_current_location"
  --   PopUpModalShareAppAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_share_app" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_share_app" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_share_app" "secondary_text_click"
  --   CallSupportAction act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_contact_support" "secondary_text_click"
  --   ContinueWithoutOffers resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_without_offers"
  --   CheckBoxClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_box_click"
  --   TagClick savedAddressType arrItem -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "tag_click"
  --   DriverArrivedAction driverArrivalTime -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "driver_arrived_action"
  --   WaitingTimeAction timerID timeInMinutes seconds -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_time_action"
  --   UpdateETA currentETA currentDistance -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_eta"
  --   EstimatesTryAgain quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "estimates_try_again"
  --   SearchExpireCountDown seconds status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_expiry_count_down"
  --   UpdateCurrentStage stage -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_stage"
  --   ExitLocationSelected item addToRecents -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "exit_location_selected"
  --   NotificationListener notificationType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_listener"
  --   GetEstimates quotesRes -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_estimates"
  --   GetRideConfirmation resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_ride_confirmation"
  --   GetQuotesList (SelectListRes resp) -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "get_quotes_list"
  --   MAPREADY key latitude longitude -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_ready"
  --   CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
  --   SourceToDestinationActionController act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "source_to_destination"
  --   TrackDriver resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "track_driver"
  --   HandleCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "handle_call_back"
  --   UpdatePickupLocation  key lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_pickup_location"
  --   ContinueCmd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "continue_cmd"
  --   Restart err -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "restart"
  --   UpdateSourceName lat lon name -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_name"
  --   RequestInfoCardAction act -> case act of
  --     RequestInfoCard.Close -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "got_it"
  --     RequestInfoCard.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "backpressed_in_screen"
  --     RequestInfoCard.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "request_info_card" "no_action"
  --   PreferencesDropDown -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "preferences_drop_down"
  --   OnIconClick autoAssign -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "close_icon_auto_assign"
  --   PopUpModalAction act -> case act of
  --       PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "on_goback"
  --       PopUpModal.OnButton2Click -> do
  --         trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "register_on_different_number"
  --         trackAppEndScreen appId (getScreen HOME_SCREEN)
  --       PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "no_action"
  --       PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_action" "image"
  --       PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_action" "primary_edit_text"
  --       PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "countdown_updated"
  --       PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
  --       PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
  --       PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "options_with_html_click"
  --       PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "secondary_text_click"
  --   ReferralFlowAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code"
  --   ReferralFlowNoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
  --   NewUser -> do
  --     trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "have_referral_code_no_action"
  --     trackAppEndScreen appId (getScreen HOME_SCREEN)
  --   MapReadyAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "map_render"
  --   TrackLiveLocationAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "track_live_location_using"
  --   LottieLoaderAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_rickshaw_processing"
  --   UpdateSourceFromPastLocations -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_source_from_past_saved_locations"
  --   UpdateLocAndLatLong lat lon-> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_current_loc_lat_and_lon"
  --   UpdateSavedLoc state -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_saved_loc"
  --   NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
  --   UpdateMessages msg sender timeStamp size -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
  --   InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
  --   RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
  --   MessagingViewActionController act -> case act of
  --     MessagingView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
  --     MessagingView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
  --     MessagingView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
  --     MessagingView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
  --     MessagingView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
  --     MessagingView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
  --   OnResumeCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "on_resume_callback"
  --   CheckFlowStatusAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "check_flow_status"
  --   GoToEditProfile -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "GoToEditProfile"
  --   HideLiveDashboard val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "hide_live_dashboard"
  --   LiveDashboardAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "LiveDashboardAction"
  --   OnResumeCallback -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "OnResumeCallback"
  --   CheckFlowStatusAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "CheckFlowStatusAction"
  --   IsMockLocation val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "IsMockLocation"
  --   MenuButtonActionController act -> case act of 
  --     MenuButtonController.OnClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "menu_button_action" "on_click"
  --   ChooseYourRideAction act -> case act of 
  --     ChooseYourRideController.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "no_action"
  --     ChooseYourRideController.ChooseVehicleAC arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "choose_vehicle"
  --     ChooseYourRideController.RadioButtonClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "CheckBoxClick"
  --     ChooseYourRideController.OnIconClick arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnIconClick"
  --     ChooseYourRideController.PreferencesDropDown -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "preferences_drop_down"
  --     ChooseYourRideController.PrimaryButtonActionController act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "choose_your_ride_action" "primary_button"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "choose_your_ride_action" "primary_button_no_action"
  --   SearchForSelectedLocation -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "search_for_saved_location"
  --   GenderBannerModal act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "gender_banner_modal" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "gender_banner_modal" "banner_no_action"
  --   CancelSearchAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "cancel"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "accept"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_search" "secondary_text_click"
  --   TriggerPermissionFlow val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "trigger_persmission_screen_flow"
  --   PopUpModalCancelConfirmationAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "secondary_text_click"
  --   ScrollToBottom -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_to_bottom"
  --   SelectButton val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "select_button"
  --   RateClick val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "rate_click"
  --   Support -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "support"
  --   IssueReportIndex val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "issue_report_index"
  --   RideDetails -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_details"
  --   TerminateApp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "terminate_app"
  --   DirectSearch -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "direct_search"
  --   ZoneTimerExpired act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_zone_timer_expired" "secondary_text_click"
  --   DisabilityBannerAC act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_banner" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_banner" "banner_no_action"
  --   DisabilityPopUpAC act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "disability_pop_up" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "disability_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "disability_pop_up" "secondary_text_click"
  --   RideCompletedAC act -> case act of 
  --     RideCompletedCard.Support -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "support_clicked"
  --     RideCompletedCard.RideDetails -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "ride_details_clicked"
  --     RideCompletedCard.SelectButton arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "select_button_click"
  --     RideCompletedCard.IssueReportIndex arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "issue_report_index"
  --     RideCompletedCard.RateClick arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen" "rate_click"
  --     RideCompletedCard.IssueReportPopUpAC act -> case act of 
  --       CancelRidePopUp.Button1 act -> case act of
  --         PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_1" "primary_button_on_click"
  --         PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_1" "primary_button_no_action"
  --       CancelRidePopUp.Button2 act -> case act of
  --         PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_2" "primary_button_on_click"
  --         PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup_2" "primary_button_no_action"
  --       CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "update_index"
  --       CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "go_back"
  --       CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "clear_options"
  --       CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "text_changed"
  --       CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_issue_report_popup" "no_action"
  --     RideCompletedCard.SkipButtonActionController act -> case act of 
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_skip_button_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_skip_button_action" "primary_button_no_action"
  --     RideCompletedCard.ContactSupportPopUpAC act -> case act of 
  --       PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "button1_click"
  --       PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "button2_click"
  --       PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "no_action"
  --       PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "image_click"
  --       PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "primary_edit_text"
  --       PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "countdown_updated"
  --       PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "tip_clicked"
  --       PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "popup_dismissed"
  --       PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "options_with_html_click"
  --       PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "secondary_text_click"
  --     RideCompletedCard.UpiQrRendered arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_screen_conatct_support_pop_up" "upi_qr_rendered"
  --     RideCompletedCard.BannerAction act -> case act of 
  --       Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_banner" "banner_on_click"
  --       Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_banner" "banner_no_action"
  --     RideCompletedCard.HelpAndSupportAC -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ride_completed_help_and_support" "help_and_support"
  --   LoadMessages -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "load_messages"
  --   KeyboardCallback val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "key_board_callback"
  --   NotifyDriverStatusCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notify_driver_status_countdown"
  --   UpdateProfileButtonAC act -> case act of 
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "update_profile_button" "primary_button_on_click"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "update_profile_button" "primary_button_no_action"
  --   SkipAccessibilityUpdateAC act -> case act of 
  --     PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "skip_accessibility_button" "primary_button_on_click"
  --     PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "skip_accessibility_button" "primary_button_no_action"
  --   SpecialZoneOTPExpiryAction arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "special_zone_otp_expiry_action"
  --   TicketBookingFlowBannerAC act -> case act of 
  --     Banner.OnClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ticket_booking_flow" "banner_on_click"
  --     Banner.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "ticket_booking_flow" "banner_no_action"
  --   WaitingInfo -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "waiting_info"
  --   ShareRide -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "share_ride"
  --   ScrollStateChanged val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_state_changed"
  --   RemoveNotification -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_notification"
  --   MessageDriver -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_driver"
  --   SendQuickMessage val -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "send_quick_message"
  --   MessageExpiryTimer seconds status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_expiry_timer"
  --   NotificationAnimationEnd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "notification_animation_end"
  --   RideSupport -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_support"
  --   OpenEmergencyHelp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_emergency_help"
  --   ShowCallDialer callType -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "show_call_dialer"
  --   CloseShowCallDialer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "close_show_call_dialer"
  --   CheckAndAskNotificationPermission -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "check_and_ask_notification_permission"
  --   OpenChatScreen -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_chat_screen"
  --   PickUpFarFromCurrentLocAC act -> case act of
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "book_ride"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "go_back"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "image"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "pick_up_far_from_curr_location_pop_up" "secondary_text_click"
  --   IssueReportPopUpAC act -> case act of
  --     CancelRidePopUp.Button1 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button1_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button1_action" "primary_button_no_action"
  --     CancelRidePopUp.Button2 act -> case act of
  --       PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button2_action" "primary_button_on_click"
  --       PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup_button2_action" "primary_button_no_action"
  --     CancelRidePopUp.UpdateIndex index -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "update_index"
  --     CancelRidePopUp.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "go_back"
  --     CancelRidePopUp.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "clear_options"
  --     CancelRidePopUp.TextChanged valId newVal ->  trackAppTextInput appId (getScreen HOME_SCREEN) "issue_report_popup" "text_changed"
  --     CancelRidePopUp.NoAction ->  trackAppActionClick appId (getScreen HOME_SCREEN) "issue_report_popup" "no_action"
  --   CheckFlowStatusAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "check_flow_status_action"
  --   HideLiveDashboard arg -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "hide_dash_board_action"
  --   PopUpModalAction act -> case act of 
  --     PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "button1_click"
  --     PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "button2_click"
  --     PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "no_action"
  --     PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen_pop_up" "image_click"
  --     PopUpModal.ETextController act -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_screen_pop_up" "primary_edit_text"
  --     PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "countdown_updated"
  --     PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "tip_clicked"
  --     PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "popup_dismissed"
  --     PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "options_with_html_click"
  --     PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen_pop_up" "secondary_text_click"
  --   MessageViewAnimationEnd -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "message_view_animation_end"
  --   RepeatRide arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "repeat_ride"
  --   Scroll arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_action"
  --   WhereToClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "where_to_click"
  --   ShowMoreSuggestions -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "show_more_suggestions"
  --   SuggestedDestinationClicked arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "suggested_destination_clicked"
  --   RepeatRideCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "repeat_ride_count_down"
  --   StopRepeatRideTimer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stop_repeat_ride_timer"
  --   OpenLiveDashboard -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_live_dashboard"
  --   UpdatePeekHeight -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_peek_height"
  --   ReAllocate -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "reallocate_ride"
  --   AutoScrollCountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "auto_scroll_count_down"
  --   StopAutoScrollTimer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stop_auto_scroll_timer" 
  --   UpdateRepeatTrips arg1 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_repeat_trips"
  --   RemoveShimmer -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_shimmer"
  --   ReportIssueClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "report_issue_click"
  --   ChooseSingleVehicleAction act -> case act of 
  --         ChooseVehicleController.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "no_action"
  --         ChooseVehicleController.ShowRateCard arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "ShowRateCard"
  --         ChooseVehicleController.OnImageClick -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnImageClick" 
  --         ChooseVehicleController.OnSelect arg -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "choose_your_ride_action" "OnSelect"

data ScreenOutput = LogoutUser
                  | Cancel HomeScreenState
                  | GoToHelp HomeScreenState
                  | ConfirmRide HomeScreenState
                  | GoToAbout HomeScreenState
                  | GoToNammaSafety HomeScreenState Boolean Boolean
                  | PastRides HomeScreenState
                  | GoToMyProfile HomeScreenState Boolean
                  | ChangeLanguage HomeScreenState
                  | Retry HomeScreenState
                  | GetQuotes HomeScreenState
                  | UpdatedState HomeScreenState Boolean
                  | CancelRide HomeScreenState
                  | NotificationHandler String HomeScreenState
                  | GetSelectList HomeScreenState
                  | RideConfirmed HomeScreenState
                  | SelectEstimate HomeScreenState
                  | LocationSelected LocationListItemState Boolean HomeScreenState
                  | SearchPlace String HomeScreenState
                  | UpdateLocationName HomeScreenState Number Number
                  | UpdatePickupName HomeScreenState Number Number
                  | GoToHome HomeScreenState
                  | GoToFavourites HomeScreenState
                  | SubmitRating HomeScreenState
                  | UpdatedSource HomeScreenState
                  | OpenGoogleMaps HomeScreenState
                  | InAppTrackStatus HomeScreenState
                  | UpdateSavedLocation HomeScreenState
                  | CheckLocServiceability HomeScreenState Number Number
                  | GoToInvoice HomeScreenState
                  | CheckFavDistance HomeScreenState
                  | SaveFavourite HomeScreenState
                  | GoToReferral ReferralType HomeScreenState
                  | CallDriver HomeScreenState CallType String
                  | CallContact HomeScreenState
                  | CallSupport HomeScreenState
                  | CallPolice HomeScreenState
                  | UpdateSosStatus HomeScreenState
                  | FetchContacts HomeScreenState
                  | CheckCurrentStatus
                  | CheckFlowStatus HomeScreenState
                  | ExitToPermissionFlow PermissionScreenStage
                  | RetryFindingQuotes Boolean HomeScreenState
                  | ReportIssue HomeScreenState
                  | RideDetailsScreen HomeScreenState
                  | GoToTicketBookingFlow HomeScreenState
                  | GoToMyTickets HomeScreenState
                  | RepeatTrip HomeScreenState Trip
                  | ExitToTicketing HomeScreenState
                  | GoToHelpAndSupport HomeScreenState
                  | ReAllocateRide HomeScreenState
                  | GoToRentalsFlow 
                  | GoToScheduledRides
                  | Add_Stop HomeScreenState
                  | SafetySupport HomeScreenState Boolean
                  | GoToShareRide HomeScreenState
                  | GoToNotifyRideShare HomeScreenState
                  | ExitToFollowRide HomeScreenState
                  | GoToReportSafetyIssue HomeScreenState
                  | GoToMyMetroTickets HomeScreenState
                  | GoToMetroTicketBookingFlow HomeScreenState
                  | GoToSafetyEducation HomeScreenState
                  | RepeatSearch HomeScreenState
                  | ChangeVehicleVarient HomeScreenState
                  | ExitToConfirmingLocationStage HomeScreenState
                  | UpdateReferralCode HomeScreenState String
                  | GoToSafetySettingScreen 

data Action = NoAction
            | BackPressed
            | CancelSearch
            | RecenterCurrentLocation
            | SidebarCloseAnimationCompleted
            | NotificationListener String
            | OpenSettings
            | ContinueCmd
            | OpenPricingTutorial
            | OpenSearchLocation
            | GetEstimates GetQuotesRes Int
            | GetRideConfirmation RideBookingRes
            | GetQuotesList SelectListRes
            | MAPREADY String String String
            | AfterRender
            | UpdateSource Number Number String
            | Restart ErrorResponse
            | CurrentLocation String String
            | PrimaryButtonActionController PrimaryButtonController.Action
            | SettingSideBarActionController SettingSideBarController.Action
            | PricingTutorialModelActionController PricingTutorialModelController.Action
            | SourceToDestinationActionController SourceToDestinationController.Action
            | SearchLocationModelActionController SearchLocationModelController.Action
            | QuoteListModelActionController QuoteListModelController.Action
            | DriverInfoCardActionController DriverInfoCardController.Action
            | RatingCardAC RatingCard.Action
            | UpdateLocation String String String
            | CancelRidePopUpAction CancelRidePopUp.Action
            | PopUpModalAction PopUpModal.Action
            | TrackDriver GetDriverLocationResp
            | HandleCallback
            | UpdatePickupLocation String String String
            | CloseLocationTracking
            | ShowCallDialer CallType
            | CloseShowCallDialer
            | StartLocationTracking String
            | ExitLocationSelected LocationListItemState Boolean
            | DistanceOutsideLimitsActionController PopUpModal.Action
            | ShortDistanceActionController PopUpModal.Action
            | PickUpFarFromCurrentLocAC PopUpModal.Action
            | SourceUnserviceableActionController ErrorModalController.Action
            | UpdateCurrentLocation String String
            | UpdateCurrentStage String
            | GoBackToSearchLocationModal
            | SkipButtonActionController PrimaryButtonController.Action
            | SearchExpireCountDown Int String String
            | EstimatesTryAgain GetQuotesRes Int
            | EstimateChangedPopUpController PopUpModal.Action
            | RateCardAction RateCard.Action
            | ShowRateCard
            | UpdateETA Int Int
            | WaitingTimeAction String String Int
            | DriverArrivedAction String
            | PredictionClickedAction LocationListItemController.Action
            | SavedAddressClicked LocationTagBarController.Action
            | FavouriteLocationModelAC FavouriteLocationModelController.Action
            | UpdateSourceName Number Number String
            | SaveFavouriteCardAction SaveFavouriteCardController.Action
            | TagClick CardType (Maybe LocationListItemState)
            | ContinueWithoutOffers SelectListRes
            | CheckBoxClick Boolean
            | PreferencesDropDown
            | PopUpModalShareAppAction PopUpModal.Action
            | CallSupportAction PopUpModal.Action
            | RequestInfoCardAction RequestInfoCard.Action
            | OnIconClick Boolean
            | ReferralFlowAction
            | NewUser
            | MapReadyAction
            | CheckAndAskNotificationPermission
            | TrackLiveLocationAction
            | LottieLoaderAction
            | ReferralFlowNoAction
            | UpdateSourceFromPastLocations
            | UpdateLocAndLatLong String String
            | UpdateSavedLoc (Array LocationListItemState)
            | UpdateMessages String String String String
            | InitializeChat
            | RemoveChat
            | OpenChatScreen
            | MessagingViewActionController MessagingView.Action
            | HideLiveDashboard String
            | LiveDashboardAction
            | OnResumeCallback
            | CheckFlowStatusAction
            | GoToEditProfile
            | IsMockLocation String
            | MenuButtonActionController MenuButtonController.Action
            | ChooseYourRideAction ChooseYourRideController.Action
            | SearchForSelectedLocation
            | GenderBannerModal Banner.Action
            | CancelSearchAction PopUpModal.Action
            | TriggerPermissionFlow PermissionScreenStage
            | PopUpModalCancelConfirmationAction PopUpModal.Action
            | ScrollToBottom
            | SelectButton Int
            | RateClick Int
            | Support
            | IssueReportPopUpAC CancelRidePopUp.Action
            | IssueReportIndex Int
            | RideDetails
            | TerminateApp
            | DirectSearch
            | ZoneTimerExpired PopUpModal.Action
            | DisabilityBannerAC Banner.Action
            | DisabilityPopUpAC PopUpModal.Action
            | RideCompletedAC RideCompletedCard.Action
            | LoadMessages
            | KeyboardCallback String
            | NotifyDriverStatusCountDown Int String String
            | UpdateProfileButtonAC PrimaryButtonController.Action 
            | SkipAccessibilityUpdateAC PrimaryButtonController.Action
            | SpecialZoneOTPExpiryAction Int String String
            | TicketBookingFlowBannerAC Banner.Action
            | MetroTicketBookingBannerAC Banner.Action
            | ScrollStateChanged String
            | RemoveNotification
            | MessageDriver
            | SendQuickMessage String
            | MessageExpiryTimer Int String String
            | NotificationAnimationEnd
            | ShareRide
            | OpenEmergencyHelp
            | OpenOffUsSOS
            | MessageViewAnimationEnd
            | RepeatRide Int Trip
            | Scroll Number
            | WhereToClick 
            | ShowMoreSuggestions 
            | SuggestedDestinationClicked LocationListItemState
            | RepeatRideCountDown Int String String
            | StopRepeatRideTimer 
            | OpenLiveDashboard
            | UpdatePeekHeight 
            | ReAllocate
            | AutoScrollCountDown Int String String 
            | StopAutoScrollTimer 
            | UpdateRepeatTrips RideBookingListRes 
            | RemoveShimmer 
            | ReportIssueClick
            | DateTimePickerAction String Int Int Int String Int Int
            | ChooseSingleVehicleAction ChooseVehicleController.Action
            | UpdateSheetState BottomSheetState
            | LocationTagBarAC LocationTagBarV2Controller.Action
            | RentalBannerAction Banner.Action
            | BottomNavBarAction BottomNavBarIcon
            | BannerCarousel BannerCarousel.Action
            | SetBannerItem ListItem
            | UpdateBanner
            | BannerChanged String
            | BannerStateChanged String
            | MetroTicketBannerClickAC Banner.Action
            | SafetyBannerAction Banner.Action
            | SafetyAlertAction PopUpModal.Action
            | ContactAction ContactCircle.Action
            | NotifyRideShare PrimaryButtonController.Action
            | ToggleShare Int
            | UpdateFollowers FollowRideRes
            | GoToFollowRide
            | ShowBookingPreference
            | UpdateBookingDetails RideBookingRes
            | UpdateContacts (Array NewContacts)
            | UpdateChatWithEM Boolean
            | ShareRideAction PopupWithCheckboxController.Action
            | AllChatsLoaded
            | GoToSafetyEducationScreen
            | SpecialZoneInfoTag
            | GoToConfirmingLocationStage
            | ReferralComponentAction ReferralComponent.Action
            | GoToHomeScreen
            | ShowMultipleProvider Boolean
            | ShowPref
            | ProviderAutoSelected Int String String
            | ShowProviderInfo Boolean
            | AcWorkingPopupAction PopUpModal.Action
            | NoRender

eval :: Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState

eval GoToConfirmingLocationStage state = 
  exit $ ExitToConfirmingLocationStage state

eval (ChooseSingleVehicleAction (ChooseVehicleController.ShowRateCard config)) state = do
  continue state 
    { props 
      { showRateCard = true }
    , data 
      { rateCard 
        { onFirstPage = false
        , vehicleVariant = config.vehicleVariant
        , currentRateCardType = DefaultRateCard
        , pickUpCharges = config.pickUpCharges 
        , tollCharge = config.tollCharge
        , extraFare = config.extraFare
        , driverAdditions = config.driverAdditions
        , fareInfoDescription = config.fareInfoDescription
        , isNightShift = config.isNightShift
        , nightChargeTill = config.nightChargeTill
        , nightChargeFrom = config.nightChargeFrom
        }
      }
    }

eval (ChooseSingleVehicleAction (ChooseVehicleController.OnEditClick)) state =  do
  let 
    topProvider = filter (\element -> element.providerType == CTP.ONUS) state.data.specialZoneQuoteList
    firstTopProvider = fromMaybe ChooseVehicleController.config $ head topProvider 
    firstAllProvider = fromMaybe ChooseVehicleController.config $ head state.data.specialZoneQuoteList
    showMultiProvider' = if state.data.currentCityConfig.iopConfig.enable then null topProvider else false -- if there is no top provider then show all providers
    selectedEstimate' = if showMultiProvider' then firstAllProvider else firstTopProvider
    specialZoneQuoteList' = mapWithIndex (\index element -> element{activeIndex = selectedEstimate'.index}) state.data.specialZoneQuoteList
    (Tuple estimateId otherSelectedEstimates) = getEstimateId specialZoneQuoteList' selectedEstimate'

  exit $ ChangeVehicleVarient state{
    data{
      specialZoneQuoteList = specialZoneQuoteList',
      iopState { 
        showMultiProvider = showMultiProvider'
      , showPrefButton = state.data.currentCityConfig.iopConfig.enable && (not (null topProvider))
      } 
      , selectedEstimatesObject = selectedEstimate' {activeIndex = selectedEstimate'.index}
      , otherSelectedEstimates = otherSelectedEstimates
    }
  , props{
      isRepeatRide = false 
    , estimateId = estimateId
    }
  }

eval ShowMoreSuggestions state = do
  void $ pure $ map (\item -> startLottieProcess lottieAnimationConfig{ rawJson =  (getAssetsBaseUrl FunctionCall) <> "lottie/right_arrow.json" , speed = 1.0,lottieId = (getNewIDWithTag $ "movingArrowView" <> show item), minProgress = 0.0 }) [0,1]
  continueWithCmd state { props {suggestionsListExpanded = not state.props.suggestionsListExpanded} } [pure NoAction]

eval RemoveShimmer state = continue state{props{showShimmer = false}}

eval ShowPref state = continue state { data{ iopState { providerPrefInfo = false, providerPrefVisible = not state.data.iopState.providerPrefVisible}}}

-- Provider Switch Action in Estimates Screen
eval (ShowMultipleProvider showMultiProvider) state = do
  let 
    customerTip = if showMultiProvider then HomeScreenData.initData.props.customerTip else state.props.customerTip
    topProvider = filter (\element -> element.providerType == CTP.ONUS) state.data.specialZoneQuoteList
    firstTopProvider = fromMaybe ChooseVehicleController.config $ head topProvider 
    firstAllProvider = fromMaybe ChooseVehicleController.config $ head state.data.specialZoneQuoteList
    selectedEstimate' = if showMultiProvider then firstAllProvider else firstTopProvider
    specialZoneQuoteList' = mapWithIndex (\index element -> element{activeIndex = selectedEstimate'.index}) state.data.specialZoneQuoteList
    (Tuple estimateId otherSelectedEstimates) = getEstimateId specialZoneQuoteList' selectedEstimate'

  continueWithCmd state {
    data { 
      specialZoneQuoteList = specialZoneQuoteList', 
      triggerPatchCounter = state.data.triggerPatchCounter + 1, 
      otherSelectedEstimates = otherSelectedEstimates,
      iopState { 
        showMultiProvider = showMultiProvider, 
        providerPrefVisible = false
      }, 
      selectedEstimatesObject = selectedEstimate'{
        activeIndex = selectedEstimate'.index
      }
    }, 
    props { 
      customerTip = customerTip, 
      estimateId = estimateId
    }
  } [pure NoAction]

eval (ShowProviderInfo showProviderInfo) state = continue state { 
  data { 
    iopState { 
      providerPrefInfo = showProviderInfo
    , providerPrefVisible = false
    }
  }
}

eval (UpdateFollowers (FollowRideRes resp)) state = do
  let followers = map (\(Followers follower) -> follower) resp
  continue state{
    data{
      followers = Just followers
    }
  }

eval GoToFollowRide state = exit $ ExitToFollowRide state

eval (UpdateRepeatTrips rideList) state = do
  void $ pure $ setValueToLocalStore UPDATE_REPEAT_TRIPS "false"
  let shimmerState = state{props{showShimmer = false}}
      list = rideListToTripsTransformer (rideList ^._list)
  if not (null list) then do
    let updatedMap = updateMapWithPastTrips list shimmerState
    void $ pure $ setSuggestionsMap updatedMap
    if state.props.sourceLat /= 0.0 && state.props.sourceLong /= 0.0 then
      updateCurrentLocation shimmerState (show state.props.sourceLat) (show state.props.sourceLong)
    else 
      continue shimmerState
  else do
    continue shimmerState

        
eval UpdatePeekHeight state = continue state{data{peekHeight = getPeekHeight state}}

eval (Scroll item) state = do
  let sheetState = if item == state.props.currSlideIndex then state.props.isHomescreenExpanded
                   else item > state.props.currSlideIndex
      updatedState = state { props { isHomescreenExpanded = sheetState, currSlideIndex = item } }
  continue updatedState

eval ReAllocate state =
  if isLocalStageOn ReAllocated then do
    let updatedState = state{ props{ currentStage = FindingQuotes } }
    void $ pure $ setValueToLocalStore LOCAL_STAGE ( show FindingQuotes)
    updateAndExit updatedState $ ReAllocateRide updatedState
  else continue state
  
eval (SetBannerItem bannerItem) state = continue state{data{bannerData{bannerItem = Just bannerItem}}, props{isBannerDataComputed = true}}

eval UpdateBanner state = do
  if state.data.bannerData.bannerScrollState == "1" then continue state
  else do
    let nextBanner = state.data.bannerData.currentBanner + 1
        bannerArray = if state.props.currentStage == HomeScreen then getBannerConfigs state BannerCarousel else getDriverInfoCardBanners state BannerCarousel
        updatedIdx = if nextBanner >= (length bannerArray) then 0 else nextBanner
        newState = state{data {bannerData{currentBanner = updatedIdx, currentPage = updatedIdx}}}
    continue newState

eval (BannerChanged item) state = do
  let currentBanner = fromString item
  case currentBanner of
    Just idx -> do 
        let newState = state{data {bannerData{currentBanner = idx}}}
        if state.data.bannerData.currentPage /= idx then void $ pure $ unsafePerformEffect $ processEvent "RestartAutoScroll" unit -- To stop and stop the new autosroll
          else pure unit
        continue newState
    Nothing  -> continue state

eval (BannerStateChanged item) state = do
  let newState = state{data {bannerData{bannerScrollState = item}}}
  continue newState

eval (BannerCarousel (BannerCarousel.OnClick idx)) state = 
  continueWithCmd state [do
    let banners = if state.props.currentStage == HomeScreen then getBannerConfigs state BannerCarousel else getDriverInfoCardBanners state BannerCarousel
    case index banners idx of
      Just config -> do
        let _ = runFn2 updatePushInIdMap "bannerCarousel" false
        case config.type of
          BannerCarousel.Gender -> pure $ GenderBannerModal $ Banner.OnClick
          BannerCarousel.Disability -> pure $ DisabilityBannerAC $ Banner.OnClick
          BannerCarousel.ZooTicket -> pure $ TicketBookingFlowBannerAC $ Banner.OnClick
          BannerCarousel.MetroTicket -> pure $ MetroTicketBannerClickAC $ Banner.OnClick
          BannerCarousel.Safety -> pure $ SafetyBannerAction $ Banner.OnClick
          BannerCarousel.CabLaunch -> pure $ WhereToClick
          BannerCarousel.Remote link -> do
            if os == "IOS" && STR.contains (STR.Pattern "vp=sedu&option=video") link  -- To be removed after deep links are added in iOS
              then pure GoToSafetyEducationScreen
            else do
              void $ openUrlInApp link
              pure NoAction
          _ -> pure NoAction
      Nothing -> pure NoAction
  ] 

eval (MetroTicketBannerClickAC Banner.OnClick) state =  exit $ GoToMetroTicketBookingFlow state

eval GoToSafetyEducationScreen state = exit $ GoToSafetyEducation state

eval SearchForSelectedLocation state = do
  let currentStage = if state.props.searchAfterEstimate then TryAgain else FindingEstimate
  updateAndExit state{props{isPopUp = NoPopUp}} $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = currentStage, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }, isPopUp = NoPopUp}}

eval CheckFlowStatusAction state = exit $ CheckFlowStatus state

eval TerminateApp state = do
  pure $ terminateApp state.props.currentStage true
  continue state

eval (KeyboardCallback keyBoardState) state = do 
  let isOpen = case keyBoardState of
                    "onKeyboardOpen" -> true
                    "onKeyboardClose" -> false
                    _ -> false 
  if isLocalStageOn ChatWithDriver && isOpen then
    void $ pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true 
  else pure unit
  continue state

eval (NotifyDriverStatusCountDown seconds status timerID) state = do 
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId timerID
    _ <- pure $ setValueToLocalStore NOTIFIED_CUSTOMER "true"
    let eta = fromMaybe 0 state.data.driverInfoCardState.eta
    if isLocalStageOn RideAccepted && isJust state.data.driverInfoCardState.eta && (secondsToHms eta) /= "--" then 
      continue state{data{lastMessage = state.data.lastMessage{message = state.data.config.notifyRideConfirmationConfig.autoGeneratedText <> (secondsToHms eta), sentBy = "Driver"}},props{unReadMessages = true, showChatNotification = true}}
    else continue state
  else continue state

eval (RepeatRideCountDown seconds status timerID) state = do
  if state.props.currentStage == FindingQuotes then do
    void $ pure $ clearTimerWithId timerID
    continue state{props{repeatRideTimer = "", repeatRideTimerId = "", repeateRideTimerStoped = true}}
  else if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    void $ pure $ performHapticFeedback unit
    void $ pure $ updateLocalStage FindingQuotes
    void $ pure $ setValueToLocalStore SELECTED_VARIANT state.data.selectedEstimatesObject.vehicleVariant
    let updatedState = state{data{rideHistoryTrip = Nothing}, props{repeatRideTimerId = "", currentStage = FindingQuotes,repeateRideTimerStoped = true, searchExpire = (getSearchExpiryTime "LazyCheck")}}
    updateAndExit (updatedState) (GetQuotes updatedState)
  else continue state{props{repeatRideTimer = (show seconds), repeatRideTimerId = timerID, repeateRideTimerStoped = false}}

eval StopRepeatRideTimer state =  do
  void $ pure $ clearTimerWithId state.props.repeatRideTimerId
  continue state{props{repeatRideTimer = "", repeatRideTimerId = "", repeateRideTimerStoped = true}}

eval (AutoScrollCountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    void $ pure $ performHapticFeedback unit
    let updatedState = state{props{autoScroll = false, autoScrollTimerId = "", homeScreenSheetState = EXPANDED, isHomescreenExpanded=true, currSlideIndex=1.0, autoScrollTimer = ""}}
    continue updatedState
  else continue state{props{autoScrollTimer = (show seconds), autoScrollTimerId = timerID}}

eval StopAutoScrollTimer state =  do
  void $ pure $ clearTimerWithId state.props.autoScrollTimerId
  continue state{props{autoScrollTimer = "", autoScrollTimerId = ""}}

eval (IsMockLocation isMock) state = do
  let val = isMock == "true"
      _ = unsafePerformEffect $ if val then  logEvent (state.data.logField) "ny_fakeGPS_enabled" else pure unit -- we are using unsafePerformEffect becasue without it we are not getting logs in firebase, since we are passing a parameter from state i.e. logField then the output will be inline and it will not be able to precompute so it's safe to use it here.
  continue state{props{isMockLocation = false}}

eval (UpdateCurrentStage stage) state = do
  _ <- pure $ spy "updateCurrentStage" stage
  if stage == "REALLOCATED" then
    exit $ NotificationHandler "REALLOCATE_PRODUCT" state
  else if (stage == "INPROGRESS") && (not $ state.props.isChatWithEMEnabled || isLocalStageOn RideStarted) then
    exit $ NotificationHandler "TRIP_STARTED" state
  else if (stage == "COMPLETED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "TRIP_FINISHED" state
  else if (stage == "CANCELLED") && (not $ isLocalStageOn HomeScreen) then
    exit $ NotificationHandler "CANCELLED_PRODUCT" state
  else
    continue state

eval OnResumeCallback state =
  case getValueToLocalNativeStore LOCAL_STAGE of
    "FindingQuotes" -> do
      let secondsLeft = findingQuotesSearchExpired false
      case (methodArgumentCount "startLottieProcess") == 1 of
        true  -> do
          let findingQuotesProgress = 1.0 - (toNumber secondsLeft)/(toNumber (getSearchExpiryTime "LazyCheck"))
          if secondsLeft > 0 then
            void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
          else pure unit
        false -> pure unit
      case flowWithoutOffers WithoutOffers of
        true | secondsLeft <= 0 -> do
            _ <- pure $ updateLocalStage QuoteList
            continueWithCmd state [do
              let response = SelectListRes { selectedQuotes: Nothing, bookingId : Nothing }
              pure $ GetQuotesList response
            ]
        _ -> continue state
    "QuoteList" -> do
      let findingQuotesProgress = 1.0 - 30.0/(toNumber (getSearchExpiryTime "LazyCheck"))
      void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), minProgress = findingQuotesProgress, scaleType="CENTER_CROP"}
      continue state
    "RideAccepted" | state.data.currentSearchResultType == QUOTES -> exit $ Retry state
    _ -> continue state

eval (UpdateSavedLoc savedLoc) state = continue state{data{savedLocations = savedLoc}}

eval ( RideCompletedAC (RideCompletedCard.SelectButton index)) state = 
  case state.data.ratingViewState.issueFacedView of
    true -> continue state { data { ratingViewState { selectedYesNoButton = index, doneButtonVisibility = true}}}
    false -> continue state {data { ratingViewState{selectedYesNoButton = index, doneButtonVisibility = true, wasOfferedAssistance = Just (index==0)}}} 

eval ( RideCompletedAC (RideCompletedCard.RateClick index)) state = do
  void $ pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
  continue
    state
      { props { currentStage = if state.data.driverInfoCardState.providerType == CTP.ONUS then RideRating else state.props.currentStage }
      , data
        { rideRatingState 
            { rating = index
            , feedbackList = state.data.rideRatingState.feedbackList
            }
          , ratingViewState { selectedRating = index }
        }
      }

eval ( RideCompletedAC (RideCompletedCard.IssueReportIndex index)) state =
  case index of
    0 -> continue state { data { ratingViewState { openReportIssue = true }}}
    1 -> exit $ ReportIssue state { data {  ratingViewState { issueReason = Nothing }}}
    _ -> continue state

eval (RideCompletedAC (RideCompletedCard.Support)) state = continue state {props {callSupportPopUp = true}}

eval (RideCompletedAC (RideCompletedCard.RideDetails)) state = exit $ RideDetailsScreen state -- TODO needs to fill the data

eval (RideCompletedAC (RideCompletedCard.HelpAndSupportAC)) state = exit $ GoToHelpAndSupport state

------------------------------- ChatService - Start --------------------------

eval (UpdateMessages message sender timeStamp size) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{data{messagesSize = size}} [do
      pure $ LoadMessages
    ]

eval LoadMessages state = do
  let allMessages = getChatMessages FunctionCall
  case last allMessages of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true, isChatNotificationDismissed = false}} 
                      else do
                        let currentUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) else "Customer" 
                        if value.sentBy == currentUser then updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = if state.props.isChatWithEMEnabled then getSuggestionsfromKey emChatSuggestion emergencyContactInitialChatSuggestionId else [], lastMessage = value, lastSentMessage = value}, props {canSendSuggestion = true,  isChatNotificationDismissed = false}}
                        else do
                          let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                              unReadMessages = if readMessages == 0 && state.props.currentStage /= ChatWithDriver then true else (readMessages < (length allMessages) && state.props.currentStage /= ChatWithDriver)
                              suggestionKey = if state.props.isChatWithEMEnabled then emChatSuggestion else chatSuggestion
                              suggestions = getSuggestionsfromKey suggestionKey value.message
                              isChatNotificationDismissed = not state.props.isChatNotificationDismissed || state.data.lastMessage.message /= value.message
                              showNotification = isChatNotificationDismissed && unReadMessages
                          updateMessagesWithCmd state {data {messages = allMessages, chatSuggestionsList = suggestions, lastMessage = value, lastSentMessage = MessagingView.dummyChatComponent, lastReceivedMessage = value}, props {unReadMessages = unReadMessages, showChatNotification = showNotification, canSendSuggestion = true, isChatNotificationDismissed = false, removeNotification = not showNotification, enableChatWidget = showNotification}}
      Nothing -> 
        if state.props.isChatWithEMEnabled then
            continueWithCmd state [do 
            pure $ SendQuickMessage "c013253fcbe2fdc50b1c261501de9045"
            ] 
          else continue state {props {canSendSuggestion = true}}

eval (OpenChatScreen) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.MessageDriver))
    ]

eval MessageDriver state = do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (DriverInfoCardActionController (DriverInfoCardController.MessageDriver))
    ]

eval (MessagingViewActionController (MessagingView.TextChanged value)) state = continue state{data{messageToBeSent = (STR.trim value)},props{sendMessageActive = (STR.length (STR.trim value)) >= 1}}

eval (DriverInfoCardActionController (DriverInfoCardController.BannerCarousel act)) state = 
  continueWithCmd state [do
      pure $ BannerCarousel act
    ]
    
eval (DriverInfoCardActionController (DriverInfoCardController.UpdateBanner)) state = 
  continueWithCmd state [do
      pure $ UpdateBanner
    ]

eval (DriverInfoCardActionController (DriverInfoCardController.BannerChanged value)) state = 
  continueWithCmd state [do
      pure $ BannerChanged value
    ]

eval (DriverInfoCardActionController (DriverInfoCardController.BannerStateChanged value)) state = 
  continueWithCmd state [do
      pure $ BannerStateChanged value
    ]

eval(MessagingViewActionController (MessagingView.Call)) state = do
  void $ pure $ performHapticFeedback unit
  void $ pure $ hideKeyboardOnNavigation true
  if state.props.isChatWithEMEnabled 
    then do
      let filterContacts = filter (\item -> item.priority == 0) $ fromMaybe [] state.data.contactList
      case head filterContacts of
        Nothing -> continue state
        Just contact -> do 
          void $ pure $ showDialer contact.number true
          continue state
    else if length state.data.config.callOptions > 1 then
      continue state { props { showCallPopUp = true } }
    else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (MessagingViewActionController (MessagingView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then do
    pure $ sendMessage state.data.messageToBeSent
    pure $ setText (getNewIDWithTag "ChatInputEditText") ""
    continue state{data{messageToBeSent = ""},props {sendMessageActive = false}}
  else
    continue state

eval (MessagingViewActionController (MessagingView.BackPressed)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
      pure $ BackPressed
    ]

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval InitializeChat state = do
  continue state {props { chatcallbackInitiated = true } }


eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
    pure $ NoAction
  ]

eval (DriverInfoCardActionController (DriverInfoCardController.WaitingInfo)) state =
  if state.props.currentStage == RideAccepted then
    continue state { data { waitTimeInfo = true } }
  else
    continue state

eval (SendQuickMessage chatSuggestion) state = do
  if state.props.canSendSuggestion then do
    _ <- pure $ sendMessage chatSuggestion
    continue state {props {unReadMessages = false}}
  else continue state

eval (DriverInfoCardActionController (DriverInfoCardController.MessageDriver)) state = do
  if state.data.config.feature.enableChat && state.data.driverInfoCardState.providerType == CTP.ONUS then do
    if not state.props.chatcallbackInitiated || state.data.waitTimeInfo then continue state else do
      void $ pure $ performHapticFeedback unit
      _ <- pure $ updateLocalStage ChatWithDriver
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      let allMessages = getChatMessages FunctionCall
      continueWithCmd state {data{messages = allMessages}, props {currentStage = ChatWithDriver, sendMessageActive = false, unReadMessages = false, showChatNotification = false, isChatNotificationDismissed = false,sheetState = Just COLLAPSED}}  [ do pure $ UpdateSheetState COLLAPSED]
  else continueWithCmd state[ do
        pure $ DriverInfoCardActionController (DriverInfoCardController.CallDriver) 
      ]

eval (UpdateSheetState sheetState) state = continue state {props {sheetState = Nothing, currentSheetState = sheetState}}

eval (DriverInfoCardActionController (DriverInfoCardController.CollapseBottomSheet)) state = continue state {props {sheetState = Just COLLAPSED, currentSheetState = COLLAPSED}}

eval RemoveNotification state = do
  continue state {props { showChatNotification = false, isChatNotificationDismissed = true}}

eval NotificationAnimationEnd state = do
  let isExpanded = state.props.showChatNotification && state.props.chatcallbackInitiated
      areMessagesEmpty =  (length $ getChatMessages FunctionCall) == 0 && (not state.props.isChatWithEMEnabled)
      showNotification = (areMessagesEmpty || state.props.showChatNotification) && ((state.props.currentStage == RideAccepted) || state.props.isChatWithEMEnabled) && not state.props.isChatNotificationDismissed
  continue state {props { isNotificationExpanded = isExpanded, showChatNotification = showNotification, removeNotification = not showNotification, enableChatWidget = (isExpanded || areMessagesEmpty) && not state.props.isChatNotificationDismissed}}

eval MessageViewAnimationEnd state = do
  continue state {props { removeNotification = not state.props.showChatNotification}}

eval (MessagingViewActionController (MessagingView.SendSuggestion chatSuggestion)) state = do
  if state.props.canSendSuggestion then do
    _ <- pure $ sendMessage chatSuggestion
    continue state {data {chatSuggestionsList = []}, props {canSendSuggestion = false}}
  else continue state

eval AllChatsLoaded state = do
  if state.props.isChatWithEMEnabled then do
    void $ pure $ sendMessage "c013253fcbe2fdc50b1c261501de9045"
    continue state
  else
    continue state

------------------------------- ChatService - End --------------------------

eval (MessageExpiryTimer seconds status timerID) state = do
  let newState = state{data{triggerPatchCounter = state.data.triggerPatchCounter + 1}}
  if status == "EXPIRED"
    then do
      _ <- pure $ clearTimerWithId timerID
      let currentUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) getKeyInSharedPrefKeys) else "Customer" 
      if state.data.lastMessage.sentBy == currentUser then
      continueWithCmd newState [ do
        pure $ RemoveNotification
      ]
      else continue newState
  else
      continue newState

eval (DriverInfoCardActionController (DriverInfoCardController.NoAction)) state = continue state {data{infoCardPeekHeight = getInfoCardPeekHeight state}}

eval (ScrollStateChanged scrollState) state = do
  let sheetState = case scrollState of 
              "1" -> STATE_DRAGGING
              "2" -> STATE_SETTLING
              "3" -> STATE_EXPANDED
              "4" -> STATE_COLLAPSED
              "5" -> STATE_HIDDEN
              "6" -> STATE_HALF_EXPANDED
              _ -> STATE_HIDDEN
  continue state {props {bottomSheetState = sheetState, currentSheetState = if sheetState == STATE_EXPANDED then EXPANDED else state.props.currentSheetState, sheetState = Nothing}}

eval (DriverInfoCardActionController (DriverInfoCardController.CallDriver)) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (DriverInfoCardActionController (DriverInfoCardController.SpecialZoneInfoTag)) state = continue state{ props{ showSpecialZoneInfoPopup = true } }

eval DirectSearch state =continue state{props{currentStage = SearchLocationModel}}

eval BackPressed state = do
  void $ pure $ toggleBtnLoader "" false
  let _ = runFn2 updatePushInIdMap "EstimatePolling" true
  case state.props.currentStage of
    SearchLocationModel -> do
      if state.props.isSaveFavourite then 
        continueWithCmd state{props{isSearchCancelled = false}} [pure $ (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose))]
      else do
        if state.props.isSearchLocation == LocateOnMap then do
          void $ pure $ exitLocateOnMap ""
          void $ pure $ hideKeyboardOnNavigation true
          continue state{data{nearByPickUpPoints = []},props{defaultPickUpPoint = "" , isSearchLocation = SearchLocation, locateOnMap = false, isSearchCancelled = false}}
        else do
          if (getSearchType unit) == "direct_search" then
            pure $ terminateApp state.props.currentStage false
          else 
            pure unit
          void $ pure $ removeAllPolylines ""
          void $ pure $ updateLocalStage HomeScreen
          void $ pure $ setValueToLocalStore SESSION_ID (generateSessionId unit)
          void $ pure $ enableMyLocation true
          void $ pure $ setValueToLocalStore NOTIFIED_CUSTOMER "false"
          let 
            { savedLocationsWithOtherTag
            , recentlySearchedLocations
            , suggestionsMap
            , trips
            , suggestedDestinations
            } = getHelperLists state.data.savedLocations state.data.recentSearchs state state.props.currentLocation.lat state.props.currentLocation.lng
          recenterCurrentLocation $ 
            HomeScreenData.initData
              { data
                { disability = state.data.disability
                , bannerData = state.data.bannerData
                , tripSuggestions = trips
                , recentSearchs {predictionArray = recentlySearchedLocations}
                , destinationSuggestions = suggestedDestinations
                , settingSideBar
                  { gender = state.data.settingSideBar.gender
                  , email = state.data.settingSideBar.email
                  , hasCompletedSafetySetup = state.data.settingSideBar.hasCompletedSafetySetup
                  }
                  , followers = state.data.followers
                }
              , props { 
                  isBanner = state.props.isBanner
                , sourceLat = state.props.sourceLat
                , showShimmer = false
                , city = state.props.city
                , sourceLong = state.props.sourceLong
                , currentLocation = state.props.currentLocation
                , sosBannerType = state.props.sosBannerType 
                , followsRide = state.props.followsRide
                , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled
                , rideSearchProps { 
                    cachedPredictions = state.props.rideSearchProps.cachedPredictions
                  }
                }
              }
    SettingPrice -> do
      void $ pure $ performHapticFeedback unit
      void $ pure $ clearTimerWithId state.props.repeatRideTimerId
      let updatedState = state{props{repeatRideTimer = "", repeatRideTimerId = "", isSearchCancelled = false}}
      if updatedState.props.showRateCard then 
        if updatedState.data.rateCard.currentRateCardType /= DefaultRateCard then
          continue updatedState{data{rateCard {currentRateCardType = DefaultRateCard}}}
        else 
          continue updatedState{props{showRateCard = false}}
      else if updatedState.props.showMultipleRideInfo then 
        continue updatedState{props{showMultipleRideInfo=false}}
      else if state.props.showBookingPreference then 
        continue state {props {showBookingPreference = false}}
      else if state.data.iopState.providerPrefVisible || state.data.iopState.providerPrefInfo then 
        continue state { data { iopState { providerPrefInfo = false ,providerPrefVisible = false}}}
      else if state.data.iopState.providerSelectionStage then do
        void $ pure $ clearTimerWithId state.data.iopState.timerId
        continue state { props{isPopUp = ConfirmBack}, data { iopState { timerVal = "0"}}}
      else do
        void $ pure $ updateLocalStage SearchLocationModel
        continue state{
          data{
            rideHistoryTrip = Nothing
          , specialZoneQuoteList = []
          }
        , props{
            rideRequestFlow = false
          , currentStage = SearchLocationModel
          , searchId = ""
          , isSource = Just false
          , isSearchLocation = SearchLocation
          , isRepeatRide = false
          , customerTip = HomeScreenData.initData.props.customerTip
          , tipViewProps = HomeScreenData.initData.props.tipViewProps
          }
        }
    ConfirmingLocation -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ exitLocateOnMap ""
                      _ <- pure $ removeAllPolylines ""
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{defaultPickUpPoint = "", rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation},data{polygonCoordinates = "", nearByPickUpPoints = []}}
    FindingEstimate -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      let newState = state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
                      updateAndExit newState $ GoToHome newState
                      -- continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation}}
    QuoteList       -> do
                      void $ pure $ performHapticFeedback unit
                      if state.props.isPopUp == NoPopUp then continue $ state { props{isPopUp = ConfirmBack}} else continue state
    PricingTutorial -> do
                      void $ pure $ performHapticFeedback unit
                      continue state { props { currentStage = SettingPrice}}
    DistanceOutsideLimits -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation }}
    ShortDistance -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage SearchLocationModel
                      continue state{props{isSource = Just false,isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}
    FindingQuotes ->  do
                      void $ pure $ performHapticFeedback unit
                      continue $ state { props{isPopUp = ConfirmBack}}
    FavouriteLocationModel -> do
                      void $ pure $ performHapticFeedback unit
                      _ <- pure $ updateLocalStage (if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel)
                      continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel}}
    ChatWithDriver -> do
                        if state.props.showCallPopUp then continue state {props{showCallPopUp = false}}
                         else do
                            let lastStage = if state.props.isChatWithEMEnabled then RideStarted else RideAccepted
                            _ <- pure $ updateLocalStage lastStage
                            continue state {props {currentStage = lastStage}}
    RideRating ->     do
                      _ <- pure $ updateLocalStage RideCompleted
                      continue state {props {currentStage = RideCompleted}}
    ReAllocated ->    continue state
    _               -> do
                        if state.props.isLocationTracking then continue state{props{isLocationTracking = false}}
                          else if state.props.cancelSearchCallDriver then continue state{props{cancelSearchCallDriver = false}}
                          else if state.props.showCallPopUp then continue state{props{showCallPopUp = false}}
                          else if state.props.isCancelRide then continue state{props{isCancelRide = false}}
                          else if state.props.isSaveFavourite then continueWithCmd state [pure $ SaveFavouriteCardAction SaveFavouriteCardController.OnClose]
                          else if state.props.showShareAppPopUp then continue state{props{showShareAppPopUp=false}}
                          else if state.props.showMultipleRideInfo then continue state{props{showMultipleRideInfo=false}}
                          else if state.props.showLiveDashboard then do
                            continueWithCmd state [do
                              _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                              pure NoAction
                            ]
                          else if state.props.callSupportPopUp then continue state {props {callSupportPopUp = false}}
                          else if state.data.ratingViewState.openReportIssue then continue state {data {ratingViewState {openReportIssue = false}}}
                          else if state.props.showEducationalCarousel then do 
                            _ <- pure $ pauseYoutubeVideo unit
                            continue state{props{showEducationalCarousel = false}}
                          else if state.data.waitTimeInfo then continue state { data {waitTimeInfo =false} }
                          else if state.props.showSpecialZoneInfoPopup then continue state { props{ showSpecialZoneInfoPopup = false } }
                          else do
                              pure $ terminateApp state.props.currentStage true
                              continue state

eval GoBackToSearchLocationModal state = do
  void $ pure $ updateLocalStage SearchLocationModel
  void $ pure $ exitLocateOnMap ""
  continue state { props { rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just true, isSrcServiceable = true, isRideServiceable = true } }

eval HandleCallback state = do
  continue state { props { callbackInitiated = true } }

eval (UpdateSource lat lng name) state = do
  _ <- pure $ printLog "Name::" name
  exit $ UpdatedState state { data { source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lng}, props { sourceLat = lat, sourceLong = lng, searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} } true

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard =false, showShimmer = false}}

eval LiveDashboardAction state = do
  _ <- pure $ firebaseLogEvent "ny_user_on_ride_live_stats"
  if os == "IOS" then do
      continueWithCmd state [do
        _ <- openUrlInApp "https://nammayatri.in/open?source=in-app"
        pure NoAction
      ]
  else continue state {props {showLiveDashboard = true}}


eval (UpdateSourceName lat lon name) state = continue state {data{source = name, sourceAddress = encodeAddress name [] state.props.sourcePlaceId lat lon}, props{searchLocationModelProps{crossBtnSrcVisibility = (STR.length name) > 2}} }

eval (MAPREADY key latitude longitude) state =
  case key of
    _ -> continueWithCmd state [ do
      _ <- checkPermissionAndUpdatePersonMarker state
      pure AfterRender
    ]

eval ShowBookingPreference state = continue state {props {showBookingPreference = not state.props.showBookingPreference, showMultipleRideInfo = false}}

eval OpenSearchLocation state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ firebaseLogEvent "ny_user_hs_pickup_click"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.PickupSearch" "true"
  let srcValue = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source
  exit $ UpdateSavedLocation state { props { isSource = Just true, currentStage = SearchLocationModel, isSearchLocation = SearchLocation, searchLocationModelProps{crossBtnSrcVisibility = (STR.length srcValue) > 2}, rideSearchProps{ sessionId = generateSessionId unit } }, data {source=srcValue, locationList = state.data.recentSearchs.predictionArray} }

eval (SourceUnserviceableActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = continueWithCmd state [ do pure $ OpenSearchLocation ]

eval (UpdateLocation key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  if os == "IOS" && not state.props.locateOnMapProps.cameraAnimatedToSource && (getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLong) > 5.0 then do
    continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
      void $ animateCamera state.props.sourceLat state.props.sourceLong 25.0 "NO_ZOOM"
      pure NoAction
    ]
  else do
    let updatedState = state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } }
        sourceManuallyMoved = if updatedState.props.isSource == Just true then true else updatedState.props.rideSearchProps.sourceManuallyMoved
        destManuallyMoved = if updatedState.props.isSource == Just false then true else updatedState.props.rideSearchProps.destManuallyMoved
    case key of
      "LatLon" -> do
        let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates latitude longitude spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot)  ) updatedState.data.nearByPickUpPoints)
        exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = "", rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved }, hotSpot{ selectedSpot = selectedSpot }, locateOnMapProps{ isSpecialPickUpGate = false }}} latitude longitude
      _ ->  case (filter(\item -> item.place == key) updatedState.data.nearByPickUpPoints) !! 0 of
              Just spot -> exit $ UpdateLocationName updatedState{props{defaultPickUpPoint = key, rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved}, locateOnMapProps{ isSpecialPickUpGate = fromMaybe false spot.isSpecialPickUp }, hotSpot{ centroidPoint = Nothing }}} spot.lat spot.lng
              Nothing -> continue updatedState
    

eval (UpdatePickupLocation key lat lon) state = do
  let latitude = fromMaybe 0.0 (NUM.fromString lat)
      longitude = fromMaybe 0.0 (NUM.fromString lon)
  if os == "IOS" && not state.props.locateOnMapProps.cameraAnimatedToSource && (getDistanceBwCordinates latitude longitude state.props.sourceLat state.props.sourceLong) > 5.0 then do
    continueWithCmd state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } } [do
      void $ animateCamera state.props.sourceLat state.props.sourceLong 25.0 "NO_ZOOM"
      pure NoAction
    ]
  else do
    let updatedState = state{ props{ locateOnMapProps{ cameraAnimatedToSource = true } } }
        sourceManuallyMoved = true
    case key of
      "LatLon" -> do
        let selectedSpot = head (filter (\spots -> (getDistanceBwCordinates (fromMaybe 0.0 (NUM.fromString lat)) (fromMaybe 0.0 (NUM.fromString lon)) spots.lat spots.lng) * 1000.0 < (toNumber JB.locateOnMapConfig.thresholdDistToSpot) ) updatedState.data.nearByPickUpPoints)
        exit $ UpdatePickupName updatedState{props{defaultPickUpPoint = "", rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved}, hotSpot{ selectedSpot = selectedSpot }, locateOnMapProps{ isSpecialPickUpGate = false }}} latitude longitude
      _ -> do
        let focusedIndex = findIndex (\item -> item.place == key) updatedState.data.nearByPickUpPoints
            spot = (filter(\item -> item.place == key) updatedState.data.nearByPickUpPoints) !! 0
        case focusedIndex, spot of
          Just index, Just spot' -> do
            _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
            exit $ UpdatePickupName updatedState{props{defaultPickUpPoint = key, rideSearchProps{ sourceManuallyMoved = sourceManuallyMoved}, locateOnMapProps{ isSpecialPickUpGate = fromMaybe false spot'.isSpecialPickUp }, hotSpot{ centroidPoint = Nothing }}} spot'.lat spot'.lng
          _, _ -> continue updatedState

eval (CheckBoxClick autoAssign) state = do
  void $ pure $ performHapticFeedback unit
  let event = if autoAssign then "ny_user_pref_autoassigned" else "ny_user_pref_driveroffers"
  let _ = unsafePerformEffect $ logEvent state.data.logField event
  _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show autoAssign)
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ if autoAssign then "4" else "17"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ if autoAssign then "8000.0" else "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT $ if autoAssign then "22" else "117"
  continue state{props{flowWithoutOffers = autoAssign, showBookingPreference = false}, data { iopState { providerPrefVisible = false}}}

eval (OnIconClick autoAssign) state = do
  continue state { props {showMultipleRideInfo = true}}

eval PreferencesDropDown state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (RatingCardAC (RatingCard.Rating index)) state = do
  let feedbackListArr = if index == state.data.rideRatingState.rating then state.data.rideRatingState.feedbackList else []
  continue state { data { rideRatingState { rating = index , feedbackList = feedbackListArr}, ratingViewState { selectedRating = index} } }

eval (RatingCardAC (RatingCard.SelectPill feedbackItem id)) state = do
  let newFeedbackList = updateFeedback id feedbackItem state.data.rideRatingState.feedbackList
      filterFeedbackList = filter (\item -> length item.answer > 0) newFeedbackList
  continue state { data { rideRatingState {  feedbackList = filterFeedbackList} } }

eval (RatingCardAC (RatingCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = updateAndExit state $ SubmitRating state

eval (RatingCardAC (RatingCard.FeedbackChanged value)) state = continue state { data { rideRatingState { feedback = value } } }

eval (RatingCardAC (RatingCard.BackPressed)) state = do
  _ <- pure $ updateLocalStage RideCompleted
  continue state {props {currentStage = RideCompleted}}

eval (SettingSideBarActionController (SettingSideBarController.PastRides)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_myrides_click"
  exit $ PastRides state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.OnHelp)) state = exit $ GoToHelp state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.ChangeLanguage)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_language"
  exit $ ChangeLanguage state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToAbout)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_about"
  exit $ GoToAbout state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SettingSideBarActionController (SettingSideBarController.GoToNammaSafety)) state = do
  exit $ GoToNammaSafety state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false false

eval (SettingSideBarActionController (SettingSideBarController.GoToMyTickets)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_zoo_tickets"
  exit $ GoToMyTickets state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyMetroTickets)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_metro_tickets"
  exit $ GoToMyMetroTickets state { data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.ShareAppLink)) state =
  exit $ GoToReferral GIVE_REFERRAL state

eval (SettingSideBarActionController (SettingSideBarController.EditProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false

eval (SettingSideBarActionController (SettingSideBarController.OnClosed)) state = continue state{ data{settingSideBar {opened = SettingSideBarController.CLOSED}}}

eval (SettingSideBarActionController (SettingSideBarController.OnClose)) state =
  if state.props.showLiveDashboard then do
    continueWithCmd state [do
      _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
      pure NoAction
    ]
    else if state.props.isPopUp == Logout then
      continue state {props{isPopUp = NoPopUp}}
      else case state.data.settingSideBar.opened of
                SettingSideBarController.CLOSED -> do
                                                    if state.props.currentStage == HomeScreen then do
                                                      pure $ terminateApp state.props.currentStage true
                                                      continue state
                                                      else continueWithCmd state [pure $ BackPressed]
                _                               -> continue state {data{settingSideBar{opened = SettingSideBarController.CLOSING}}}

eval (SettingSideBarActionController (SettingSideBarController.OnLogout)) state = continue state { props { isPopUp = Logout } }

eval (SettingSideBarActionController (SettingSideBarController.GoToFavourites)) state = exit $ GoToFavourites state {data{settingSideBar{opened = SettingSideBarController.OPEN}}}

eval (SettingSideBarActionController (SettingSideBarController.GoToMyProfile)) state = exit $ GoToMyProfile state { data { settingSideBar { opened = SettingSideBarController.OPEN } } } false


eval (SettingSideBarActionController (SettingSideBarController.LiveStatsDashboard)) state = openLiveDashboard state

eval OpenLiveDashboard state = openLiveDashboard state{props{showShimmer = false}}

eval (SearchLocationModelActionController (SearchLocationModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  let newState = state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, locateOnMap = false, defaultPickUpPoint = ""}}
  updateAndExit newState $ LocationSelected (fromMaybe dummyListItem (if state.props.isSource == Just false then state.data.selectedLocationListItem else Nothing)) (state.props.isSource == Just false) newState

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = do
    case state.props.currentStage of
      HomeScreen   -> do
        void $ pure $ performHapticFeedback unit
        let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.DestinationSearch" "true"
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
        exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }}, data{source= state.data.source}}
      ConfirmingLocation -> do
        void $ pure $ performHapticFeedback unit
        _ <- pure $ exitLocateOnMap ""
        _ <- pure $ updateLocalStage FindingEstimate
        let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.ConfirmLocation" "true"
        let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_confirm_pickup"
        let updatedState = state{props{currentStage = FindingEstimate, locateOnMap = false}, data { iopState { showMultiProvider = false}}}
        updateAndExit updatedState $  (UpdatedSource updatedState)
      SettingPrice -> do
                        void $ pure $ performHapticFeedback unit
                        _ <- pure $ updateLocalStage FindingQuotes
                        void $ pure $ setValueToLocalStore SELECTED_VARIANT state.data.selectedEstimatesObject.vehicleVariant
                        let updatedState = state{data{rideHistoryTrip = Nothing}, props{currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck")}}
                        updateAndExit (updatedState) (GetQuotes updatedState)
      _            -> continue state

eval WhereToClick state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.DestinationSearch" "true"
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_where_to_btn"
  exit $ UpdateSavedLocation state{props{isSource = Just false, isSearchLocation = SearchLocation, currentStage = SearchLocationModel, searchLocationModelProps{crossBtnSrcVisibility = false }, rideSearchProps{sessionId = generateSessionId unit}}, data{source= if state.data.source == "" then getString CURRENT_LOCATION else state.data.source}}
  
eval (RideCompletedAC RideCompletedCard.GoToSOS) state = exit $ GoToNammaSafety state true false 

eval (RideCompletedAC (RideCompletedCard.SkipButtonActionController (PrimaryButtonController.OnClick))) state = 
  case state.data.ratingViewState.issueFacedView of
    true -> do
            void $ pure $ toggleBtnLoader "SkipButton" false
            _ <- pure $ setValueToLocalStore REFERRAL_STATUS "HAS_TAKEN_RIDE"
            let newState = state
                           { props { nightSafetyFlow = false }
                           , data
                             { rideRatingState =
                               dummyRideRatingState
                                 { driverName = state.data.driverInfoCardState.driverName
                                 , rideId = state.data.driverInfoCardState.rideId
                                 },
                               ratingViewState {issueFacedView = false, openReportIssue = false, selectedYesNoButton = -1, doneButtonVisibility = false}
                             }
                           }
            if state.props.nightSafetyFlow && state.data.ratingViewState.selectedYesNoButton == boolToInt state.props.nightSafetyFlow  then 
              exit $ GoToReportSafetyIssue newState
            else continue newState
              
    false ->  
      if state.props.showOfferedAssistancePopUp then do
        void $ pure $ toggleBtnLoader "SkipButton" false
        continue state {data {ratingViewState {selectedYesNoButton = -1, doneButtonVisibility = false}} , props{showOfferedAssistancePopUp = false}} 
      else 
        if state.data.ratingViewState.selectedRating > 0 then updateAndExit state $ SubmitRating state{ data {rideRatingState {rating = state.data.ratingViewState.selectedRating }}}
            else do
              _ <- pure $ firebaseLogEvent "ny_user_ride_skip_feedback"
              _ <- pure $ setValueToLocalStore RATING_SKIPPED "true"
              _ <- pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
                                          event : "process_result"
                                        , payload : Just {
                                          action : "feedback_skipped"
                                        , trip_amount : Just state.data.finalAmount
                                        , trip_id : Just state.props.bookingId
                                        , ride_status : Nothing
                                        , screen : Just $ getScreenFromStage state.props.currentStage
                                        , exit_app : false
                                        }
                                        }
              updateAndExit state $ GoToHome state

eval OpenSettings state = do
  _ <- pure $ hideKeyboardOnNavigation true
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_burger_menu"
  continue state { data { settingSideBar { opened = SettingSideBarController.OPEN } } }

eval (SearchExpireCountDown seconds status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId timerID
    let tipViewData = HomeScreenData.initData.props.tipViewProps
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    continue state { props { searchExpire = seconds } }
  else do
    let enableTips = isTipEnabled state
    if any ( _ == state.props.currentStage) [FindingQuotes , QuoteList] then continue state { props { searchExpire = seconds ,timerId = timerID , tipViewProps {isVisible = enableTips && (seconds <= (getSearchExpiryTime "LazyCheck")-state.data.config.tipDisplayDuration || state.props.tipViewProps.isVisible || state.props.tipViewProps.activeIndex >= 0)}, customerTip{enableTips = enableTips}} }
      else do
        _ <- pure $ clearTimerWithId timerID
        continue state { props { searchExpire = (getSearchExpiryTime "LazyCheck") ,timerId = timerID , tipViewProps {isVisible = false}} }

eval CancelSearch state = case state.props.currentStage of
  FindingEstimate -> do
    void $ pure $ performHapticFeedback unit
    _ <- pure $ updateLocalStage SearchLocationModel
    let _ = unsafePerformEffect $ logEvent state.data.logField  "ny_user_estimate_cancel_search"
    continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation } }
  ConfirmingRide -> do
    void $ pure $ performHapticFeedback unit
    continue state { props { currentStage = SettingPrice, isSearchLocation = NoView } }
  _ -> continue state

eval SidebarCloseAnimationCompleted state = continue state --{props{sideBarStatus = SettingSideBarController.CLOSED}}

eval OpenPricingTutorial state = continue state { props { currentStage = PricingTutorial } }

eval (PricingTutorialModelActionController (PricingTutorialModelController.Close)) state = continue state { props { currentStage = SettingPrice } }

eval (DriverInfoCardActionController (DriverInfoCardController.PrimaryButtonAC PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state
    [ do
        _ <- pure $ showDialer (getDriverNumber "") false -- TODO: FIX_DIALER
        _ <- (logEventWithTwoParams state.data.logField "ny_user_call_click" "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID))
        pure NoAction
    ]
eval (DriverArrivedAction driverArrivalTime) state =
  if any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver] then do
      _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_WAITING_ACTION"
      exit $ Cancel state { data { driverInfoCardState { driverArrived = true, driverArrivalTime = getExpiryTime driverArrivalTime true } } }
    else continue state

eval (WaitingTimeAction timerID timeInMinutes seconds) state = do
  _ <- pure $ if getValueToLocalStore DRIVER_ARRIVAL_ACTION == "TRIGGER_WAITING_ACTION"
                then setValueToLocalStore DRIVER_ARRIVAL_ACTION "WAITING_ACTION_TRIGGERED"
                else pure unit
  continue state { data { driverInfoCardState { waitingTime = timeInMinutes} }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }

eval (SpecialZoneOTPExpiryAction seconds status timerID) state = do
  if status == "EXPIRED" then do
    _ <- pure $ toast $ getString $ OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN"
    _ <- pure $ clearTimerWithId timerID
    exit $ NotificationHandler "CANCELLED_PRODUCT" state
  else do
    let timeInMinutes = formatDigits $ seconds/60
        timeInSeconds = formatDigits $ seconds - (seconds/60) * 60
    continue state { data { driverInfoCardState { waitingTime = timeInMinutes <> " : " <> timeInSeconds } }, props { waitingTimeTimerIds = union state.props.waitingTimeTimerIds [timerID] } }
  where
    formatDigits :: Int -> String
    formatDigits time = (if time >= 10 then "" else "0") <> show time

eval (DriverInfoCardActionController (DriverInfoCardController.OnNavigate mode lat lon)) state = do
  void $ pure $ openNavigation lat lon (show mode)
  continue state

eval (DriverInfoCardActionController (DriverInfoCardController.RideSupport)) state = do
  void $ pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp = true}}

eval (CancelSearchAction PopUpModal.DismissPopup) state = do continue state {props { cancelSearchCallDriver = false }}

eval (CancelSearchAction PopUpModal.OnButton1Click) state = do
  if length state.data.config.callOptions > 1 then
    continue state { props { showCallPopUp = true, cancelSearchCallDriver = false } }
  else callDriver state $ fromMaybe "ANONYMOUS" $ state.data.config.callOptions !! 0

eval (CancelSearchAction PopUpModal.OnButton2Click) state = do
  let isAcCab = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
  continue state { props { isCancelRide = true, cancellationReasons = cancelReasons isAcCab, cancelRideActiveIndex = Nothing, cancelReasonCode = "", cancelDescription = "", cancelSearchCallDriver = false } }

eval (DriverInfoCardActionController (DriverInfoCardController.CancelRide infoCard)) state =
  if (state.data.config.driverInfoConfig.showCancelPrevention && not state.props.isSpecialZone) || state.props.zoneType.sourceTag == METRO then
    continue state { props { cancelSearchCallDriver = true } }
      else continueWithCmd state [ pure $ CancelSearchAction PopUpModal.OnButton2Click]

eval (DriverInfoCardActionController (DriverInfoCardController.LocationTracking)) state = do
  void $ pure $ performHapticFeedback unit
  continue state { props { isLocationTracking = true } }

eval OpenEmergencyHelp state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_ic_safety_center_clicked"
  exit $ GoToNammaSafety state true false

eval OpenOffUsSOS state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_ic_safety_center_clicked"
  exit $ GoToSafetySettingScreen

eval (DriverInfoCardActionController (DriverInfoCardController.ToggleBottomSheet)) state = continue state{props{currentSheetState = if state.props.currentSheetState == EXPANDED then COLLAPSED else EXPANDED}}

eval (DriverInfoCardActionController (DriverInfoCardController.ShareRide)) state = 
  if state.data.config.feature.shareWithEmergencyContacts 
    then exit $ GoToShareRide state
    else continueWithCmd state [pure ShareRide]

eval ShareRide state = do
  continueWithCmd state
        [ do
            let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
            _ <- pure $ shareTextMessage "" $ getString $ TRACK_RIDE_STRING appName state.data.driverInfoCardState.driverName (state.data.config.appData.website <> "t?i="<>state.data.driverInfoCardState.rideId) state.data.driverInfoCardState.registrationNumber
            void $ pure $ cleverTapCustomEvent "ny_user_share_ride_via_link"
            pure NoAction
         ]

eval (CancelRidePopUpAction (CancelRidePopUp.Button1 PrimaryButtonController.OnClick)) state = do
      void $ pure $ performHapticFeedback unit
      continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.OnGoBack)) state = continue state { props { isCancelRide = false } }

eval (CancelRidePopUpAction (CancelRidePopUp.UpdateIndex index)) state = continue state { props { cancelRideActiveIndex = Just index, cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode } }

eval (CancelRidePopUpAction (CancelRidePopUp.TextChanged valId newVal)) state = continue state { props { cancelDescription = newVal } }

eval (CancelRidePopUpAction (CancelRidePopUp.ClearOptions)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state { props { cancelDescription = "", cancelReasonCode = "", cancelRideActiveIndex = Nothing } }

eval (CancelRidePopUpAction (CancelRidePopUp.Button2 PrimaryButtonController.OnClick)) state = do
    let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".CancelRide") "true"
    void $ pure $ performHapticFeedback unit
    case state.props.cancelRideActiveIndex of
      Just index -> if ( (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "OTHER" || (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode == "TECHNICAL_GLITCH" ) then exit $ CancelRide state{props{cancelDescription = if (state.props.cancelDescription == "") then (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description else state.props.cancelDescription }}
                      else exit $ CancelRide state{props{cancelDescription = (fromMaybe dummyCancelReason (state.props.cancellationReasons !!index)).description , cancelReasonCode = (fromMaybe dummyCancelReason (state.props.cancellationReasons !! index)).reasonCode }}
      Nothing    -> continue state

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.Button1 PrimaryButtonController.OnClick))) state = continue state { data { ratingViewState { openReportIssue = false } } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.OnGoBack))) state = continue state { data { ratingViewState { openReportIssue = false } } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.UpdateIndex index))) state = continue state { data { ratingViewState { issueReportActiveIndex = Just index} } }

eval ( RideCompletedAC (RideCompletedCard.IssueReportPopUpAC (CancelRidePopUp.Button2 PrimaryButtonController.OnClick))) state = do
  let issue = (if state.props.nightSafetyFlow then safetyIssueOptions true else reportIssueOptions state)!!(fromMaybe 1 state.data.ratingViewState.issueReportActiveIndex)
      reason = (fromMaybe dummyCancelReason issue)
  exit $ ReportIssue state { data {
    ratingViewState { issueReason = Just reason.reasonCode, issueDescription = reason.description},
    rideRatingState {rideId = state.data.driverInfoCardState.rideId, feedback = ""}
    }}

eval (PredictionClickedAction (LocationListItemController.OnClick item)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_prediction_list_item"
  locationSelected item false state{data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source}, props{isSource = Just false}}

eval (SuggestedDestinationClicked item) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_sd_list_item"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.SuggestedDestination" "true"
  locationSelected item true state{props{isSource = Just false, rideSearchProps{sessionId = generateSessionId unit}, suggestedRideFlow = true}, data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source, nearByPickUpPoints = [], polygonCoordinates = ""}}

eval (PredictionClickedAction (LocationListItemController.FavClick item)) state = do
  if (length state.data.savedLocations >= 20) then do
    void $ pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
    continue state
    else exit $ CheckFavDistance state{data{saveFavouriteCard{ address = item.description, selectedItem = item, tag = "", tagExists = false, isBtnActive = false }, selectedLocationListItem = Just item}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.OnClose)) state = continue state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing, saveFavouriteCard {address = "" , tag = "", isBtnActive = false}}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.SaveFavourite)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ SaveFavourite state{props{isSaveFavourite = false},data{selectedLocationListItem = Nothing}}

eval (SaveFavouriteCardAction (SaveFavouriteCardController.PrimayEditTA (PrimaryEditTextController.TextChanged id val))) state = do
  let input = STR.trim val
  let updatedState = state{data{saveFavouriteCard{isBtnActive = ((STR.length input) >=3),tagExists = not (validTag (getSavedTagsFromHome state.data.savedLocations) input ""),tag = input}}}
  continue updatedState

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.FavClick item))) state = continueWithCmd state [pure $ (PredictionClickedAction (LocationListItemController.FavClick item))]

eval (FavouriteLocationModelAC (FavouriteLocationModelController.GenericHeaderAC (GenericHeaderController.PrefixImgOnClick))) state = continue state { props { currentStage = if state.props.isSearchLocation == NoView then HomeScreen else SearchLocationModel} }

eval (FavouriteLocationModelAC (FavouriteLocationModelController.FavouriteLocationAC (SavedLocationCardController.CardClicked item))) state = do
  if state.props.isSource == Just true then do
    let newState = state {data{ source = item.savedLocation, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = ST.FAVOURITE } }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.savedLocation
    exit $ LocationSelected item  false newState
    else do
      let newState = state {data{ destination = item.savedLocation,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.savedLocation
      exit $ LocationSelected item  false newState

eval (SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem)) state = if not state.props.isSrcServiceable then continue state else do 
  _ <- pure $ firebaseLogEvent ("ny_user_savedLoc_" <> show savedAddressType)
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.SavedLocation." <> show savedAddressType) "true"
  tagClickEvent savedAddressType arrItem state{data{source = if state.data.source == "" then (getString CURRENT_LOCATION) else state.data.source},props{isSource = Just false}}

eval (SearchLocationModelActionController (SearchLocationModelController.SavedAddressClicked (LocationTagBarController.TagClick savedAddressType arrItem))) state = tagClickEvent savedAddressType arrItem state

eval (TagClick savedAddressType arrItem) state = tagClickEvent savedAddressType arrItem state

eval (SearchLocationModelActionController (SearchLocationModelController.LocationListItemActionController (LocationListItemController.OnClick item))) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_location_list_item"
  let condition = state.props.isSource == Just true && any (_ == item.locationItemType) [Just RECENTS, Just SUGGESTED_DESTINATIONS] 
  locationSelected item {tag = if condition then "" else item.tag, showDistance = Just false} true state{ props { rideSearchProps{ sourceSelectType = if condition then ST.SUGGESTION else state.props.rideSearchProps.sourceSelectType } }, data { nearByDrivers = Nothing } }

eval (ExitLocationSelected item addToRecents)state = exit $ LocationSelected item  addToRecents state

eval (SearchLocationModelActionController (SearchLocationModelController.DebounceCallBack searchString isSource)) state = do
  if (STR.length searchString > 2) && (isSource == fromMaybe true state.props.isSource) then 
    validateSearchInput state searchString
  else continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceChanged input)) state = do
  let srcValue = if (state.data.source == "" || state.data.source == "Current Location") then true else false
  let sourceSelectType = if state.props.locateOnMap then ST.MAP else state.props.rideSearchProps.sourceSelectType
      newState = state {props{ rideSearchProps{ sourceSelectType = sourceSelectType } }}
  if (input /= state.data.source) then do 
    continueWithCmd newState { props { isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input) > 2 then state.props.searchLocationModelProps.isAutoComplete else false}}} 
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd newState{props {searchLocationModelProps{crossBtnSrcVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationChanged input)) state = do
  if (input /= state.data.destination) then do
    continueWithCmd state { props { isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = if (STR.length input)>2 then state.props.searchLocationModelProps.isAutoComplete else false}} }
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]
  else
    continueWithCmd state{props {searchLocationModelProps{crossBtnDestVisibility = (STR.length input) > 2, isAutoComplete = false}}}
      [ do
          _ <- pure $ updateInputString input
          pure NoAction
      ]

eval (SearchLocationModelActionController (SearchLocationModelController.EditTextFocusChanged textType)) state = do
  _ <- pure $ spy "searchLocationModal" textType
  if textType == "D" then
    continue state { props { isSource = Just false, searchLocationModelProps{crossBtnDestVisibility = (STR.length state.data.destination) > 2}}, data {source = if state.data.source == "" then state.data.searchLocationModelData.prevLocation else state.data.source, locationList = if state.props.isSource == Just false then state.data.locationList else state.data.destinationSuggestions } }
  else
    continue state { props { isSource = Just true, searchLocationModelProps{crossBtnSrcVisibility = (STR.length state.data.source) > 2}} , data{ locationList = if state.props.isSource == Just true then state.data.locationList else state.data.recentSearchs.predictionArray } }

eval (SearchLocationModelActionController (SearchLocationModelController.NoAction)) state = continue state

eval (SearchLocationModelActionController (SearchLocationModelController.SourceClear)) state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "SourceEditText")
    pure unit
  else
    pure unit
  let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { source = "", recentSearchs {predictionArray = predicArray}, locationList = predicArray, searchLocationModelData{prevLocation = state.data.source}}, props { isSource = Just true, isSrcServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnSrcVisibility = false} } }

eval (SearchLocationModelActionController (SearchLocationModelController.DestinationClear)) state = do
  void $ pure $ performHapticFeedback unit
  if (state.props.isSearchLocation /= LocateOnMap) then do
    _ <- pure $ requestKeyboardShow (getNewIDWithTag "DestinationEditText")
    pure unit
  else
    pure unit
  let predicArray = (updateLocListWithDistance state.data.recentSearchs.predictionArray state.props.sourceLat state.props.sourceLong true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist)
  continue state { data { destination = "", locationList = predicArray }, props {isSource = Just false, isDestServiceable = true, isRideServiceable = true, searchLocationModelProps{crossBtnDestVisibility = false}} }

eval (SearchLocationModelActionController (SearchLocationModelController.GoBack)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state{props{showShimmer = true}}
    [ do
        _ <- pure $ hideKeyboardOnNavigation true
        pure $ BackPressed
    ]

eval (SearchLocationModelActionController (SearchLocationModelController.SetCurrentLocation)) state = do
  _ <- pure $ currentPosition ""
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_currentlocation_click"
  pure $ setText (getNewIDWithTag "SourceEditText") (if (state.data.source == "") then (getString CURRENT_LOCATION) else state.data.source)
  continue state{ props{ rideSearchProps{ sourceSelectType = if state.props.isSource == Just true then ST.SEARCH else state.props.rideSearchProps.sourceSelectType }, searchLocationModelProps{isAutoComplete = false}}, data{source = if state.props.currentLocation.place /= "" then state.props.currentLocation.place else (getString CURRENT_LOCATION)}}

eval (SearchLocationModelActionController (SearchLocationModelController.SetLocationOnMap)) state = do
  void $ pure $ performHapticFeedback unit
  let isSource = case state.props.isSource of
                    Just true -> true
                    _         -> false
      isDestinationNotEmpty = (not isSource && state.props.destinationLat /= 0.0 && state.props.destinationLong /= 0.0)
      lat = if isDestinationNotEmpty then state.props.destinationLat else state.props.sourceLat
      lon = if isDestinationNotEmpty then state.props.destinationLong else state.props.sourceLong
  _ <- pure $ hideKeyboardOnNavigation true
  _ <- pure $ removeAllPolylines ""
  _ <- pure $ unsafePerformEffect $ runEffectFn1 locateOnMap locateOnMapConfig { lat = lat, lon = lon, geoJson = state.data.polygonCoordinates, points = state.data.nearByPickUpPoints, zoomLevel = pickupZoomLevel, labelId = getNewIDWithTag "LocateOnMapPin", locationName = fromMaybe "" state.props.locateOnMapProps.sourceLocationName, specialZoneMarkerConfig{ labelImage = zoneLabelIcon state.props.confirmLocationCategory }}
  pure $ unsafePerformEffect $ logEvent state.data.logField if state.props.isSource == Just true  then "ny_user_src_set_location_on_map" else "ny_user_dest_set_location_on_map"
  let srcValue = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source
  when (state.data.destination == "") $ do
    pure $ setText (getNewIDWithTag "DestinationEditText") ""
  let newState = state
                  { data {source = srcValue}
                  , props { isSearchLocation = LocateOnMap
                          , currentStage = SearchLocationModel
                          , locateOnMap = true,
                           isRideServiceable = true
                           , showlocUnserviceablePopUp = false
                           , searchLocationModelProps{isAutoComplete = false}
                           , locateOnMapLocation
                              { sourceLat = state.props.sourceLat
                              , sourceLng = state.props.sourceLong
                              , source = state.data.source
                              , sourceAddress = state.data.sourceAddress
                              , destinationLat = if state.props.destinationLat /= 0.0 then state.props.destinationLat else state.props.sourceLat
                              , destinationLng = if state.props.destinationLong /= 0.0 then state.props.destinationLong else state.props.sourceLong
                              , destination = state.data.destination
                              , destinationAddress = state.data.destinationAddress 
                              }
                           }
                    }
  (updateAndExit newState) $ UpdatedState newState false

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateSource lat lng name)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.props.isSource == Just true then do
    let newState = state{data{source = if state.data.source == "" then getString CURRENT_LOCATION else state.data.source, sourceAddress = encodeAddress name [] Nothing lat lng},props{ sourceLat= lat,  sourceLong = lng, sourcePlaceId = Nothing, searchLocationModelProps{isAutoComplete = false}}}
    updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState
    else do
      let newState = state{data{destination = name,destinationAddress = encodeAddress name [] Nothing lat lng},props{ destinationLat = lat,  destinationLong = lng, destinationPlaceId = Nothing}}
      updateAndExit newState $ LocationSelected (fromMaybe dummyListItem newState.data.selectedLocationListItem) false newState

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.Click quote))) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd (state { data { quoteListModelState = map (\x -> x { selectedQuote = (Just quote.id) }) state.data.quoteListModelState }, props { selectedQuote = Just quote.id } })
    [ do
        if (getValueToLocalStore AUTO_SELECTING) == "CANCELLED_AUTO_ASSIGN" then
          pure NoAction
        else do
          void $ pure $ setValueToLocalStore AUTO_SELECTING quote.id
          pure NoAction
    ]


eval (QuoteListModelActionController (QuoteListModelController.CancelAutoAssigning)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ setValueToLocalStore AUTO_SELECTING "CANCELLED_AUTO_ASSIGN"
  continue state


eval (QuoteListModelActionController (QuoteListModelController.TipViewPrimaryButtonClick PrimaryButtonController.OnClick)) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".Tip") "true"
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
  _ <- pure $ clearTimerWithId state.props.timerId
  void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "progress_loader_line", lottieId = (getNewIDWithTag "lottieLoaderAnimProgress"), scaleType="CENTER_CROP"}
  let tipViewData = state.props.tipViewProps{stage = TIP_AMOUNT_SELECTED, onlyPrimaryText = true}
  let newState = state{ props{rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }, findingRidesAgain = true ,searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = TryAgain, isPopUp = NoPopUp ,tipViewProps = tipViewData ,customerTip {tipForDriver = (fromMaybe 0 (customerTipArrayWithValues !! state.props.tipViewProps.activeIndex)) , tipActiveIndex = state.props.tipViewProps.activeIndex, isTipSelected = true } }, data{nearByDrivers = Nothing}}
  updateAndExit newState $ RetryFindingQuotes false newState

eval (QuoteListModelActionController (QuoteListModelController.TipsViewActionController (TipsView.TipBtnClick index value))) state = do
  let check = index == state.props.tipViewProps.activeIndex
  continue state { props {tipViewProps { stage = (if check then DEFAULT else TIP_AMOUNT_SELECTED) , isprimaryButtonVisible = not check , activeIndex = (if check then -1 else index)}}}

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController QuoteListItemController.ConfirmRide)) state = do
  void $ pure $ performHapticFeedback unit
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote_confirm"
  exit $ ConfirmRide state

eval (QuoteListModelActionController (QuoteListModelController.QuoteListItemActionController (QuoteListItemController.CountDown seconds status id))) state = do
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId id
    let
      autoSelecting = (getValueToLocalStore AUTO_SELECTING) == id
    if (id == fromMaybe "" state.props.selectedQuote && autoSelecting && state.props.currentStage == QuoteList) then do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_auto_assign"
      continueWithCmd state [ pure $ (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) ]
    else do
      let
        newState = state { data { quoteListModelState = filter (\x -> x.id /= id) state.data.quoteListModelState } }
      continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote, expiredQuotes = (snoc state.props.expiredQuotes id) } }
  else do
    let
      newState = state { data = state.data { quoteListModelState = map (\x -> if x.id == id then x { timer = (show seconds) } else x) state.data.quoteListModelState } }
    continue newState { props { selectedQuote = if newState.data.quoteListModelState == [] then Nothing else newState.props.selectedQuote } }

eval (QuoteListModelActionController (QuoteListModelController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  case state.props.selectedQuote, (null state.data.quoteListModelState) of
    Just _, false -> do
      _ <- pure $ updateLocalStage ConfirmingRide
      let
        newState = state { props { currentStage = ConfirmingRide } }
      updateAndExit newState $ ConfirmRide newState
    _, _ -> continue state

eval (QuoteListModelActionController (QuoteListModelController.GoBack)) state = do
  void $ pure $ performHapticFeedback unit
  continueWithCmd state [ do pure $ BackPressed ]

eval (QuoteListModelActionController (QuoteListModelController.TryAgainButtonActionController  PrimaryButtonController.OnClick)) state = updateAndExit state $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state{props{currentStage = TryAgain, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }}}

eval (QuoteListModelActionController (QuoteListModelController.HomeButtonActionController PrimaryButtonController.OnClick)) state = do
  void $ pure $ performHapticFeedback unit
  updateAndExit state CheckCurrentStatus

eval (QuoteListModelActionController (QuoteListModelController.ChangeTip)) state = do
  continue state {props { tipViewProps {stage = DEFAULT}}}

eval (Restart err) state = exit $ LocationSelected (fromMaybe dummyListItem state.data.selectedLocationListItem) false state

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state =   case state.props.isPopUp of
  TipsPopUp -> do
    void $ pure $ performHapticFeedback unit
    let _ = unsafePerformEffect $ logEvent state.data.logField if state.props.customerTip.isTipSelected then ("ny_added_tip_for_" <> (show state.props.currentStage)) else "ny_no_tip_added"
    _ <- pure $ clearTimerWithId state.props.timerId
    let tipViewData = state.props.tipViewProps{stage = RETRY_SEARCH_WITH_TIP , isVisible = not (state.props.customerTip.tipActiveIndex == 0) , activeIndex = state.props.customerTip.tipActiveIndex, onlyPrimaryText = true}
    let newState = state{ props{findingRidesAgain = true ,searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = RetryFindingQuote, isPopUp = NoPopUp ,tipViewProps = tipViewData, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH } }}
    _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
    logInfo "retry_finding_quotes" ( "TipConfirmed : Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
    exit $ RetryFindingQuotes true newState
  Logout -> continue state{props{isPopUp = NoPopUp}}
  _ -> do
    void $ pure $ performHapticFeedback unit
    _ <- pure $ firebaseLogEvent "ny_tip_not_applicable" 
    if (isLocalStageOn FindingQuotes ) then do
        _ <- pure $ clearTimerWithId state.props.timerId
        let tipViewData = HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        exit $ RepeatSearch state{props{customerTip = HomeScreenData.initData.props.customerTip, tipViewProps = HomeScreenData.initData.props.tipViewProps, isPopUp = NoPopUp, selectedQuote = Nothing, isRepeatRide = false}, data{quoteListModelState = []}}
      else if state.data.iopState.providerSelectionStage then do
      _ <- pure $ updateLocalStage SearchLocationModel
      void $ pure $ clearTimerWithId state.data.iopState.timerId
      continue state{data{rideHistoryTrip = Nothing, iopState{ providerSelectionStage = false}},props{ isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSource = Just false,isSearchLocation = SearchLocation, isRepeatRide = false}}
      else do
      _ <- pure $ clearTimerWithId state.props.timerId
      let newState = state{props{findingRidesAgain = true , searchExpire = (getSearchExpiryTime "LazyCheck"), currentStage = RetryFindingQuote, isPopUp = NoPopUp, rideSearchProps{ sourceSelectType = ST.RETRY_SEARCH }}}
      updateAndExit newState $ RetryFindingQuotes true newState

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = case state.props.isPopUp of
    TipsPopUp -> case state.props.currentStage of
      QuoteList -> do
        void $ pure $ performHapticFeedback unit
        updateAndExit state CheckCurrentStatus
      FindingQuotes -> do
        void $ pure $ performHapticFeedback unit
        exit $ CheckCurrentStatus
      _ -> continue state
    Logout -> exit LogoutUser
    ConfirmBack -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_no_retry"
      case (getValueToLocalStore LOCAL_STAGE) of
        "QuoteList" -> do
          void $ pure $ performHapticFeedback unit
          exit $ CheckCurrentStatus
        "SettingPrice" -> do
          void $ pure $ performHapticFeedback unit
          continue state{props{isPopUp = NoPopUp}}
        "FindingQuotes" -> do
          void $ pure $ performHapticFeedback unit
          continue state{props{isPopUp = NoPopUp}}
        _ -> continue state
    NoPopUp -> continue state
    ActiveQuotePopUp -> do
      void $ pure $ performHapticFeedback unit
      exit $ CheckCurrentStatus

eval (PopUpModalAction (PopUpModal.TipsViewActionController (TipsView.TipBtnClick index value))) state = do
  void $ pure $ performHapticFeedback unit
  case state.props.isPopUp of
    TipsPopUp -> continue state{props{customerTip{tipActiveIndex = index, tipForDriver= value, isTipSelected = not (index == 0)}}}
    _ -> continue state

eval (PopUpModalAction (PopUpModal.DismissPopup)) state = do
  let newState = if (isLocalStageOn QuoteList) then state else state{props{isPopUp = NoPopUp, customerTip{tipActiveIndex = 1,tipForDriver = 10, isTipSelected = false} }}
  continue newState

eval (DistanceOutsideLimitsActionController (PopUpModal.OnButton2Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state { props { isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation, isSource = Just false, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }

eval (ShortDistanceActionController (PopUpModal.OnButton2Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ exitLocateOnMap ""
  exit $ UpdatedSource state

eval (ShortDistanceActionController (PopUpModal.OnButton1Click)) state = do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ updateLocalStage SearchLocationModel
  continue state{props{isSource = Just false, isPopUp = NoPopUp, rideRequestFlow = false, currentStage = SearchLocationModel, searchId = "", isSearchLocation = SearchLocation}}

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton2Click)) state = do 
  if (state.props.isShorterTrip)  then do 
    void $ pure $ updateLocalStage ShortDistance
    continue state {props{currentStage = ShortDistance}}
    else continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton2Click) ]

eval (PickUpFarFromCurrentLocAC (PopUpModal.OnButton1Click)) state = do 
  continueWithCmd state [ do pure $ ShortDistanceActionController (PopUpModal.OnButton1Click) ]

eval (EstimateChangedPopUpController (PopUpModal.OnButton1Click)) state = exit $ GoToHome state

eval (EstimateChangedPopUpController (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ updateLocalStage FindingQuotes
  let
    updatedState = state { props { currentStage = FindingQuotes, isEstimateChanged = false, searchExpire = (getSearchExpiryTime "LazyCheck") } }
  updateAndExit updatedState $ GetQuotes updatedState

eval CloseLocationTracking state = continue state { props { isLocationTracking = false } }

eval CloseShowCallDialer state = continue state { props { showCallPopUp = false } }

eval (ShowCallDialer item) state = do
  case item of
    ANONYMOUS_CALLER -> callDriver state "ANONYMOUS"
    DIRECT_CALLER -> callDriver state "DIRECT"

eval (DriverInfoCardActionController (DriverInfoCardController.StartLocationTracking item)) state = continueWithCmd state [do pure $ StartLocationTracking item]

eval (StartLocationTracking item) state = do
  void $ pure $ performHapticFeedback unit
  case item of
    "GOOGLE_MAP" -> do
      let
        newState = state { props { isLocationTracking = false } }
      updateAndExit (newState) (OpenGoogleMaps newState)
    "IN_APP" -> exit $ InAppTrackStatus state { props { isInApp = not state.props.isInApp, isLocationTracking = false, forFirst = true } }
    _ -> continue state

eval (GetEstimates (GetQuotesRes quotesRes) count ) state = do
  logStatus "finding_estimates_and_quotes" quotesRes
  case null quotesRes.quotes of
    false -> do
      let _ = unsafePerformEffect $ Events.addEventData ("External.Search." <> state.props.searchId <> ".Type") "Quotes"      
      specialZoneFlow ( quotesRes.quotes) state
    true -> do
      let _ = unsafePerformEffect $ Events.addEventData ("External.Search." <> state.props.searchId <> ".Type") "Estimates"
      estimatesListFlow ( quotesRes.estimates) state count


eval (EstimatesTryAgain (GetQuotesRes quotesRes) count ) state = do
  case (getMerchant FunctionCall) of
    YATRI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    YATRISATHI -> estimatesListTryAgainFlow (GetQuotesRes quotesRes) state
    _ -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_estimate_try_again"
      let
        estimatedQuotes = quotesRes.estimates

        estimatedVarient = filter (\x -> x ^. _vehicleVariant == "AUTO_RICKSHAW") estimatedQuotes

        estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0

        estimateId = if isJust (estimatedVarient !! 0) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimateId else ""
      case (null estimatedVarient) of
        true -> do
          _ <- pure $ hideKeyboardOnNavigation true
          _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
          continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
        false -> do
          if (estimatedPrice > state.data.suggestedAmount) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, isEstimateChanged = true } }
          else do
            _ <- pure $ updateLocalStage FindingQuotes
            let
              updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = estimateId, currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } }
            updateAndExit updatedState $ GetQuotes updatedState


eval (GetQuotesList (SelectListRes resp)) state = do
  case spy "flowWithoutOffers" $ flowWithoutOffers WithoutOffers of
    true  -> do
      continueWithCmd state [pure $ ContinueWithoutOffers (SelectListRes resp)]
    false -> do
              let selectedQuotes = getQuoteList ((fromMaybe dummySelectedQuotes resp.selectedQuotes)^._selectedQuotes) state.props.city
              _ <- pure $ printLog "vehicle Varient " selectedQuotes
              let filteredQuoteList = filter (\a -> length (filter (\b -> a.id == b.id )state.data.quoteListModelState) == 0 ) selectedQuotes
              let removeExpired = filter (\a -> a.seconds > 0) filteredQuoteList
              _ <- pure $ spy "quotes" filteredQuoteList
              let quoteListModelState = state.data.quoteListModelState <> removeExpired
              if (getValueToLocalStore GOT_ONE_QUOTE == "FALSE") && (length quoteListModelState > 0) then do
                _ <- pure $ firebaseLogEvent "ny_user_received_quotes"
                _ <- pure $ setValueToLocalStore GOT_ONE_QUOTE "TRUE"
                pure unit
              else pure unit
              let newState = state{data{quoteListModelState = quoteListModelState },props{isSearchLocation = NoView, isSource = Nothing,currentStage = QuoteList}}
              if isLocalStageOn QuoteList then do
                let updatedState = if isTipEnabled state then tipEnabledState newState{props{isPopUp = TipsPopUp, findingQuotesProgress = 0.0}} else newState{props{isPopUp = ConfirmBack, findingQuotesProgress = 0.0}}
                logInfo "retry_finding_quotes" ( "QuoteList : Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
                exit $ GetSelectList updatedState
              else if(state.props.selectedQuote == Nothing && (getValueToLocalStore AUTO_SELECTING) /= "CANCELLED_AUTO_ASSIGN") then do
                let id = (fromMaybe dummyQuoteList (newState.data.quoteListModelState!!0)).id
                    nextState = newState{data{quoteListModelState = map (\x -> x{selectedQuote = (Just id)}) newState.data.quoteListModelState}, props{selectedQuote = if (id /= "") then Just id else Nothing}}
                _ <- pure $ setValueToLocalStore AUTO_SELECTING id
                logInfo "retry_finding_quotes" ( "SelectedQuote: Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
                continue nextState
              else do
                let quoteListEmpty = null newState.data.quoteListModelState
                logInfo "retry_finding_quotes" ( "Default :Current Stage: " <> (show newState.props.currentStage) <> " LOCAL_STAGE : " <> (getValueToLocalStore LOCAL_STAGE) <> "Estimate Id:" <> state.props.estimateId)
                _ <- pure $ setValueToLocalStore AUTO_SELECTING (if quoteListEmpty then "false" else getValueToLocalStore AUTO_SELECTING)
                continue newState{props{selectedQuote = if quoteListEmpty then Nothing else newState.props.selectedQuote}}

eval (ContinueWithoutOffers (SelectListRes resp)) state = do
  case resp.bookingId of
    Just bookingId -> do
      case STR.trim bookingId of
        "" -> continue state
        _  -> do
          _ <- pure $ updateLocalStage ConfirmingRide
          exit $ ConfirmRide state{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp, selectedQuote = Nothing}}
    Nothing -> do
      if isLocalStageOn QuoteList then do
        let onUs = state.data.selectedEstimatesObject.providerType == CTP.ONUS
            updatedState = if (isTipEnabled state && onUs) then tipEnabledState state{props{isPopUp = TipsPopUp, customerTip{enableTips = true}}} else state{props{isPopUp = ConfirmBack}} 
        continue updatedState 
        else continue state

eval (GetRideConfirmation resp) state = do
  logStatus "confirming_ride" resp
  case state.props.isSpecialZone of
    false -> normalRideFlow resp state
    true -> specialZoneRideFlow resp state

eval (NotificationListener notificationType) state = do
  _ <- pure $ printLog "storeCallBackCustomer notificationType" notificationType
  case notificationType of
    "DRIVER_QUOTE_INCOMING" -> continue state
    _ -> exit $ NotificationHandler notificationType state { props { callbackInitiated = false}}

eval RecenterCurrentLocation state = do
  recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.RecenterCurrentLocation)) state = recenterCurrentLocation state

eval (SearchLocationModelActionController (SearchLocationModelController.UpdateCurrentLocation lat lng)) state = do
  if state.props.isSource == Just true then
    updateCurrentLocation state lat lng
  else
    continue state

eval (UpdateCurrentLocation lat lng) state = updateCurrentLocation state lat lng

eval (CurrentLocation lat lng) state = do
  void $ pure $ setValueToLocalStore LAST_KNOWN_LAT lat
  void $ pure $ setValueToLocalStore LAST_KNOWN_LON lng
  if isLocalStageOn FindingEstimate
    then continue state
    else exit $ UpdatedState state { props { sourceLat = fromMaybe 0.0 (NUM.fromString lat), sourceLong = fromMaybe 0.0 (NUM.fromString lng) } } false

eval (RateCardAction RateCard.Close) state = continue state { props { showRateCard = false } , data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.BackPressed) state = continue state { props { showRateCard = false } ,data{rateCard{onFirstPage = false,currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.NoAction) state = continue state

eval (RateCardAction RateCard.GoToDefaultStart) state = continue state { data{rateCard{currentRateCardType = DefaultRateCard}}}

eval (RateCardAction RateCard.GoToDriverAddition) state = continue state { data{rateCard{currentRateCardType = DriverAddition,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToFareUpdate) state = continue state { data{rateCard{currentRateCardType = FareUpdate,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToWaitingCharges) state = continue state { data{rateCard{currentRateCardType = WaitingCharges,onFirstPage = true}}}

eval (RateCardAction RateCard.GoToTollOrParkingCharges) state = continue state { data{rateCard{currentRateCardType = TollOrParkingCharges,onFirstPage = true}}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = 
  continueWithCmd state { props { showMultipleRideInfo = false }, data {waitTimeInfo = false , iopState {providerPrefInfo = false }}} [ do
    pure $ (RequestInfoCardAction RequestInfoCard.BackPressed)
  ]

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = 
  if state.props.showSpecialZoneInfoPopup then
    continue state{ props{ showSpecialZoneInfoPopup = false } }
  else
    continue state { props { showMultipleRideInfo = false }, data {waitTimeInfo = false }}

eval SpecialZoneInfoTag state = 
  continue state{ props{ showSpecialZoneInfoPopup = true } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval (GenderBannerModal Banner.OnClick) state = exit $ GoToMyProfile state true

eval (UpdateProfileButtonAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  let newState = state{props{showEducationalCarousel = false}} 
  updateAndExit newState $ GoToMyProfile newState true

eval (DisabilityBannerAC Banner.OnClick) state = if (addCarouselWithVideoExists unit ) then continue state{props{showEducationalCarousel = true}} else exit $ GoToMyProfile state true

eval (TicketBookingFlowBannerAC Banner.OnClick) state = exit $ GoToTicketBookingFlow state

eval (MetroTicketBookingBannerAC Banner.OnClick) state = exit $ GoToMetroTicketBookingFlow state

eval (SkipAccessibilityUpdateAC PrimaryButtonController.OnClick) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  let _ = runFn2 updatePushInIdMap "bannerCarousel" true
  continue state{props{showEducationalCarousel = false}}

eval (DisabilityPopUpAC PopUpModal.OnButton1Click) state = do 
  _ <- pure $ pauseYoutubeVideo unit
  continue state{props{showDisabilityPopUp = false}}

eval (SafetyBannerAction Banner.OnClick) state = exit $ GoToNammaSafety state false $ state.props.sosBannerType == Just MOCK_DRILL_BANNER

eval ShowRateCard state = do
  continue state { props { showRateCard = true } }

eval (PopUpModalShareAppAction PopUpModal.OnButton1Click) state= continue state{props{showShareAppPopUp=false}}

eval (PopUpModalShareAppAction PopUpModal.OnButton2Click) state= do
  _ <- pure $ setValueToLocalStore SHARE_APP_COUNT "-1"
  let shareAppConfig = state.data.config.shareAppConfig
  _ <- pure $ shareTextMessage shareAppConfig.title shareAppConfig.description
  continue state{props{showShareAppPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton1Click) state= do
  void $ pure $ performHapticFeedback unit
  continue state{props{callSupportPopUp=false}}

eval (CallSupportAction PopUpModal.OnButton2Click) state= do
  void $ pure $ performHapticFeedback unit
  _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_ride_support_click"
  continue state{props{callSupportPopUp=false}}

eval (UpdateETA currentETA currentDistance) state = do
  let initDistance = state.data.driverInfoCardState.initDistance
  distance <- case initDistance of
                      Just initDistance -> pure initDistance
                      Nothing -> do
                                    let storedDistance = getValueToLocalStore PICKUP_DISTANCE
                                    if storedDistance == "0" || storedDistance == "__failed" || storedDistance == "(null)" then do
                                      _ <- pure $ setValueToLocalStore PICKUP_DISTANCE (show currentDistance)
                                      pure currentDistance
                                      else pure $ fromMaybe 0 (fromString storedDistance)
  let
    sheetState = if isLocalStageOn ChatWithDriver && state.props.currentSheetState == EXPANDED then Just COLLAPSED else state.props.sheetState
    currentSheetState =  if isLocalStageOn ChatWithDriver && state.props.currentSheetState == EXPANDED then COLLAPSED else state.props.currentSheetState
    newState = state { data { driverInfoCardState { eta = Just currentETA, distance = currentDistance, initDistance = Just distance } }, props {sheetState = sheetState, currentSheetState = currentSheetState}}
  continue newState

eval (RepeatRide index item) state = do 
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_repeat_trip"
  let _ = unsafePerformEffect $ Events.addEventData "External.Clicked.RepeatRide" "true"
  void $ pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS (show true)
  void $ pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT $ "4" 
  void $ pure $ setValueToLocalStore TEST_POLLING_INTERVAL $ "8000.0" 
  void $ pure $ setValueToLocalStore TEST_POLLING_COUNT $ "22" 
  updateAndExit state{props{currentStage = LoadMap, suggestedRideFlow = true}, data{settingSideBar { opened = SettingSideBarController.CLOSED }}} $ RepeatTrip state{props{isRepeatRide = true, suggestedRideFlow = true, repeatRideServiceTierName = item.serviceTierNameV2}} item

eval (ReferralFlowAction) state = 
  continue state{ props{ referral{ showAddReferralPopup = true }, referralComponentProps = HomeScreenData.initData.props.referralComponentProps } }

eval NewUser state = continueWithCmd state [ do
  if (getValueToLocalNativeStore REGISTRATION_APPROVED) == "true" then do
    _ <- pure $ setValueToLocalStore REGISTRATION_APPROVED "false"
    _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
      _ <- UI.successScreen ((getString HEY) <> " " <> (getValueToLocalStore USER_NAME)) (getString $ SUCCESSFUL_ONBOARD "SUCCESSFUL_ONBOARD")
      pure unit
    pure unit
    else
      pure unit
  pure NoAction
]

eval UpdateSourceFromPastLocations state = do
  let currLocationArr = (getNearestCurrentLocation state.props.sourceLat state.props.sourceLong state.data.previousCurrentLocations.pastCurrentLocations)
      currLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((currLocationArr)!!0))
      savedLocationArr = (getNearestSavedLocation state.props.sourceLat state.props.sourceLong state.data.savedLocations)
      arr = (sortBy compareByDistance (currLocationArr <> savedLocationArr))
      nearestLocation = (fromMaybe {locationDetails: {lat:0.0, lon : 0.0, placeName : ""}, distance: 0.0} ((arr)!!0))
  continue state{data{source = nearestLocation.locationDetails.placeName, sourceAddress = encodeAddress nearestLocation.locationDetails.placeName [] Nothing 0.0 0.0}}

eval (UpdateLocAndLatLong lat lng) state = do
  let slat = fromMaybe 0.0 (NUM.fromString lat)
      slng = fromMaybe 0.0 (NUM.fromString lng)
  continueWithCmd state{props{currentLocation { lat = slat, lng = slng } , sourceLat = slat, sourceLong = slng , locateOnMapLocation {sourceLat = slat, sourceLng = slng, source = state.data.source, sourceAddress = state.data.sourceAddress}}} [do
    if os == "IOS" && state.props.currentStage == HomeScreen then do
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9)
      pure unit
      else pure unit
    pure NoAction
  ]

eval GoToEditProfile state = do
  exit $ GoToMyProfile state true
eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = do
  continueWithCmd state{props{defaultPickUpPoint = config.id}} [do
      let focusedIndex = findIndex (\item -> item.place == config.id) state.data.nearByPickUpPoints
      case focusedIndex of
        Just index -> do
          _ <- pure $ scrollViewFocus (getNewIDWithTag "scrollViewParent") index
          pure unit
        Nothing -> pure unit
      _ <- animateCamera config.lat config.lng 25.0 "NO_ZOOM"
      pure NoAction
    ]
eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.NoAction config))) state = do
  let height = (runFn1 getLayoutBounds $ getNewIDWithTag config.id).height
      updatedState = state{props{defaultPickUpPoint = "", currentEstimateHeight = height, selectedEstimateHeight = if config.vehicleVariant == "BOOK_ANY" then state.props.selectedEstimateHeight else height}}
  continue updatedState

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.ServicesOnClick config item))) state = do
  let updatedServices = if elem item config.selectedServices then delete item config.selectedServices else insert item config.selectedServices
      selectedEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) updatedServices then acc <> [item.id] else acc) [] state.data.specialZoneQuoteList
      estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
      otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
      updatedQuotes = map (\item -> if item.vehicleVariant == "BOOK_ANY" then item{selectedServices = updatedServices}
                                    else item
                          ) state.data.specialZoneQuoteList 
  continue state{data{specialZoneQuoteList = updatedQuotes, otherSelectedEstimates = otherSelectedEstimates, selectedEstimatesObject = config{selectedServices = updatedServices, activeIndex = config.index}}, props {estimateId = estimateId}}

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.OnSelect config))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".ChooseVehicle") "true"
  let updatedQuotes = map (\item -> item{activeIndex = config.index}) state.data.specialZoneQuoteList
      props = if config.activeIndex == config.index then state.props else state.props{customerTip = HomeScreenData.initData.props.customerTip, tipViewProps = HomeScreenData.initData.props.tipViewProps}
      newState = state{data{specialZoneQuoteList = updatedQuotes}, props = props}
      selectedEstimates = if config.vehicleVariant == "BOOK_ANY" then foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) config.selectedServices then acc <> [item.id] else acc) [] state.data.specialZoneQuoteList else [] 
      estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
      otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
  void $ pure $ setValueToLocalNativeStore SELECTED_VARIANT (config.vehicleVariant)
  if state.data.currentSearchResultType == QUOTES then do
    continue newState{data{specialZoneSelectedQuote = Just config.id ,specialZoneSelectedVariant = Just config.vehicleVariant }}
  else continue newState{props{estimateId = estimateId }, data {selectedEstimatesObject = config{activeIndex = config.index}, otherSelectedEstimates = otherSelectedEstimates}}

eval (ChooseYourRideAction (ChooseYourRideController.ChooseVehicleAC (ChooseVehicleController.ShowRateCard config))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".RateCard") "true"
  continue state{ props { showRateCard = true }
                , data {  rateCard {  onFirstPage = false
                                    , vehicleVariant = config.vehicleVariant
                                    , currentRateCardType = DefaultRateCard
                                    , pickUpCharges = config.pickUpCharges
                                    , tollCharge = config.tollCharge
                                    , extraFare = config.extraFare
                                    , fareInfoDescription = config.fareInfoDescription
                                    , additionalFare = config.additionalFare
                                    , isNightShift = config.isNightShift
                                    , nightChargeTill = config.nightChargeTill
                                    , nightChargeFrom = config.nightChargeFrom
                                    , driverAdditions = config.driverAdditions
                                    }}}




eval (ChooseYourRideAction (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  let _ = unsafePerformEffect $ Events.addEventData ("External.Clicked.Search." <> state.props.searchId <> ".BookNow") "true"
      (Tuple estimateId otherSelectedEstimates) = spy "Praveen" $ getEstimateId state.data.specialZoneQuoteList state.data.selectedEstimatesObject 
  _ <- pure $ setValueToLocalStore FARE_ESTIMATE_DATA state.data.selectedEstimatesObject.price
  void $ pure $ setValueToLocalStore SELECTED_VARIANT (state.data.selectedEstimatesObject.vehicleVariant)
  if state.data.currentSearchResultType == QUOTES then  do
    _ <- pure $ updateLocalStage ConfirmingRide
    exit $ ConfirmRide state{props{currentStage = ConfirmingRide}}
  else if state.data.iopState.showMultiProvider then do 
    void $  pure $ updateLocalStage ProviderSelection 
    exit $ Cancel state { data { iopState { providerSelectionStage = true}, otherSelectedEstimates = otherSelectedEstimates}, props {estimateId = estimateId}}
  else do
    _ <- pure $ updateLocalStage FindingQuotes
    let customerTip = if state.props.tipViewProps.activeIndex == -1 then HomeScreenData.initData.props.customerTip else state.props.customerTip
        tipViewProps = if state.props.tipViewProps.activeIndex == -1 then HomeScreenData.initData.props.tipViewProps 
                          else if state.props.tipViewProps.stage == TIP_AMOUNT_SELECTED then state.props.tipViewProps{stage = TIP_ADDED_TO_SEARCH}
                          else state.props.tipViewProps
        updatedState = state{props{currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck"), customerTip = customerTip, tipViewProps = tipViewProps, estimateId = estimateId}, data{otherSelectedEstimates = otherSelectedEstimates}}
    void $ pure $ setTipViewData (TipViewData { stage : tipViewProps.stage , activeIndex : tipViewProps.activeIndex , isVisible : tipViewProps.isVisible })
    updateAndExit (updatedState) (GetQuotes updatedState)

eval (ChooseYourRideAction ChooseYourRideController.NoAction) state = do
  continue state{ props{ defaultPickUpPoint = "" } }

eval (QuoteListModelActionController (QuoteListModelController.CancelTimer)) state = do
  void $ pure $ clearTimerWithId state.data.iopState.timerId
  continue state { data { iopState { timerVal = "0"}}}

eval (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.ButtonClick (PrimaryButtonController.OnClick)))) state = do
  void $ pure $ updateLocalStage FindingQuotes
  void $ pure $ clearTimerWithId state.data.iopState.timerId
  void $ pure $ spy "ButtonClick state" state
  
  let updatedState = state{props{ currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck")}, data { iopState { providerSelectionStage = false}}}
  void $ pure $ spy "ButtonClick updatedState" updatedState
  updateAndExit (updatedState) (GetQuotes updatedState)

eval (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.FavClick item))) state = do
  let selectedItem = find (\quote -> quote.id == item.id) state.data.specialZoneQuoteList 
  void $ pure $ spy "quote" selectedItem
  case selectedItem of
    Just quote -> continue state { data { selectedEstimatesObject = quote}, props { estimateId = item.id}}
    _ -> continue state

eval (ProviderAutoSelected seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId timerID
    continueWithCmd state [pure $ (QuoteListModelActionController (QuoteListModelController.ProviderModelAC (PM.ButtonClick (PrimaryButtonController.OnClick))))]
  else continue state { data { iopState { timerVal = show seconds, timerId = timerID}}}-- update timer in ui

eval MapReadyAction state = do
  continueWithCmd state [ do
      permissionConditionA <- isLocationPermissionEnabled unit
      permissionConditionB <- isLocationEnabled unit
      internetCondition <- isInternetAvailable unit
      when (state.props.currentStage == HomeScreen) $ do
        void $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9)
      let action =  if( not internetCondition) then TriggerPermissionFlow INTERNET_ACTION
                    else if ( not (permissionConditionA && permissionConditionB)) then TriggerPermissionFlow LOCATION_DISABLED
                    else CheckAndAskNotificationPermission
      pure action
    ]

eval (ChooseYourRideAction( ChooseYourRideController.RadioButtonClick autoAssign)) state = 
  continueWithCmd state [ do
    pure (CheckBoxClick autoAssign)
  ]
  
eval (ChooseYourRideAction (ChooseYourRideController.OnIconClick autoAssign)) state = 
  continue state { props {showMultipleRideInfo = not autoAssign}}

eval (ChooseYourRideAction ChooseYourRideController.PreferencesDropDown) state = do
  continue state { data { showPreferences = not state.data.showPreferences}}

eval (ChooseYourRideAction ChooseYourRideController.SpecialZoneInfoTag) state = do
  continue state{ props{ showSpecialZoneInfoPopup = true } }

eval (ChooseYourRideAction (ChooseYourRideController.PrimaryButtonActionController (PrimaryButtonController.NoAction))) state = continueWithCmd state{data {triggerPatchCounter = state.data.triggerPatchCounter + 1}} [pure NoAction]

eval (ChooseYourRideAction (ChooseYourRideController.TipBtnClick index value)) state = do
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
      tip = fromMaybe 0 (customerTipArrayWithValues !! index)
      isTipSelected = tip > 0
      customerTip = if isTipSelected then 
                      state.props.customerTip {isTipSelected = isTipSelected, enableTips = isTipEnabled state, tipForDriver = tip, tipActiveIndex = index}
                      else HomeScreenData.initData.props.customerTip
      tipViewProps = if isTipSelected then 
                      state.props.tipViewProps{ stage = RETRY_SEARCH_WITH_TIP, activeIndex = index, onlyPrimaryText = true}
                      else HomeScreenData.initData.props.tipViewProps
  void $ pure $ setTipViewData (TipViewData { stage : tipViewProps.stage , activeIndex : tipViewProps.activeIndex , isVisible : tipViewProps.isVisible })
  continue state { props {customerTip = customerTip , tipViewProps = tipViewProps }}

eval (ChooseYourRideAction ChooseYourRideController.AddTip) state = do
  continue state { props { tipViewProps {stage = TIP_AMOUNT_SELECTED}}}

eval (ChooseYourRideAction ChooseYourRideController.ChangeTip) state = do
  continue state { props {tipViewProps { activeIndex = state.props.customerTip.tipActiveIndex, stage = TIP_AMOUNT_SELECTED}}} 

eval CheckAndAskNotificationPermission state = do 
  _ <- pure $ checkAndAskNotificationPermission false
  continue state

eval (TriggerPermissionFlow flowType) state = exit $ ExitToPermissionFlow flowType

eval (GenderBannerModal (Banner.OnClick)) state = exit $ GoToMyProfile state true

eval ReportIssueClick state = exit $  GoToHelp state

eval (DateTimePickerAction dateResp year month day timeResp hour minute) state = do 
  continue state

eval (LocationTagBarAC (LocationTagBarV2Controller.TagClicked tag)) state = do 
  case tag of 
    "RENTALS" -> exit $ GoToRentalsFlow 
    _ -> continue state
  
eval (RentalBannerAction Banner.OnClick) state = exit $ GoToScheduledRides

eval (BottomNavBarAction id) state = do 
  let newState = state {props {focussedBottomIcon = id}}
  case id of 
    TICKETING -> updateAndExit newState $ GoToTicketBookingFlow newState
    MOBILITY -> continue newState 
    _ -> continue state 
    
eval (SafetyAlertAction PopUpModal.OnButton1Click) state = do
  void $ pure $ cleverTapCustomEvent "ny_user_night_safety_mark_i_feel_safe"
  exit $ SafetySupport state{props{safetyAlertType = Nothing}} true

eval (SafetyAlertAction PopUpModal.OnButton2Click) state = do
    void $ pure $ cleverTapCustomEvent "ny_user_night_safety_mark_need_help"
    void $ pure $ setValueToLocalNativeStore SAFETY_ALERT_TYPE "false"
    exit $ GoToNammaSafety state{props{safetyAlertType = Nothing}} true false

eval (NotifyRideShare PrimaryButtonController.OnClick) state = exit $ GoToNotifyRideShare state

eval (ToggleShare index) state = continue state {data{contactList = Just $ mapWithIndex (\i item -> if index == i then item {isSelected = not item.isSelected} else item) (fromMaybe [] state.data.contactList)}}

eval (UpdateContacts contacts) state = continue state {data{contactList = Just $ contacts}}
eval (UpdateChatWithEM flag) state = continue state {props{isChatWithEMEnabled = flag}}
eval (ShareRideAction PopupWithCheckboxController.DismissPopup) state = continue state {props{showShareRide = false}}

eval (ShareRideAction (PopupWithCheckboxController.ClickPrimaryButton PrimaryButtonController.OnClick)) state = exit $ GoToNotifyRideShare state

eval (ShareRideAction (PopupWithCheckboxController.ClickSecondaryButton)) state = continueWithCmd state [pure ShareRide]

eval (ShareRideAction (PopupWithCheckboxController.ToggleSelect index)) state = do 
  let contacts = fromMaybe [] state.data.contactList
  case contacts !! index of 
    Just contactToUpdate -> do
      let updatedContactList = updateAt index contactToUpdate{isSelected = not contactToUpdate.isSelected} contacts
      continue state {
        data{
          contactList = updatedContactList
        }
      }
    Nothing -> continue state

eval (UpdateBookingDetails (RideBookingRes response)) state = do
  let rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status
      newState = state{ props { currentStage =
                      case rideStatus of
                        "NEW" -> RideAccepted
                        "INPROGRESS" -> RideStarted
                        "COMPLETED" -> RideCompleted
                        "CANCELLED" -> HomeScreen
                        _ -> RideAccepted
                    , bookingId = response.id
                    }, data { 
                      driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.currentSearchResultType == QUOTES)}}
  continue newState

eval (ReferralComponentAction componentAction) state =
  case componentAction of
    ReferralComponent.OnClickDone referralCode ->
      if STR.length referralCode == 6 then 
        continue state{ props{ referralComponentProps{ applyButtonActive = true, referralCode = Just referralCode } } }
      else
        continue state{ props{ referralComponentProps{ applyButtonActive = false } } }

    ReferralComponent.PopUpModalAction popUpAction ->
      case popUpAction of
        PopUpModal.OnButton1Click -> do
          case state.props.referral.referralStatus of
            REFERRAL_INVALID -> do
              void $ pure $ JB.showKeyboard (getNewIDWithTag "RefferalCode")
              continue state{ props{ referral{ referralStatus = NO_REFERRAL, showAddReferralPopup = true } } }
            REFERRAL_APPLIED -> do
              void $ pure $ setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
              continue state{ props{ referral{ referralStatus = NO_REFERRAL }, isReferred = true } } 
            _ -> continue state
        PopUpModal.OnButton2Click ->
          continue state{ props{ referral{ referralStatus = NO_REFERRAL, showAddReferralPopup = false } } }
        _ -> continue state

    ReferralComponent.ApplyAction buttonAction ->
      case buttonAction of
        PrimaryButtonController.OnClick ->
          case state.props.referralComponentProps.referralCode of
            Just code -> exit $ UpdateReferralCode state{ props{ referralComponentProps{ applyButtonActive = false } } } code
            Nothing -> continue state
        _ -> continue state

    ReferralComponent.SkipAction buttonAction ->
      case buttonAction of
        PrimaryButtonController.OnClick -> do
          void $ pure $ hideKeyboardOnNavigation true
          continue state{ props{ referralComponentProps{ stage = NO_REFERRAL_STAGE }, referral{ showAddReferralPopup = false } } }
        _ -> continue state

    ReferralComponent.OpenReferralProgramInfo ->
      continue state{ props{ referralComponentProps{ showReferralProgramInfoPopup = true } } }
    
    ReferralComponent.ReferredUserInfo PopUpModal.OnButton2Click ->
      continue state{ props{ referralComponentProps{ showReferredUserInfoPopup = false } } }
    
    ReferralComponent.ReferralProgramInfo PopUpModal.OnButton2Click -> 
      continue state{ props{ referralComponentProps{ showReferralProgramInfoPopup = false } } }

    _ -> continue state

eval GoToHomeScreen state = do
  logStatus "confirming_ride" "no_active_ride"
  exit $ GoToHome state

eval (AcWorkingPopupAction (PopUpModal.OnButton1Click)) state = do
  let isAcCabRide = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing
  if isAcCabRide then
    void $ pure $ toast $ getString GREAT_ENJOY_THE_TRIP
  else pure unit
  void $ pure $ setValueToCache (show AC_POPUP_SHOWN_FOR_RIDE) state.data.driverInfoCardState.rideId (\id -> id)
  continue state{props{showAcWorkingPopup = false}}

eval (AcWorkingPopupAction (PopUpModal.OnButton2Click)) state = do
  void $ pure $ setValueToCache (show AC_POPUP_SHOWN_FOR_RIDE) state.data.driverInfoCardState.rideId (\id -> id)
  continue state{props{showAcWorkingPopup = false}}

eval (AcWorkingPopupAction PopUpModal.DismissPopup) state = continue state{props{showAcWorkingPopup = false}}

eval NoRender state = update state

eval _ state = update state

validateSearchInput :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
validateSearchInput state searchString =
  if STR.length (STR.trim searchString) > 2 && searchString /= state.data.source && searchString /= (getString CURRENT_LOCATION) && (searchString /= state.data.destination || ((getSearchType unit) == "direct_search") && (state.props.isSearchLocation == SearchLocation)) then
    callSearchLocationAPI
  else
    continue state
  where
  autoCompleteType = if state.props.isSource == Just true then Just ST.PICKUP else Just ST.DROP
  sourceManuallyMoved = if state.props.isSource == Just true then false else state.props.rideSearchProps.sourceManuallyMoved
  destManuallyMoved = if state.props.isSource == Just false then false else state.props.rideSearchProps.destManuallyMoved
  callSearchLocationAPI = updateAndExit state{props{ searchLocationModelProps{showLoader = true}}} $ SearchPlace searchString state{ props{ rideSearchProps{ autoCompleteType = autoCompleteType, sourceManuallyMoved = sourceManuallyMoved, destManuallyMoved = destManuallyMoved } } }

constructLatLong :: Number -> Number -> String -> JB.Location
constructLatLong lat lng _ =
  { lat: lat
  , lng: lng
  , place: ""
  , address: Nothing
  , city : Nothing
  , isSpecialPickUp : Nothing
  }

addItemToFeedbackList :: Array String -> String -> Array String
addItemToFeedbackList feedbackList feedbackItem = if (any (_ == feedbackItem) feedbackList ) then (filter (\item -> feedbackItem /= item) feedbackList) else snoc feedbackList feedbackItem

checkPermissionAndUpdatePersonMarker :: HomeScreenState -> Effect Unit
checkPermissionAndUpdatePersonMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  let
    conditionC = (state.props.currentStage == HomeScreen)
  if (conditionA && conditionB && conditionC) then do
    _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
    pure unit
  else do
    if (os == "IOS" && conditionC) then do
      _ <- getLocationName (showPersonMarker state (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))) 9.9 9.9 "Current Location" constructLatLong
      pure unit
    else pure unit

updateFeedback :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
updateFeedback feedbackId feedbackItem feedbackList =
  if hasFeedbackId feedbackId feedbackList
    then updateFeedbackAnswer feedbackId feedbackItem feedbackList
    else addFeedbackAnswer feedbackId feedbackItem feedbackList
  where
    hasFeedbackId :: String -> Array FeedbackAnswer -> Boolean
    hasFeedbackId fid list = any (\feedback -> feedback.questionId == fid) list

    updateFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    updateFeedbackAnswer fid newItem list =
      map (\feedback ->
        if feedback.questionId == fid
                then feedback { answer = if newItem `elem` (feedback.answer) then filter (\x -> x /= newItem) feedback.answer else feedback.answer <> [newItem] }
          else feedback
        ) list

    addFeedbackAnswer :: String -> String -> Array FeedbackAnswer -> Array FeedbackAnswer
    addFeedbackAnswer fid newItem list = do
      let config = {questionId : fid, answer : [newItem]}
      list <> [config]

showPersonMarker :: HomeScreenState -> String -> JB.Location -> Effect Unit
showPersonMarker state marker location = do
  when (state.props.currentStage == HomeScreen) $ do
    void $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) location.lat location.lng 160 0.5 0.9
  void $ pure $ printLog "Location :: " location
  animateCamera location.lat location.lng zoomLevel "ZOOM"

cancelReasons :: Boolean -> Array OptionButtonList
cancelReasons showAcReason =
  ([ { reasonCode: "CHANGE_OF_PLANS"
    , description: getString CHANGE_OF_PLANS
    , subtext: Just $ getString NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS
    , textBoxRequired : false
    }
  ]) <>
  (if showAcReason 
      then [{ reasonCode: "AC_NOT_TURNED_ON"
            , description: getString AC_IS_NOT_AVAILABLE_ON_THIS_RIDE
            , subtext: Just $ getString AC_NOT_WORKING_DESC
            , textBoxRequired : false
            }]
      else []
  ) <>
  ([
    { reasonCode: "GOT_ANOTHER_RIDE"
    , description: getString GOT_ANOTHER_RIDE_ELSE_WHERE
    , subtext: Just $ getString CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP
    , textBoxRequired : false
    }
  , { reasonCode: "DRIVER_NOT_MOVING"
    , description: getString DRIVER_IS_NOT_MOVING
    , subtext: Just $ getString DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP
    , textBoxRequired : false
    }
  , { reasonCode: "WAIT_TIME_TOO_LONG"
    , description: getString WAIT_TIME_TOO_LONG
    , subtext: Just $ getString DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION
    , textBoxRequired : false
    }
  , { reasonCode: "WRONG_PICKUP_LOCATION"
    , description: getString WRONG_PICKUP_LOCATION
    , subtext: Just $ getString THE_PICKUP_LOCATION_ENTERED_WAS_WRONG
    , textBoxRequired : false
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    , subtext: Just $ getString SOME_OTHER_REASON
    , textBoxRequired : true
    }
  ])

getEstimateId :: Array ChooseVehicleController.Config -> ChooseVehicleController.Config -> (Tuple String (Array String)) 
getEstimateId esimates config =
  let selectedEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) config.selectedServices then acc <> [item.id] else acc) [] esimates
      estimateId = if config.vehicleVariant == "BOOK_ANY" then fromMaybe "" (head selectedEstimates) else config.id
      otherSelectedEstimates = fromMaybe [] $ tail $ selectedEstimates
  in (Tuple estimateId otherSelectedEstimates)

dummyCancelReason :: OptionButtonList
dummyCancelReason =
  { reasonCode: ""
  , description: ""
  , textBoxRequired: false
  , subtext: Nothing
  }

dummyRideRatingState :: RatingCard
dummyRideRatingState = {
  rating              : 0,
  driverName          : "",
  rideId              :  "",
  finalAmount         : 0.0,
  rideStartTime       : "",
  rideStartDate       : "",
  rideEndTime         : "",
  source              : "",
  destination         : "",
  vehicleNumber       : "",
  status              : "",
  shortRideId         : "",
  bookingId           : "",
  rideEndTimeUTC      : "",
  dateDDMMYY          : "",
  offeredFare         : 0.0,
  distanceDifference  : 0,
  feedback            : "",
  feedbackList        : []
}
dummyListItem :: LocationListItemState
dummyListItem = {
    prefixImageUrl : ""
  , postfixImageUrl : ""
  , postfixImageVisibility : false
  , lat : Nothing
  , lon : Nothing
  , placeId : Nothing
  , subTitle : ""
  , title : ""
  , description : ""
  , tag : ""
  , tagType : Nothing
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : HomeScreenData.dummyAddress
  , locationItemType : Nothing
  , distance : Nothing
  , showDistance : Just false
  , actualDistance : Nothing
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
}

tagClickEvent :: CardType -> (Maybe LocationListItemState) -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
tagClickEvent savedAddressType arrItem state = do
    let stage' = if os == "IOS" && state.props.currentStage == HomeScreen then ConfirmingLocation else LoadMap
    case savedAddressType, arrItem of
        OTHER_TAG,_  -> do
          _ <- pure $ updateLocalStage FavouriteLocationModel
          continue state{props{currentStage = FavouriteLocationModel}}
        _,Nothing    -> do
          if (length state.data.savedLocations >= 20) then do
            _ <- pure $ toast (getString SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES)
            continue state
            else updateAndExit state{props{tagType = Just savedAddressType}}  $ CheckFavDistance state{props{tagType = Just savedAddressType}}
        _,Just item  -> do
          if state.props.isSource == Just true then do
              let newState = state {data{ source = item.description, sourceAddress = item.fullAddress},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = ST.FAVOURITE } }}
              pure $ setText (getNewIDWithTag "SourceEditText") item.description
              pure $ removeMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME))
              updateAndExit state{props{currentStage = stage'}} $ LocationSelected item false newState
            else do
              let newState = state {data{ destination = item.description,destinationAddress = item.fullAddress},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
              pure $ setText (getNewIDWithTag "DestinationEditText") item.description
              pure $ removeMarker $ getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)
              updateAndExit state{props{currentStage = stage'}} $ LocationSelected item false newState

flowWithoutOffers :: LazyCheck -> Boolean
flowWithoutOffers dummy = not $ (getValueToLocalStore FLOW_WITHOUT_OFFERS) == "false"

recenterCurrentLocation :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
recenterCurrentLocation state = continueWithCmd state [ do
    if state.props.locateOnMap || (not state.props.locateOnMap && state.props.currentStage == ConfirmingLocation) then do
      _ <- pure $ currentPosition "NO_ZOOM"
      pure unit
    else do
      _ <- pure $ currentPosition ""
      _ <- addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 (0.5) (0.9)
      pure unit
    -- let newState = state{data{source = state.props.currentLocation.place}}
    pure NoAction
  ]

updateCurrentLocation :: HomeScreenState -> String -> String -> Eval Action  ScreenOutput HomeScreenState
updateCurrentLocation state lat lng = exit $ (CheckLocServiceability state (fromMaybe 0.0 (NUM.fromString lat )) (fromMaybe 0.0 (NUM.fromString lng)))

locationSelected :: LocationListItemState -> Boolean -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
locationSelected item addToRecents state = do
  let stage' = if os == "IOS" && state.props.currentStage == HomeScreen then ConfirmingLocation else LoadMap
  _ <- pure $ hideKeyboardOnNavigation true
  let favClick = if item.postfixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" then "true" else "false"
  if state.props.isSource == Just true then do
    let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_pickup_select" $ [ {key : "Source", value : unsafeToForeign item.title},
                                                                                                              {key : "Favourite", value : unsafeToForeign favClick}]
        sourceSelectType = if item.tag /= "" then ST.FAVOURITE else ST.SEARCH
        newState = state {data{ source = item.title, sourceAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)},props{sourcePlaceId = item.placeId,sourceLat = fromMaybe 0.0 item.lat,sourceLong =fromMaybe 0.0  item.lon, rideSearchProps{ sourceSelectType = sourceSelectType } }}
    pure $ setText (getNewIDWithTag "SourceEditText") item.title
    updateAndExit state $ LocationSelected item addToRecents newState
    else do
      let _ = unsafePerformEffect $ logEventWithMultipleParams state.data.logField  "ny_user_destination_select" $ [{key : "Destination", value : unsafeToForeign item.title},
                                                                                                                    {key : "Favourite", value : unsafeToForeign favClick}]
      let newState = state {data{ destination = item.title,destinationAddress = encodeAddress (item.title <> ", " <>item.subTitle) [] item.placeId (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)},props{destinationPlaceId = item.placeId, destinationLat = fromMaybe 0.0 item.lat, destinationLong = fromMaybe 0.0 item.lon}}
      pure $ setText (getNewIDWithTag "DestinationEditText") item.title
      updateAndExit state{props{currentStage = stage'}} $ LocationSelected item addToRecents newState

checkCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Boolean
checkCurrentLocation lat lon previousCurrentLocations =  (length (filter (\ (item) -> (filterFunction lat lon item))(previousCurrentLocations)) > 0)

checkSavedLocations :: Number -> Number -> Array LocationListItemState -> Boolean
checkSavedLocations lat lon savedLocations = (length (filter(\item -> (filterSavedLocations lat lon item)) (savedLocations)) > 0 )
filterSavedLocations :: Number -> Number -> LocationListItemState -> Boolean
filterSavedLocations lat lon savedLocation = not ((getDistanceBwCordinates lat lon (fromMaybe 0.0 savedLocation.lat) (fromMaybe 0.0 savedLocation.lon)) > 0.05)

filterFunction :: Number -> Number -> CurrentLocationDetails -> Boolean
filterFunction lat lon   currLocation = not ((getDistanceBwCordinates lat lon (currLocation.lat) (currLocation.lon)) > 0.05)

getNearestCurrentLocation :: Number -> Number -> Array CurrentLocationDetails -> Array CurrentLocationDetailsWithDistance
getNearestCurrentLocation lat lon previousCurrentLocations =  (sortBy compareByDistance (map (\ (item) ->
  { distance : (getDistanceBwCordinates lat lon (item.lat) (item.lon)),
    locationDetails : item
  })
  (previousCurrentLocations)))

getNearestSavedLocation :: Number -> Number -> Array LocationListItemState -> Array CurrentLocationDetailsWithDistance
getNearestSavedLocation lat lon savedLocations = (sortBy compareByDistance (map(\item ->
  { distance : (getDistanceBwCordinates lat lon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon)),
    locationDetails :  {lat : (fromMaybe 0.0 item.lat), lon : (fromMaybe 0.0 item.lon), placeName : (item.description)}
  }) (savedLocations)))

compareByDistance :: CurrentLocationDetailsWithDistance -> CurrentLocationDetailsWithDistance -> Ordering
compareByDistance ( a) ( b) = compare (a.distance ) (b.distance)

updateMessagesWithCmd :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ChatWithDriver) then do
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (length state.data.messages))
      pure unit
    else
      pure unit
    if state.props.showChatNotification then pure $ (DriverInfoCardActionController (DriverInfoCardController.CollapseBottomSheet)) else pure NoAction
    ]

dummySelectedQuotes :: SelectedQuotes
dummySelectedQuotes = SelectedQuotes {
  selectedQuotes : []
}

getSearchExpiryTime :: String -> Int
getSearchExpiryTime dummy =
  let count = fromMaybe 0 (fromString (getValueToLocalStore TEST_POLLING_COUNT))
      interval = (fromMaybe 0.0 (NUM.fromString (getValueToLocalStore TEST_POLLING_INTERVAL)) / 1000.0)
      searchExpiryTime = round $ (toNumber count) * interval
  in searchExpiryTime

tipEnabledState :: HomeScreenState -> HomeScreenState
tipEnabledState state = do
  let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
      isTipEnabled = not $ null customerTipArrayWithValues
      tipActiveIndex = if state.props.customerTip.tipActiveIndex == -1 then 1 else state.props.customerTip.tipActiveIndex
  state { props{customerTip {isTipSelected = isTipEnabled, tipForDriver= (fromMaybe 0 (customerTipArrayWithValues !! tipActiveIndex)), tipActiveIndex = tipActiveIndex}}}

isTipEnabled :: HomeScreenState -> Boolean
isTipEnabled state =
    let tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
        customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
    in not $ null customerTipArrayWithValues

specialZoneFlow :: Array OfferRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
specialZoneFlow estimatedQuotes state = do
  let quoteList = getSpecialZoneQuotes estimatedQuotes state.data.config.estimateAndQuoteConfig
      defaultQuote = fromMaybe ChooseVehicleController.config (quoteList !! 0)
  if ((not (null quoteList)) && (isLocalStageOn FindingEstimate)) then do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote"
    _ <- pure $ updateLocalStage SettingPrice
    _ <- pure $ setValueToLocalStore SELECTED_VARIANT (defaultQuote.vehicleVariant)
    continue state { data {specialZoneQuoteList = quoteList, currentSearchResultType = QUOTES, specialZoneSelectedQuote = Just defaultQuote.id, specialZoneSelectedVariant = Just defaultQuote.vehicleVariant}, props {currentStage = SettingPrice, specialZoneType = "OneWaySpecialZoneAPIDetails"}}
  else do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ updateLocalStage SearchLocationModel
    _ <- pure $ toast (getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN)
    continue state { props {currentStage = SearchLocationModel}, data{currentSearchResultType = QUOTES}}

estimatesListFlow :: Array EstimateAPIEntity -> HomeScreenState -> Int -> Eval Action ScreenOutput HomeScreenState
estimatesListFlow estimates state count = do
  let 
    repeatRideFailCheck =  not $ checkRecentRideVariantInEstimates estimates state.props.repeatRideServiceTierName -- check if the repeat ride variant is available in the estimates
    newState = if state.props.isRepeatRide && repeatRideFailCheck then state { props {isRepeatRide = false}} else state -- if repeat ride is enabled and the variant is not available in the estimates then disable repeat ride
    alreadyGotEstimates = not $ null $ state.data.specialZoneQuoteList 
    showMultiProvider' = if alreadyGotEstimates then state.data.iopState.showMultiProvider else not $ any (\(EstimateAPIEntity element) -> element.isValueAddNP == Just true) estimates -- if we already got the estimate show current screen only else if we have NY show ny provider else show multi provider

    quoteList = getEstimateList estimates newState.data.config.estimateAndQuoteConfig (Just count) newState.data.selectedEstimatesObject.activeIndex

    defaultQuote = fromMaybe ChooseVehicleController.config $ 
      if newState.props.isRepeatRide then do 
        let nYQuotes = filter (\item ->item.providerType == ONUS) quoteList
            defaultQuote_ = find (\item -> isJust item.serviceTierName && item.serviceTierName == newState.props.repeatRideServiceTierName) nYQuotes
        if isJust defaultQuote_ 
          then defaultQuote_
        else (quoteList !! newState.data.selectedEstimatesObject.activeIndex)
      else 
        if showMultiProvider' then 
          quoteList !! newState.data.selectedEstimatesObject.activeIndex
        else 
          let nYQuotes = filter (\item ->item.providerType == ONUS) quoteList -- NY Quotes
          in nYQuotes !! newState.data.selectedEstimatesObject.activeIndex 


    zoneType = getSpecialTag $ case head estimates of
                  Just entity -> view _specialLocationTag entity
                  Nothing -> Nothing

    hasToll = any (\item -> maybe false (\fareBreakupList -> isEstimateFareBreakupHastitle fareBreakupList "TOLL_CHARGES") (item ^. _estimateFareBreakup)) estimates

    topProviderEstimates = filter (\element -> element.providerType == ONUS) quoteList -- filter the ny provider estimates
    topProviderCheck = if state.data.currentCityConfig.iopConfig.enable then true else not $ null topProviderEstimates -- if iop is not enabled then show ny provider else show multi provider
    hasQuotes = not $ null quoteList
    
  if hasQuotes && topProviderCheck then do -- if choosing multiple provider is not enabled then only show ny
    let 
      _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_quote"
      nearByDrivers = getNearByDrivers estimates
      nearByDriversLength = length nearByDrivers
      (Tuple estimateId otherSelectedEstimates) = if defaultQuote.vehicleVariant == "BOOK_ANY" then getSelectedEstimates defaultQuote quoteList else (Tuple defaultQuote.id []) 
      _ = runFn2 updatePushInIdMap "EstimatePolling" true
      quoteList' = map (\quote -> quote{activeIndex = defaultQuote.index}) quoteList

    void $ pure $ updateLocalStage SettingPrice
    logStatus "drivers_available" nearByDriversLength

    void $ pure $ setValueToLocalStore HAS_TOLL_CHARGES $ show hasToll
    exit $ SelectEstimate newState 
      { data
        { specialZoneQuoteList = quoteList'
        , currentSearchResultType = ESTIMATES
        , selectedEstimatesObject = defaultQuote
        , nearByDrivers = if nearByDriversLength > 0 then Just nearByDriversLength else Nothing
        , iopState { 
          showPrefButton = state.data.currentCityConfig.iopConfig.enable && (not (null topProviderEstimates)) && (not newState.props.isRepeatRide)
          , providerPrefInfo = state.data.iopState.providerPrefInfo
          , hasTopProviderEstimate = not $ null topProviderEstimates
          , showMultiProvider = showMultiProvider'
          }
        , otherSelectedEstimates = otherSelectedEstimates 
        }
      , props
        { currentStage = SettingPrice
        , estimateId = estimateId
        , zoneType = zoneType
        , hasToll = hasToll
        }
      }
  else do
    logStatus "drivers_available" "no_drivers_available"
    void $ pure $ hideKeyboardOnNavigation true
    void $ pure $ updateLocalStage SearchLocationModel
    void $ pure $ toast $ getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
    let _ = runFn2 updatePushInIdMap "EstimatePolling" true
    continue newState { props {currentStage = SearchLocationModel}, data{currentSearchResultType = ESTIMATES}}

  where 
    isEstimateFareBreakupHastitle fareBreakUpList title = any (\item -> item ^. _title == title) fareBreakUpList

    getSelectedEstimates :: ChooseVehicleController.Config -> Array ChooseVehicleController.Config -> Tuple String (Array String) 
    getSelectedEstimates quote quotes = 
      let filteredEstimates = foldl(\acc item -> if elem (fromMaybe "" item.serviceTierName) quote.selectedServices then acc <> [item.id] else acc) [] quotes
      in (Tuple (fromMaybe "" $ head filteredEstimates) (fromMaybe [] $ tail filteredEstimates))

estimatesListTryAgainFlow :: GetQuotesRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
estimatesListTryAgainFlow (GetQuotesRes quotesRes) state = do
  let
    estimates = quotesRes.estimates
    estimatedVarient = filter (\x -> x ^. _vehicleVariant == state.data.selectedEstimatesObject.vehicleVariant) estimates
    estimatedPrice = if (isJust (estimatedVarient !! 0)) then (fromMaybe dummyEstimateEntity (estimatedVarient !! 0)) ^. _estimatedFare else 0
    quoteList = getEstimateList estimatedVarient state.data.config.estimateAndQuoteConfig Nothing  state.data.selectedEstimatesObject.activeIndex
    defaultQuote = fromMaybe ChooseVehicleController.config (quoteList !! 0)
  case (null estimatedVarient) of
    true -> do
      _ <- pure $ hideKeyboardOnNavigation true
      _ <- pure $ toast $ getString NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN
      continue state { props { currentStage = SearchLocationModel, rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = true, isDestServiceable = true, isRideServiceable = true } }
    false -> do
      if (estimatedPrice >  state.data.selectedEstimatesObject.basePrice) then
            continue state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, isEstimateChanged = true } }
      else do
        _ <- pure $ updateLocalStage FindingQuotes
        let updatedState = state { data { suggestedAmount = estimatedPrice }, props { estimateId = defaultQuote.id, currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } }
        updateAndExit updatedState $ GetQuotes updatedState


normalRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
normalRideFlow  (RideBookingRes response) state = do
  let rideStatus = (fromMaybe dummyRideAPIEntity ((response.rideList) !! 0)) ^. _status
      newState = state{ props { currentStage =
            case rideStatus of
              "NEW" -> RideAccepted
              "INPROGRESS" -> RideStarted
              "COMPLETED" -> RideCompleted
              "CANCELLED" -> HomeScreen
              _ -> RideAccepted
          , isSearchLocation = NoView
          , bookingId = response.id
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.currentSearchResultType == QUOTES)
          }}
  exit $ RideConfirmed newState { props { isInApp = true } }

specialZoneRideFlow :: RideBookingRes -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
specialZoneRideFlow  (RideBookingRes response) state = do
  let
    newState =
      state
        { props
          { currentStage = RideAccepted
          , isSearchLocation = NoView
          , bookingId = response.id
          }
        , data
          { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes response) (state.data.currentSearchResultType == QUOTES)
          }
        }
  exit $ RideConfirmed newState { props { isInApp = true } }

getRateCardArray :: Boolean -> String -> Int -> Int -> Int -> Array {title :: String , description :: String}
getRateCardArray nightCharges lang baseFare extraFare additionalFare = ([ { title :(getString $ MIN_FARE_UPTO "2 km") <> if nightCharges then " 🌙" else "" , description : "₹" <> toStringJSON (baseFare) }
                      , { title : (getString RATE_ABOVE_MIN_FARE) <> if nightCharges then " 🌙" else "", description : "₹" <> toStringJSON (extraFare) <> " / km"} ]
                      <> if (getMerchant FunctionCall) == NAMMAYATRI && additionalFare > 0 then [ {title : (getString DRIVER_ADDITIONS) , description : (getString PERCENTAGE_OF_NOMINAL_FARE)}] else [])

findingQuotesSearchExpired :: Boolean -> Int
findingQuotesSearchExpired gotQuotes =
  let secondsPassed = getExpiryTime (getValueToLocalStore FINDING_QUOTES_START_TIME) true
      searchExpiryTime = getSearchExpiryTime "LazyCheck"
      secondsLeft = case gotQuotes of
                      true  -> if (searchExpiryTime - secondsPassed) < 30 then (searchExpiryTime - secondsPassed) else 30
                      false -> (searchExpiryTime - secondsPassed)
  in secondsLeft

callDriver :: HomeScreenState -> String -> Eval Action ScreenOutput HomeScreenState
callDriver state callType = do
  let newState = state{props{ showCallPopUp = false }}
      driverNumber = case callType of
                        "DIRECT" ->(fromMaybe state.data.driverInfoCardState.merchantExoPhone state.data.driverInfoCardState.driverNumber)
                        _ -> if (STR.take 1 state.data.driverInfoCardState.merchantExoPhone) == "0" then state.data.driverInfoCardState.merchantExoPhone else "0" <> state.data.driverInfoCardState.merchantExoPhone
  updateWithCmdAndExit newState
    [ do
        _ <- pure $ showDialer driverNumber false
        let _ = unsafePerformEffect $ logEventWithTwoParams state.data.logField ("ny_user_"<> callType <>"_call_click") "trip_id" (state.props.bookingId) "user_id" (getValueToLocalStore CUSTOMER_ID)
        pure NoAction
    ] $ CallDriver newState (if callType == "DIRECT" then DIRECT_CALLER else ANONYMOUS_CALLER) driverNumber

getInfoCardPeekHeight :: HomeScreenState -> Int
getInfoCardPeekHeight state = 
  let bottomSheetLayout = (runFn1 getLayoutBounds $ getNewIDWithTag (if state.data.currentSearchResultType == QUOTES then "driverInfoViewSpecialZone" else "driverInfoView"))
      brandingBanner = runFn1 getLayoutBounds $ getNewIDWithTag "BrandingBanner"
      actionsView = runFn1 getLayoutBounds $ getNewIDWithTag "DriverInfoCardActionView"
      pixels = runFn1 getPixels FunctionCall
      density = (runFn1 getDeviceDefaultDensity FunctionCall)/  defaultDensity
      currentPeekHeight = if bottomSheetLayout.height == 0 || actionsView.height == 0
                          then 0
                          else bottomSheetLayout.height + brandingBanner.height + actionsView.height
      requiredPeekHeight = if os /= "IOS" then ceil (((toNumber currentPeekHeight) /pixels) * density) else currentPeekHeight
    in requiredPeekHeight

getPeekHeight :: HomeScreenState -> Int
getPeekHeight state = 
      let homescreenHeader =  (runFn1 getLayoutBounds (getNewIDWithTag "homescreenHeader")).height
          scrHeight = (getDeviceHeight unit)
          requiredPeekHeight = if os == "IOS"
                                then getPeekHeightForIos homescreenHeader scrHeight
                                else  getPeekHeightForAndroid homescreenHeader
      in if homescreenHeader == 0 then 500 else requiredPeekHeight
      where 
        getPeekHeightForIos :: Int -> Int -> Int
        getPeekHeightForIos homescreenHeader scrHeight =
          let iosScale = runFn1 getPixels FunctionCall
              iosNativeScale = runFn1 getDefaultPixels ""
              displayZoomFactor = iosNativeScale / iosScale
          in ceil((( (toNumber scrHeight) / displayZoomFactor)/ iosScale) - (toNumber homescreenHeader) )

        getPeekHeightForAndroid :: Int -> Int
        getPeekHeightForAndroid homescreenHeader =
          let androidPixels = runFn1 getPixels FunctionCall
              androidDensity = (runFn1 getDeviceDefaultDensity FunctionCall)/  defaultDensity
          in (screenHeight unit) - ( ceil(((toNumber homescreenHeader)/androidPixels) *androidDensity))

checkRecentRideVariant :: HomeScreenState -> Boolean
checkRecentRideVariant state = any (\item -> item.providerType == ONUS && isJust item.serviceTierName && item.serviceTierName == state.props.repeatRideServiceTierName) state.data.specialZoneQuoteList 

checkRecentRideVariantInEstimates :: Array EstimateAPIEntity -> Maybe String -> Boolean
checkRecentRideVariantInEstimates estimates repeatRideServiceName = any (\(EstimateAPIEntity item) -> item.isValueAddNP == Just true && isJust item.serviceTierName && item.serviceTierName == repeatRideServiceName) estimates 

openLiveDashboard :: HomeScreenState -> Eval Action ScreenOutput HomeScreenState
openLiveDashboard state = do 
  void $ pure $ setValueToLocalStore LIVE_DASHBOARD "LIVE_DASHBOARD_SELECTED"
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_live_stats_dashboard"
      dashboardUrl = if STR.null state.data.currentCityConfig.dashboardUrl then state.data.config.dashboard.url else state.data.currentCityConfig.dashboardUrl
  if os == "IOS" then do
    continueWithCmd state [do
      void $ openUrlInApp dashboardUrl
      pure NoAction
    ]
  else continue state {props {showLiveDashboard = true}}
