{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Utils (toggleLoader)
import Engineering.Helpers.Commons (markPerformance)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), (<$>), pure, void)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.HomeScreen.Controller (ScreenOutput(..))
import Screens.HomeScreen.View as HomeScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..), HOME_SCREEN_OUTPUT(..))
import Screens.Types (BottomNavBarIcon(..))
import Screens.HomeScreen.Transformer(getTripDetailsState)
import Presto.Core.Types.Language.Flow (getLogFields)
import Debug

homeScreen ::FlowBT String HOME_SCREEN_OUTPUT
homeScreen = do
  liftFlowBT $ markPerformance "HOME_SCREEN_RUN"
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ HomeScreen.screen state.homeScreen
  void $ lift $ lift $ toggleLoader false
  case act of
    UpdateLocationName updatedState lat lng-> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_LOCATION_NAME updatedState lat lng)
    UpdatePickupName updatedState lat lng -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> pure (UPDATE_PICKUP_NAME updatedState lat lng)
    PastRides updatedState fromBanner-> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_MY_RIDES fromBanner)
    GoToHelp updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_HELP)
    ChangeLanguage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure CHANGE_LANGUAGE)
    GoToMyTickets updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_MY_TICKETS)
    GoToAbout updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_ABOUT)
    GoToMyProfile updatedState updateProfile -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_MY_PROFILE updateProfile)
    GoToFavourites updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure GO_TO_FAVOURITES_ )
    LocationSelected selectedItem addToRecents updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.BackPoint <$> (pure $ LOCATION_SELECTED selectedItem addToRecents)
    SearchPlace input updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SEARCH_LOCATION input updatedState)
    GetQuotes updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ GET_QUOTES updatedState)
    GoToTicketBookingFlow updatedState -> do 
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState{props{focussedBottomIcon = MOBILITY}})
          App.BackT $ App.BackPoint <$> (pure $ EXIT_TO_TICKETING updatedState)
    GoToMetroTicketBookingFlow updatedState -> do 
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.BackPoint <$> (pure $ GO_TO_METRO_BOOKING updatedState)
    LogoutUser -> App.BackT $ App.NoBack <$> (pure $ LOGOUT)
    SelectEstimate updatedState -> do
          modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
          App.BackT $ App.NoBack <$> (pure $ SELECT_ESTIMATE updatedState)
    GetSelectList updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GET_SELECT_LIST updatedState)
    ConfirmRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CONFIRM_RIDE updatedState)
    RideConfirmed updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ ONGOING_RIDE updatedState)
    CancelRide updatedState cancelSearchType-> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CANCEL_RIDE_REQUEST updatedState cancelSearchType)
    UpdatedState screenState saveToCurrLocs -> do
       modifyScreenState $ HomeScreenStateType (\homeScreenState → screenState)
       App.BackT $ App.BackPoint <$> (pure $ RELOAD saveToCurrLocs)
    NotificationHandler notification updatedState ->  do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ FCM_NOTIFICATION notification updatedState)
    RefreshHomeScreen updatedState -> do
        modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
        App.BackT $ App.NoBack <$> (pure $ REFRESH_HOME_SCREEN)
    Retry updatedState -> do
        modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
        App.BackT $ App.NoBack <$> (pure $ RETRY)
    GoToHome state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → state)
      App.BackT $ App.NoBack <$> (pure $ HOME_SCREEN)
    SubmitRating updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ SUBMIT_RATING updatedState)
    OpenGoogleMaps updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ OPEN_GOOGLE_MAPS updatedState)
    InAppTrackStatus updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ IN_APP_TRACK_STATUS updatedState)
    UpdatedSource updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_FIND_ESTIMATES updatedState)
    UpdateSavedLocation screenState -> do
       modifyScreenState $ HomeScreenStateType (\homeScreenState → screenState)
       App.BackT $ App.BackPoint <$> (pure $ UPDATE_SAVED_LOCATION )
    CheckLocServiceability updatedState lat long -> do
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHECK_SERVICEABILITY updatedState lat long)
    GoToInvoice updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_TO_INVOICE_ updatedState)
    CheckFavDistance updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHECK_FOR_DUPLICATE_SAVED_LOCATION updatedState)
    SaveFavourite updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ SAVE_FAVOURITE updatedState)
    GoToReferral referralType updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_REFERRAL referralType)
    CallContact updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CALL_EMERGENCY_CONTACT updatedState)
    CallSupport updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CALL_SUPPORT updatedState)
    CallPolice updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_CALL_POLICE updatedState)
    UpdateSosStatus updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_SOS_STATUS updatedState)
    FetchContacts updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_FETCH_CONTACTS updatedState)
    CheckCurrentStatus -> App.BackT $ App.NoBack <$> (pure $ CHECK_CURRENT_STATUS)
    CheckFlowStatus updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure CHECK_FLOW_STATUS)
    RetryFindingQuotes showLoader updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ RETRY_FINDING_QUOTES showLoader)
    CallDriver updatedState callType exophoneNumber -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ON_CALL updatedState callType exophoneNumber)
    ExitToPermissionFlow flowType -> App.BackT $ App.NoBack <$> (pure $ TRIGGER_PERMISSION_FLOW flowType)
    ReportIssue updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REPORT_ISSUE updatedState)
    RideDetailsScreen updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      modifyScreenState $ TripDetailsScreenStateType (\_ -> getTripDetailsState updatedState.data.ratingViewState.rideBookingRes state.tripDetailsScreen)
      App.BackT $ App.BackPoint <$> (pure $ RIDE_DETAILS_SCREEN updatedState)
    RepeatTrip state trip-> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> state)
      App.BackT $ App.BackPoint <$> (pure $ REPEAT_RIDE_FLOW_HOME trip)
    ExitToTicketing updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ EXIT_TO_TICKETING updatedState)
    GoToHelpAndSupport updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HELP_AND_SUPPORT)
    ReAllocateRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ REALLOCATE_RIDE updatedState)
    GoToRentalsFlow updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RENTALS_FLOW updatedState)
    GoToScheduledRides -> App.BackT $ App.NoBack <$> (pure $ GO_TO_SCHEDULED_RIDES)
    Add_Stop updatedState -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ ADD_STOP updatedState)
    GoToNammaSafety updatedState triggerSos showTestDrill -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_NAMMASAFETY updatedState triggerSos showTestDrill)
    GoToSafetySettingScreen -> do
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_SAFETY_SETTING_SCREEN)
    SafetySupport updatedState isSafe -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SAFETY_SUPPORT updatedState isSafe)
    GoToShareRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_SHARE_RIDE updatedState)
    GoToNotifyRideShare updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_NOTIFY_RIDE_SHARE updatedState)
    ExitToFollowRide updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure EXIT_TO_FOLLOW_RIDE)
    GoToReportSafetyIssue updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_REPORT_SAFETY_ISSUE updatedState)
    GoToMyMetroTickets updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_MY_METRO_TICKETS)
    GoToSafetyEducation updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_SAFETY_EDUCATION)
    RepeatSearch updatedState -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ REPEAT_SEARCH updatedState)
    ChangeVehicleVarient updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CHANGE_VEHICLE_VARIANT updatedState)
    ExitToConfirmingLocationStage updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GOTO_CONFIRMING_LOCATION_STAGE updatedState)
    UpdateReferralCode updatedState code -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ UPDATE_REFERRAL_CODE code)
    GoToRideRelatedIssues updatedState -> do
      modifyScreenState $ HomeScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_RIDE_RELATED_ISSUES updatedState)
    Go_To_Search_Location_Flow updatedState isSource-> do 
      modifyScreenState $ HomeScreenStateType (\homeScreenState → updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_SEARCH_LOCATION_SCREEN updatedState isSource)
    RideSearchSO -> 
      App.BackT $ App.NoBack <$> (pure $ GO_TO_RIDE_SEARCH_FLOW)
    ConfirmRentalRideSO state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> state)
      App.BackT $ App.NoBack <$> (pure $ CONFIRM_RENTAL_RIDE)
    StayInHomeScreenSO state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreenState -> state)
      App.BackT $ App.NoBack <$> (pure $ STAY_IN_HOME_SCREEN)
