{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ModifyScreenState where

import Accessor (_lat, _lon)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (modifyState, getState)
import Prelude (Unit, bind, ($))
import Resources.Constants (encodeAddress, getAddressFromBooking)
import Screens.HomeScreen.ScreenData (initData) as HomeScreen
import Screens.Types (MyRidesScreenState, Stage(..), Trip(..), LocationSelectType(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))

modifyScreenState :: ScreenType -> FlowBT String Unit
modifyScreenState st =
  case st of
    TicketingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state{ ticketingScreen = a state.ticketingScreen}) 
    ChooseLanguageScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { chooseLanguageScreen = a state.chooseLanguageScreen})
    EnterMobileNumberScreenType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { enterMobileNumberScreen = a state.enterMobileNumberScreen})
    AccountSetUpScreenStateType a -> modifyState (\(GlobalState  state) -> GlobalState  $ state { accountSetUpScreen = a state.accountSetUpScreen})
    HomeScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {homeScreen = a state.homeScreen})
    TripDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {tripDetailsScreen = a state.tripDetailsScreen})
    MyRideScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {myRidesScreen = a state.myRidesScreen})
    HelpAndSupportScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { helpAndSupportScreen = a state.helpAndSupportScreen})
    InvoiceScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {invoiceScreen = a state.invoiceScreen})
    SelectLanguageScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {selectLanguageScreen = a state.selectLanguageScreen})
    AddNewAddressScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {addNewAddressScreen = a state.addNewAddressScreen})
    MyProfileScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {myProfileScreen = a state.myProfileScreen})
    ContactUsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {contactUsScreen = a state.contactUsScreen})
    SavedLocationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {savedLocationScreen = a state.savedLocationScreen})
    ReferralScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {referralScreen = a state.referralScreen})
    EmergencyContactsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {emergencyContactsScreen = a state.emergencyContactsScreen})
    TicketBookingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {ticketBookingScreen = a state.ticketBookingScreen})
    PermissionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {permissionScreen = a state.permissionScreen})
    AboutUsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {aboutUsScreen = a state.aboutUsScreen})
    AppUpdatePopUpScreenType a->  modifyState (\(GlobalState state) -> GlobalState $ state { appUpdatePopUpScreen = a state.appUpdatePopUpScreen })
    TicketInfoScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {ticketInfoScreen = a state.ticketInfoScreen})
    FollowRideScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {followRideScreen = a state.followRideScreen})
    AppConfigType a->  modifyState (\(GlobalState state) -> GlobalState $ state { appConfig = a state.appConfig })
    RideScheduledScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideScheduledScreen = a state.rideScheduledScreen})
    SearchLocationScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {searchLocationScreen = a state.searchLocationScreen})
    GlobalPropsType a -> modifyState (\(GlobalState state) -> GlobalState $ state {globalProps = a state.globalProps})
    NammaSafetyScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {nammaSafetyScreen = a state.nammaSafetyScreen})
    RentalScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state { rentalScreen = a state.rentalScreen })
    RideSelectionScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {rideSelectionScreen = a state.rideSelectionScreen})
    ReportIssueChatScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {reportIssueChatScreen = a state.reportIssueChatScreen})
    MetroTicketDetailsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketDetailsScreen = a state.metroTicketDetailsScreen})
    MetroMyTicketsScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroMyTicketsScreen = a state.metroMyTicketsScreen})
    MetroTicketBookingScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketBookingScreen = a state.metroTicketBookingScreen})
    MetroTicketStatusScreenStateType a -> modifyState (\(GlobalState state) -> GlobalState $ state {metroTicketStatusScreen = a state.metroTicketStatusScreen})
    GlobalFlowCacheType a -> modifyState (\(GlobalState state) -> GlobalState $ state {globalFlowCache = a state.globalFlowCache})

    
updateRepeatRideDetails :: Trip -> FlowBT String Unit
updateRepeatRideDetails state = do 
  (GlobalState globalState) <- getState
  modifyScreenState $ HomeScreenStateType 
    (\homeScreen -> HomeScreen.initData{ 
    data {
      source =  state.source
    , destination = state.destination
    , locationList = homeScreen.data.locationList
    , sourceAddress = state.sourceAddress
    , savedLocations = homeScreen.data.savedLocations
    , destinationAddress = state.destinationAddress 
    , settingSideBar {
        gender = globalState.homeScreen.data.settingSideBar.gender 
      , email = globalState.homeScreen.data.settingSideBar.email
      , hasCompletedSafetySetup = globalState.homeScreen.data.settingSideBar.hasCompletedSafetySetup
    }
    }
    , props{
      rideSearchProps { sourceSelectType = REPEAT_RIDE }
    , sourceLat = state.sourceLat
    , sourceLong = state.sourceLong
    , destinationLat = state.destLat
    , destinationLong = state.destLong
    , currentStage = FindingEstimate
    , rideRequestFlow = true
    , isSpecialZone = state.isSpecialZone
    , isBanner = globalState.homeScreen.props.isBanner
    , sosBannerType = globalState.homeScreen.props.sosBannerType 
    , followsRide = globalState.homeScreen.props.followsRide
    , suggestedRideFlow = globalState.homeScreen.props.suggestedRideFlow
    }
    })


data FlowState = HelpAndSupportScreenFlow 
               | IssueReportChatScreenFlow
               | RideSelectionScreenFlow
               | HomeScreenFlow
               | ActivateSafetyScreenFlow
               | TripDetailsScreenFlow
               | ContactUsScreenFlow
               | MyRidesScreenFlow