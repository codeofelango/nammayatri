{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetySettingsScreen.Controller where

import Log
import Prelude
import PrestoDOM
import Screens.Types
import Storage
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Foreign (unsafeToForeign)
import JBridge
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Services.Config (getSupportNumber)
import Types.App (defaultGlobalState)
import Types.EndPoint (updateSosVideo)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
    StartNammaSafetyOnboarding act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "start_onboarding" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    GoToNextStep act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "next_step_onboard" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | PostEmergencySettings NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToEducationScreen NammaSafetyScreenState
  | GoToSetupScreen NammaSafetyScreenState
  | GoToActivateSosScreen NammaSafetyScreenState
  | PostContacts NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | StartNammaSafetyOnboarding PrimaryButtonController.Action
  | GoToNextStep PrimaryButtonController.Action
  | EditEmergencyContacts
  | SwitchToStage SafetySetupStage
  | ToggleSwitch SafetySetupStage
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | ChangeFollowing Int
  | GoToEducationView
  | StartTestDrill PrimaryButtonController.Action
  | ContactAction ContactCircle.Action
  | DialPolice

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval AddContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (UpdateEmergencySettings (GetEmergencySettingsRes response)) state = do
  let
    contacts =
      map
        ( \(ContactDetails item) ->
            { number: item.mobileNumber
            , name: item.name
            , isSelected: true
            , enableForFollowing: response.shareTripWithEmergencyContacts && fromMaybe false item.enableForFollowing
            , priority: fromMaybe 1 item.priority
            }
        )
        response.defaultEmergencyNumbers
  void $ pure $ setCleverTapUserProp [ { key: "Safety Setup Completed", value: unsafeToForeign response.hasCompletedSafetySetup } ]
  void $ pure $ setCleverTapUserProp [ { key: "Auto Share Night Ride", value: unsafeToForeign response.shareTripWithEmergencyContacts } ]
  void $ pure $ setCleverTapUserProp [ { key: "Mock Safety Drill Completed", value: unsafeToForeign response.hasCompletedMockSafetyDrill } ]
  void $ pure $ setCleverTapUserProp [ { key: "Night Safety Check Enabled", value: unsafeToForeign response.nightSafetyChecks } ]
  continue
    state
      { data
        { hasCompletedSafetySetup = response.hasCompletedSafetySetup
        , shareToEmergencyContacts = response.shareEmergencyContacts
        , nightSafetyChecks = response.nightSafetyChecks
        , hasCompletedMockSafetyDrill = response.hasCompletedMockSafetyDrill
        , shareTripWithEmergencyContacts = response.shareTripWithEmergencyContacts
        , emergencyContactsList = getDefaultPriorityList contacts
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = exit $ GoToEducationScreen state

eval (ToggleSwitch stage) state = case stage of
  SetNightTimeSafetyAlert -> exit $ PostEmergencySettings state { data { nightSafetyChecks = not state.data.nightSafetyChecks } }
  SetShareTripWithContacts -> exit $ PostEmergencySettings state { data { shareTripWithEmergencyContacts = not state.data.shareTripWithEmergencyContacts } }
  SetDefaultEmergencyContacts ->
    if DA.length state.data.emergencyContactsList /= 0 then
      exit $ PostEmergencySettings state { data { shareToEmergencyContacts = not state.data.shareToEmergencyContacts } }
    else
      continueWithCmd state [ pure AddContacts ]
  _ -> continue state

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = exit $ GoToSetupScreen state

eval EditEmergencyContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (SwitchToStage stage) state = continue state { props { setupStage = stage } }

eval (BackPressed) state = exit $ GoBack state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (ChangeFollowing contactIndex) state = do
  let
    newContacts =
      DA.mapWithIndex
        ( \index item -> case index == contactIndex of
            true -> item { enableForFollowing = not item.enableForFollowing }
            false -> item
        )
        state.data.emergencyContactsList

    newState = state { data { emergencyContactsList = newContacts } }
  updateAndExit newState $ PostContacts newState

eval GoToEducationView state = exit $ GoToEducationScreen state

eval (StartTestDrill PrimaryButtonController.OnClick) state =
  exit
    $ GoToActivateSosScreen
        state
          { props
            { confirmTestDrill = true
            , triggeringSos = false
            , showTestDrill = false
            }
          }

eval DialPolice state = do
   void $ pure $ HU.performHapticFeedback unit
   pure $ showDialer "112" false
   continue state

eval _ state = continue state