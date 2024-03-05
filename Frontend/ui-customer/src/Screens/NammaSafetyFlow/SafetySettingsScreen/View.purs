{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetySettingsScreen.View where

import Animation
import Prelude
import PrestoDOM
import Screens.NammaSafetyFlow.ComponentConfig
import Screens.NammaSafetyFlow.Components.ContactsList
import Screens.NammaSafetyFlow.Components.HelperViews
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, null, (!!), (..))
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Mobility.Prelude (boolToVisibility, spaceSeparatedPascalCase)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM.Animation as PrestoAnim
import Screens.EmergencyContactsScreen.View (getFirstChar, getLastChar)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.SafetySettingsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..), NewContacts, RecordingState(..), StepsHeaderModelState)
import Screens.Types as ST
import Services.API (GetSosDetailsRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import MerchantConfig.Utils (Merchant(..), getMerchant)

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SafetySettingsScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
              $ do
                  response <- Remote.getEmergencySettingsBT ""
                  lift $ lift $ doAff do liftEffect $ push $ UpdateEmergencySettings response
                  lift $ lift $ doAff do liftEffect $ push $ DisableShimmer
                  lift $ lift $ EHU.toggleLoader false
                  pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SafetySettingsScreen action " action
        let
          _ = spy "SafetySettingsScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background if state.props.isOffUs then Color.black900 else Color.white900
        , padding padding'
        , onBackPressed push $ const BackPressed
        ]
        [ Header.view (push <<< SafetyHeaderAction) headerConfig
        , dashboardView state push
        , shimmerView state
        ]
  where
  padding' =
    if EHC.os == "IOS" then
      (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom))
    else
      (PaddingLeft 0)

  headerConfig = if state.props.isOffUs then 
    (Header.config Language) { showLearnMore = state.data.hasCompletedSafetySetup, showCrossImage = true, useLightColor = true }
    else
      (Header.config Language) { showLearnMore = state.data.hasCompletedSafetySetup }

------------------------------------- dashboardView -----------------------------------
dashboardView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dashboardView state push =
  let isOffUs = state.props.isOffUs
  in
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background if isOffUs then Color.black900 else Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not state.props.showShimmer
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ] if isOffUs then 
          [ nammaSafetyViewOffUs state push
          , userSettingsView state push $ not showFeatures
          ]
          else
          [ nammaSafetyFeaturesView state push showFeatures
          , userSettingsView state push $ not showFeatures
          ]
    ]
  where
  showFeatures = (not state.data.hasCompletedSafetySetup) && (not state.props.onRide)

nammaSafetyViewOffUs :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyViewOffUs state push =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        [ featuresViewOffUs state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.black900
            , alignParentBottom "true,-1"
            ]
            [ callPoliceView state push CALL_POLICE
        ]
        ]


callPoliceView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> STR -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push text' =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , margin $ Margin 12 12 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      ]
        <> [ rippleColor Color.rippleShade, onClick push $ const $ DialPolice ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
        , height $ V 26
        , width $ V 26
        ]
    , textView
        $ [ text $ getString text'
          , gravity CENTER
          , color Color.white900
          , margin $ MarginLeft 6
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

featuresViewOffUs :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
featuresViewOffUs state push =
  let merchant = spaceSeparatedPascalCase $ show (getMerchant FunctionCall) -- Need to check other merchants and update logic
  in
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , layoutGravity "center_vertical"
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.black900
        , gravity CENTER
        , orientation VERTICAL
        , layoutGravity "center_vertical"
        , margin $ Margin 16 20 16 6
        ]
        [ flexBoxLayout
          [
            height MATCH_PARENT
          , width WRAP_CONTENT
          , justifyContent JUSTIFY_START
          , flexDirection COLUMN
          , flexWrap WRAP
          , alignItems ALIGN_STRETCH
          , orientation HORIZONTAL
          , padding (Padding 10 6 10 6)
          ][
            textView
            $ [ text $ getString (ADDITIONAL_FEATURES_ON merchant)
              , singleLine false
              , color Color.white900
              ]
            <> FontStyle.subHeading1 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingHorizontal 16 16
            , margin $ MarginTop 12
            , background Color.black800
            , visibility GONE
            ]
            [ imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ic_namma_yatri_logo"
                , height $ V 20
                , width $ V 20
                , margin $ MarginRight 8
                ]
            , textView
                $ [ text "Guaranteed Ride"
                  , color Color.white900
                  , weight 1.0
                  , height WRAP_CONTENT
                  , singleLine false
                  ]
                <> FontStyle.tags TypoGraphy
            ]
          ]
        , linearLayout 
          [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , cornerRadius 12.0
          , stroke $ "1," <> Color.black700
          , background Color.blackOpacity12
          ]
          [ 
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , cornerRadius 12.0
            ]
            [ imageWithTextView (getString NOTIFY_YOUR_EC) true state
            , imageWithTextView (getString EC_CAN_RESPOND) true state
            , imageWithTextView (getString $ QUICK_SUPPORT merchant) true state
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background Color.black700
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ GoToEducationView
            ]
            [ textView
                $ [ textFromHtml $ "<u>" <> (getString $ LEARN_ABOUT_APP_SAFETY_FEAT merchant) <> "</u>"
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
          ]
        ]
    ]
-- ---------------------------------- nammaSafetyFeaturesView -----------------------------------
nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility visibility'
        ]
        [ featuresView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.white900
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
        ]

featuresView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
featuresView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , gravity CENTER
        , orientation VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 20 16 6
        , stroke $ "1," <> Color.blue600
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_shield"
            , width $ V 220
            , height $ V 114
            ]
        , textView
            $ [ text $ getString NAMMA_SAFETY_WILL_ENABLE_ACCESS
              , margin $ Margin 16 20 16 4
              , color Color.black800
              , width MATCH_PARENT
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ imageWithTextView (getString AUTOMATIC_CALL_PLACED_TO_EMERGENCY_CONTACTS) true state
            , imageWithTextView (getString $ EMERGENCY_CONTACTS_CAN_FOLLOW "EMERGENCY_CONTACTS_CAN_FOLLOW") true state
            , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_POLICE) true state
            , imageWithTextView (getString $ ALERT_SAFETY_TEAM "ALERT_SAFETY_TEAM") true state
            , imageWithTextView (getString OPTION_TO_REPORT_A_SAFETY_ISSUE) true state
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background if state.props.onRide then Color.black700 else Color.white900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ GoToEducationView
            ]
            [ textView
                $ [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]

imageWithTextView :: String -> Boolean -> NammaSafetyScreenState -> forall w. PrestoDOM (Effect Unit) w
imageWithTextView text' isActive state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingHorizontal 16 16
    , margin $ MarginTop 12
    , background if state.props.isOffUs then Color.blackOpacity12 else Color.blue600
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET if isActive then "ny_ic_check" else "ny_ic_ellipse_outline_grey"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        ]
    , textView
        $ [ text text'
          , color if state.props.isOffUs then Color.white900 else Color.black800
          , weight 1.0
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> FontStyle.tags TypoGraphy
    ]


userSettingsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
userSettingsView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility $ boolToVisibility visibility'
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , gravity LEFT
                , padding $ Padding 16 16 16 16
                ]
                [ textView
                    $ [ text $ getString EMERGENCY_ACTIONS
                      , color Color.black900
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , textView
                    $ [ text $ getString WHEN_YOU_START_EMERGENCY_SOS
                      , color Color.black700
                      ]
                    <> FontStyle.body3 TypoGraphy
                ]
            , toggleSwitchViewLayout (ToggleSwitch SetDefaultEmergencyContacts) state.data.shareToEmergencyContacts (getString EMERGENCY_SHARING_WITH_CONTACTS) push true 16
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , padding $ Padding 16 8 16 0
                ]
                [ textView
                    $ [ text $ getString SHARING_WITH
                      , color Color.black700
                      , margin $ MarginRight 8
                      , gravity CENTER
                      , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
                      ]
                    <> FontStyle.body3 TypoGraphy
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    ]
                    (mapWithIndex (\index item -> ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactAction)) state.data.emergencyContactsList)
                , textView
                    $ [ text $ getString if null state.data.emergencyContactsList then ADD_CONTACTS else EDIT
                      , color Color.blue900
                      , margin $ MarginLeft 8
                      , gravity CENTER
                      , onClick push $ const $ EditEmergencyContacts
                      ]
                    <> FontStyle.body2 TypoGraphy
                ]
            , separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , toggleSwitchViewLayout (ToggleSwitch SetNightTimeSafetyAlert) state.data.nightSafetyChecks (getString NIGHT_TIME_SAFETY_CHECKS) push true 16
            , separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , toggleSwitchViewLayout (ToggleSwitch SetShareTripWithContacts) state.data.shareTripWithEmergencyContacts (getString RIDE_SHARE_AFTER_SIX_PM) push true 16
            , textView
                $ [ text $ getString $ WHO_CAN_TRACK_YOUR_RIDE "WHO_CAN_TRACK_YOUR_RIDE"
                  , color Color.black700
                  , margin $ Margin 16 16 16 16
                  , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
                  ]
                <> FontStyle.body1 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                ( mapWithIndex
                    ( \index item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , margin $ Margin 16 16 0 0
                          , gravity CENTER_VERTICAL
                          ]
                          [ ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactAction)
                          , toggleSwitchViewLayout (ChangeFollowing index) item.enableForFollowing item.name push true 12
                          ]
                    )
                    state.data.emergencyContactsList
                )
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , visibility $ boolToVisibility $ getValueToLocalStore IS_SOS_ACTIVE /= "true" && state.data.shareToEmergencyContacts
            ]
            [ PrimaryButton.view (push <<< StartTestDrill) (goToDrillButtonConfig state) ]
        ]

toggleSwitchViewLayout :: Action -> Boolean -> String -> (Action -> Effect Unit) -> Boolean -> Int -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchViewLayout action isActive text' push visibility' marginLeft =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginHorizontal marginLeft 16
    ]
    [ textView
        $ [ text text'
          , weight 1.0
          , color Color.black800
          ]
        <> FontStyle.body2 TypoGraphy
    , toggleSwitchView isActive true action push
    ]


toggleSwitchView :: Boolean -> Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive visibility' action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action --ToggleSwitch stage
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]

shimmerView :: forall w. ST.NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , visibility $ boolToVisibility state.props.showShimmer
    ]
    [ sfl (V 400) 16 1 true
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , alignParentBottom "true,-1"
        ]
        [ sfl (V 80) 130 3 (getValueToLocalStore IS_SOS_ACTIVE == "true")
        , sfl (V 80) 130 1 (getValueToLocalStore IS_SOS_ACTIVE /= "true")
        ]
    ]

sfl :: forall w. Length -> Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
sfl height' marginTop numberOfBoxes visibility' =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop marginTop
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        ( map
            ( \item ->
                linearLayout
                  [ height height'
                  , background Color.greyDark
                  , cornerRadius 12.0
                  , weight 1.0
                  , stroke $ "1," <> Color.grey900
                  , margin $ Margin 4 4 4 4
                  ]
                  []
            )
            (1 .. numberOfBoxes)
        )
    ]