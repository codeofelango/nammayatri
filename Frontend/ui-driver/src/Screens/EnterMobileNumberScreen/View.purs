{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.View where

import Data.Maybe (Maybe(..))
import Prelude (Unit, const, ($), (<<<), (<>), bind, pure , unit, void, (==), (-), (+), discard)
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alpha, background, clickable, color, cornerRadius, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textView, visibility, weight, width, afterRender, imageWithFallback, singleLine, textFromHtml, adjustViewWithKeyboard, scrollView, disableKeyboardAvoidance, scrollBarY, id)
import Components.PrimaryButton as PrimaryButton
import Components.MobileNumberEditor as MobileNumberEditor
import Components.PrimaryEditText.View as PrimaryEditText
import Effect (Effect)
import Screens.EnterMobileNumberScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App
import Screens.EnterMobileNumberScreen.ComponentConfig
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Debug(spy)
import ConfigProvider
import Services.API (OAuthProvider(..))
import Effect.Uncurried (runEffectFn2)
import Mobility.Prelude

screen :: ST.EnterMobileNumberScreenState -> Screen Action ST.EnterMobileNumberScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterMobileNumberScreen"
  , globalEvents : [(\push -> do 
                              void $ runEffectFn2 JB.storeKeyBoardCallback push KeyboardCallback
                              pure $ pure unit)]
  , eval: (\action state -> do
      let _ = spy "EnterMobileNUmber state -----" state
      let _ = spy "EnterMobileNUmber--------action" action
      eval action state)
  }

view :: forall w . (Action -> Effect Unit) -> ST.EnterMobileNumberScreenState -> PrestoDOM (Effect Unit) w
view push state =
   linearLayout
   [  height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
   ][  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push (const AfterRender)
    , margin $ MarginBottom 24
    , padding $ PaddingBottom EHC.safeMarginBottom
    , background Color.white900
    , onBackPressed push (const BackPressed)
    ][  headerView state push
      , frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , padding (Padding 16 0 16 0)
        ][if state.data.config.enterMobileNumberScreen.emailAuth then oAuthProvidersView state push else enterMobileNumberView  state push]
      ]
    ]
    where 
      headerView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
      headerView state push = 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background state.data.config.enterMobileNumberScreen.headerBackground
        , padding $ Padding 16 (EHC.safeMarginTop + 16) 16 16
        ][  imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , height $ V 25 
            , width $ V 25
            , onClick push $ const BackPressed
            ]
          , textView $ 
            [ text $ getString LETS_GET_YOU_TRIP_READY
            , color Color.white900
            , margin $ MarginVertical 5 22
            , height WRAP_CONTENT
            , width MATCH_PARENT
            ] <> FontStyle.h1 TypoGraphy

        ]


--------------------- backArrow ----------------------------
backArrow :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backArrow state push =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 16 16 0)
  ][ imageView
      [ width ( V 25 )
      , height ( V 25 )
      , margin (MarginTop 20)
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
      , onClick push (const BackPressed)
      ]
  ]

------------------------- enterMobileNumberTextView -------------------
enterMobileNumberTextView :: ST.EnterMobileNumberScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterMobileNumberTextView state =
 textView (
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , text (getString ENTER_YOUR_MOBILE_NUMBER)
  , color Color.textPrimary
  , margin (MarginVertical 37 8)
  ] <> FontStyle.body3 TypoGraphy
  )

--------------------------------- underlinedTextView ----------------------
underlinedTextView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
underlinedTextView _ _ =
  let config = getAppConfig appConfig
  in
  linearLayout
  [ height WRAP_CONTENT
  , orientation HORIZONTAL
  , weight 1.0
  ][ textView $
    [ weight 1.0
    , height WRAP_CONTENT
    , textFromHtml $ getString BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC
    , color Color.black700
    , onClick (\_ -> JB.openUrlInApp $ config.termsLink) (const NonDisclosureAgreementAction)
    , singleLine false
    ] <> FontStyle.body3 TypoGraphy
 ]

-------------------------------- termsAndConditionsView ------------------
termsAndConditionsView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionsView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin (Margin 15 10 16 20)
  ][ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin (MarginLeft 10)
      ][textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR)
        , color Color.greyTextColor
        , alpha 0.5
        ] <> FontStyle.body3 TypoGraphy)
      , underlinedTextView state push
      ]
  ]

enterMobileNumberView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
enterMobileNumberView  state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility VISIBLE
    , alpha 1.0
    , orientation VERTICAL
    , margin $ MarginTop 37
    ][   PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
      ] $ MobileNumberEditor.view (push <<< PrimaryEditTextAction) (mobileNumberConfig state)  

    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      ][]
    , PrestoAnim.animationSet
      ( if EHC.os == "IOS" then [] else [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig -- 400 15 0 0 true PrestoAnim.Linear -- Temporary fix for iOS
      ]) $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 0 0 0 10)
        ][ underlinedTextView state push
          ]
     , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][PrimaryButton.view (push <<< PrimaryButtonActionController) (mobileNumberButtonConfig state)]
    ]


oAuthProvidersView :: ST.EnterMobileNumberScreenState -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
oAuthProvidersView state push =
  let isFunctionExists = EHC.jBridgeMethodExists "oAuthSignIn"
  in
  scrollView
  [ width MATCH_PARENT
  , height $ if EHC.os == "IOS" then V $ (EHC.screenHeight unit) - 150 - EHC.safeMarginBottom else WRAP_CONTENT
  , disableKeyboardAvoidance true
  , id $ EHC.getNewIDWithTag "OAuthScrollView"
  , scrollBarY false
  ][
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 37
    , adjustViewWithKeyboard "true"
    ][ textView $
      [ text $ if isFunctionExists then "To signup or login, choose an option" else "Enter a email to signup or login"
      , color Color.black800
      ] <> FontStyle.subHeading2 TypoGraphy
    , PrimaryButton.view (push <<< (OAuthPB Google)) (googleProvider state isFunctionExists)
    , if EHC.os == "IOS" then PrimaryButton.view (push <<< (OAuthPB IOS)) (appleProvider state isFunctionExists) else linearLayout[][]
    , textView $
        [ text "Or"
        , gravity CENTER
        , width MATCH_PARENT
        , margin $ MarginTop 24
        , color Color.black800
        , visibility $ boolToVisibility isFunctionExists
        ] <> FontStyle.body1 TypoGraphy
    ,  PrimaryEditText.view (push <<< EmailPBAC) $ emailIdPrimaryEditTextConfig state
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , adjustViewWithKeyboard "true"
      ][ PrimaryButton.view (push <<< PrimaryButtonEmailAC) (primaryButtonEmailConfig state)
      ]
    ]
  ]