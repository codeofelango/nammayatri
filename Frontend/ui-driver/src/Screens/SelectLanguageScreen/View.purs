{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectLanguageScreen.View where

import Screens.SelectLanguageScreen.ComponentConfig
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import Components.SelectMenuButton as MenuButton
import Data.Array as DA
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, ($), (<<<), (==))
import PrestoDOM (visibility, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, afterRender, alpha, background, color, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, rippleColor, cornerRadius)
import Screens.SelectLanguageScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Debug(spy)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import PrestoDOM.Animation as PrestoAnim
import Debug
import Engineering.Helpers.Commons as EHC
import Mobility.Prelude(boolToVisibility)

screen :: ST.SelectLanguageScreenState -> Screen Action ST.SelectLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "SelectLanguageScreen"
  , globalEvents : []
  , eval:
      \action state -> do
        let _ = spy "SelectLanguageScreen action " action
        let _ = spy "SelectLanguageScreen state " state
        eval action state
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.SelectLanguageScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.fadeIn true] $
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , onBackPressed push (const BackPressed)
      , background Color.white900
      , afterRender push (const AfterRender)
      , padding $ PaddingBottom EHC.safeMarginBottom
      ][  headerLayout push state
        , menuButtonsView state push
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]

-------------------------------------------------- headerLayout --------------------------
headerLayout :: (Action -> Effect Unit) -> ST.SelectLanguageScreenState ->  forall w . PrestoDOM (Effect Unit) w
headerLayout push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 10 (EHC.safeMarginTopWithDefault 13) 10 13
        ]
        [ imageView
            [ width $ V 30
            , height $ V 30
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
            , onClick push $ const BackPressed
            , rippleColor Color.rippleShade
            , cornerRadius 15.0
            ]
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString SELECT_LANGUAGE)
              , margin $ MarginLeft 10
              , padding $ PaddingBottom 2
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.greyLight
        ]
        []
    ]

------------------------------ menuButtonsView ------------------------------
menuButtonsView :: ST.SelectLanguageScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
menuButtonsView state push = 
 scrollView
  [ width MATCH_PARENT
  , weight 1.0
  , padding $ if state.props.onlyGetTheSelectedLanguage then PaddingHorizontal 16 16 else Padding 0 0 0 0
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 1 0 1 0
      , background Color.white900
      ][  textView $ 
          [ visibility $ boolToVisibility state.props.onlyGetTheSelectedLanguage
          , text $ getString SELECT_THE_LANGUAGE_YOU_CAN_READ
          , color $ Color.black800
          , margin $ MarginTop 16
          ] <> FontStyle.subHeading1 TypoGraphy
       ,  linearLayout
          [   height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ Margin 1 16 1 0
            , background Color.white900
          ](DA.mapWithIndex
              (\ index language ->
              MenuButton.view (push <<< MenuButtonAction) (menuButtonConfig state language index)) $ if state.props.onlyGetTheSelectedLanguage then state.data.languageList else (state.data.config.languageList)
          )
      ]
  ]