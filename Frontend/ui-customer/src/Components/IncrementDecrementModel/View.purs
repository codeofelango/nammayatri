{-
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.IncrementDecrementModel.View where

import Prelude

import Common.Types.App (LazyCheck(..))
import Components.IncrementDecrementModel.Controller (Action(..), Config)
import Effect (Effect)
import Font.Style as FontStyle
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, background, color, cornerRadius, gravity, height, linearLayout, margin, onClick, orientation, padding, rippleColor, stroke, text, textView, visibility, weight, width)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height WRAP_CONTENT
  , width config.width
  , orientation VERTICAL
  , gravity config.gravity
  , background config.background
  , cornerRadius config.cornerRadius
  , padding config.padding
  ]
  [ textView $
    [ text config.heading
    , color Color.black800
    , margin $ MarginTop 16
    , visibility $ boolToVisibility $ not isHorizontal
    ] <> FontStyle.body1 TypoGraphy
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation config.orientation
    , gravity config.gravity
    ]
    [ textView $
      [ background config.buttonTextConfig.background
      , text $ if isHorizontal then "-" else "+"
      , color config.buttonTextConfig.color
      , cornerRadius config.buttonTextConfig.cornerRadius
      , padding config.buttonTextConfig.padding
      , background config.buttonTextConfig.background
      , gravity config.buttonTextConfig.gravity
      , onClick push $ const (if isHorizontal then OnDecrement else OnIncrement)
      , margin config.buttonTextConfig.margin
      ] <> config.buttonTextConfig.fontStyle
    , textView $
        [ background config.countTextConfig.background
        , text $ show config.initialValue <> " km"
        , color config.countTextConfig.color
        , padding config.countTextConfig.padding
        , margin config.countTextConfig.margin
        , cornerRadius config.countTextConfig.cornerRadius
        , gravity $ CENTER
        , weight 1.0
        , visibility $ boolToVisibility $ isHorizontal
        ] <> config.countTextConfig.fontStyle
    , linearLayout
      [ height WRAP_CONTENT
      , width config.lineWidth
      , orientation HORIZONTAL
      , gravity config.gravity
      , visibility $ boolToVisibility $ not isHorizontal
      ] 
      [ linearLayout
        [ height WRAP_CONTENT
      --   -- , width $ V $ (config.width / 3) - (if config.initialValue > 100 then 18 else if config.initialValue > 10 then 10 else 4)
        , weight 1.0
        ] []
      , textView $
        [ background Color.white900
        , text $ show config.initialValue
        , color config.countTextConfig.color
        , padding config.countTextConfig.padding
        , margin config.countTextConfig.margin
        , cornerRadius config.countTextConfig.cornerRadius
        , stroke config.countTextConfig.stroke
        ] <> config.countTextConfig.fontStyle
      , textView $
        [ text config.countSuffixText
        , color config.countTextConfig.color
        , margin $ MarginLeft 4
        ] <> FontStyle.h2 TypoGraphy
      ]
    , textView $
      [ background config.buttonTextConfig.background
      , text $ if isHorizontal then "+" else "-"
      , color config.buttonTextConfig.color
      , cornerRadius config.buttonTextConfig.cornerRadius
      , padding config.buttonTextConfig.padding
      , cornerRadius config.buttonTextConfig.cornerRadius
      , gravity config.buttonTextConfig.gravity
      , margin config.buttonTextConfig.margin
      , onClick push $ const (if isHorizontal then OnIncrement else OnDecrement)
      ] <> config.buttonTextConfig.fontStyle
    ]
  ]
  where
    isHorizontal :: Boolean
    isHorizontal = config.heading == ""