{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RateCard.View where

import Common.Types.App (LazyCheck(..), RateCardType(..))
import Components.RateCard.Controller (Action(..), Config)
import Data.String as DS
import Data.Int as DI
import Data.Maybe as DM
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, (<>), (>),(==), (||), (&&), (/), (*), (/=), (+), (<<<), unit, map, (-))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, imageUrl, fontStyle, gravity, height, imageView, textFromHtml,imageWithFallback, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, lineHeight,fontStyle, scrollView, maxLines, singleLine, stroke, horizontalScrollView, relativeLayout)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Components.PrimaryButton as PrimaryButton
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (os, screenWidth, screenHeight)
import Mobility.Prelude (boolToVisibility)
import Debug (spy)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 20 20
  , gravity CENTER
  , background Color.black9000
  , onClick push $ const BackPressed
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , background Color.white900
     , cornerRadius 16.0
     , onClick push $ const NoAction
     ][linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background if config.isNightShift then Color.black900 else Color.blue600
        , orientation HORIZONTAL
        , cornerRadii $ Corners 16.0 true true false false
        ][ 
          linearLayout
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , orientation VERTICAL
           , padding $ Padding 16 22 16 0
           ][ commonTV push config.title (if config.isNightShift then Color.white900 else Color.black800) FontStyle.h1 LEFT 0 NoAction
            , commonTV push config.description (if config.isNightShift then Color.black500 else Color.black700) FontStyle.tags LEFT 3 NoAction
            ]
         , imageView
           [ width MATCH_PARENT
           , height $ V 90
           , imageWithFallback $ fetchImage FF_COMMON_ASSET $ 
              case config.currentRateCardType of
              PaymentFareBreakup -> ""
              _ -> if config.isNightShift then "ny_ic_night" else "ny_ic_day"
           ]  
         ]
      ,linearLayout
        [ width MATCH_PARENT
        , height $ if config.currentRateCardType == PaymentFareBreakup then WRAP_CONTENT 
                   else if config.currentRateCardType == RentalRateCard then V 400
                   else if config.showDetails then V 350 else V 250 -- check in IOS (Added to handle glitch)
        , orientation HORIZONTAL
        ][PrestoAnim.animationSet [ if (DA.any (_ == config.currentRateCardType) [ PaymentFareBreakup, DefaultRateCard]) then (translateInXBackwardAnim config.onFirstPage) else (translateInXForwardAnim true) ] $
          case config.currentRateCardType of 
            DefaultRateCard -> defaultRateCardView push config 
            DriverAddition -> driverAdditionView push config 
            PaymentFareBreakup -> paymentfareBreakup push config
            TollOrParkingCharges -> tollOrParkingView push config
            RentalRateCard -> rentalRateCardView push config
            _ -> defaultRateCardView push config 
        ]
      ,linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding $ PaddingBottom $ if config.currentRateCardType /= RentalRateCard then 20 else 0
      ][ case config.buttonText of
          Just text' -> textView $ 
                       [ text text'
                       , height WRAP_CONTENT
                       , width MATCH_PARENT
                       , padding $ PaddingVertical 12 16
                       , onClick push $ const $ if DA.any (_ == config.currentRateCardType) [DefaultRateCard, RentalRateCard] then Close else GoToDefaultStart
                       , color Color.blue800
                       , gravity CENTER
                       ] <> FontStyle.subHeading1 TypoGraphy
          Nothing -> linearLayout[][]
      ]
    ]      
  ]

fareList :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
fareList push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    ](DA.mapWithIndex (\index item -> 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ MarginTop 10
        ][ textView $
            [ height WRAP_CONTENT
            , singleLine false
            , maxLines 2
            , color Color.black700
            , text item.key
            , weight 1.0
            ] <> FontStyle.body2 LanguageStyle
          , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.black800
            , text item.val
            , singleLine false
            , maxLines 2
            ] <> FontStyle.body2 LanguageStyle
          ]
      ) config.fareList)

defaultRateCardView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
defaultRateCardView push config = 
  scrollView
  [ width MATCH_PARENT
  , height if os == "IOS" then (V 350) else WRAP_CONTENT
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ] $ [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , padding $ PaddingHorizontal 20 20
          ][ fareList push config]
        , imageView
          [ width MATCH_PARENT
          , height $ V 2 
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_horizontal_dash"
          , margin $ Margin 20 20 20 12
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , padding $ PaddingHorizontal 20 20
          ](map (\item -> 
              textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , color Color.black700
              , margin $ MarginTop 8
              , text item
              ] <> FontStyle.body3 LanguageStyle
            ) config.fareInfoDescription)
        , textView $
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , color Color.black700
          , text (getStringByKey config "CHARGE_DESCRIPTION")
          , visibility if (getStringByKey config "CHARGE_DESCRIPTION" /= "") then VISIBLE else GONE
          , padding $ PaddingHorizontal 20 20
          , margin $ MarginTop 8
          ] <> FontStyle.paragraphText TypoGraphy
        , imageView
          [ width MATCH_PARENT
          , height $ V 2 
          , visibility if config.showDetails then VISIBLE else GONE
          , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_horizontal_dash"
          , margin $ Margin 20 12 20 0
          ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , visibility if config.showDetails then VISIBLE else GONE
            ](DA.mapWithIndex (\index item -> 
                linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                ][  linearLayout
                    [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation HORIZONTAL
                      , padding $ Padding 20 12 20 12
                      , onClick push $ const case item.key of
                        "DRIVER_ADDITIONS" -> GoToDriverAddition
                        "FARE_UPDATE_POLICY" -> GoToFareUpdate
                        "TOLL_OR_PARKING_CHARGES" -> GoToTollOrParkingCharges
                        _  -> NoAction
                    ][  textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , color Color.black700
                        , text item.val
                        , textSize FontSize.a_14
                        , lineHeight "16"
                        , fontStyle $ FontStyle.regular LanguageStyle
                        , padding $ PaddingRight 20
                        ]
                      , linearLayout
                        [ weight 1.0
                        , height WRAP_CONTENT
                        , orientation HORIZONTAL
                        , padding $ PaddingLeft 20
                        , gravity RIGHT
                        ][
                          imageView
                          [ height $ V 12
                          , width $ V 12
                          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
                          , margin $ MarginTop 4
                          , color Color.black900
                          , fontStyle $ FontStyle.semiBold LanguageStyle
                          ] 
                        ]           
                    ]
                , linearLayout 
                  [ height $ V 1
                  , width MATCH_PARENT
                  , background Color.grey800
                  , margin $ MarginHorizontal 16 16
                  , visibility if (index + 1) /= (DA.length config.otherOptions) then VISIBLE else GONE
                  ][]
                ]
              )config.otherOptions)
      ] <> if config.fareInfoText /= "" then [fareInfoTextView push config] else []  
  ]
      
fareInfoTextView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
fareInfoTextView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ PaddingHorizontal 20 20
  , margin $ MarginTop 8
  ][  textView $ 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.red
      , text "*"
      , padding $ PaddingRight 5
      ] <> FontStyle.body3 LanguageStyle
    , textView $ 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black700
      , text $ config.fareInfoText
      ] <> FontStyle.paragraphText LanguageStyle
  ]

driverAdditionView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
driverAdditionView push config = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 20 20
    ][ commonTV push (getStringByKey config "DRIVER_ADDITIONS_OPTIONAL") Color.black800 FontStyle.subHeading1 LEFT 20 NoAction
     , commonTV push (getStringByKey config "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC") Color.black650 FontStyle.body3 LEFT 12 NoAction
     , horizontalScrollView 
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ MarginTop 12
          , cornerRadius 8.0
          ](DA.mapWithIndex (\index item -> 
            linearLayout
            ([ width WRAP_CONTENT
            , height WRAP_CONTENT
            , stroke $ "1," <> Color.grey900
            , orientation VERTICAL
            , gravity CENTER
            ] <> showCornerRadii index)
            [  relativeLayout 
                ([ width WRAP_CONTENT
                , height WRAP_CONTENT
                , background Color.blue600
                ] <> showCornerRadii index)
                [ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color Color.black700
                    , padding $ Padding 6 12 6 12
                    , text item.val
                    , visibility INVISIBLE
                    ] <> FontStyle.body3 LanguageStyle
                  , textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color Color.black700
                    , text item.key
                    , padding $ Padding 6 12 6 12
                    , gravity CENTER
                    ] <> FontStyle.body3 LanguageStyle]
              , linearLayout 
                [ height $ V 1
                , width MATCH_PARENT
                , background Color.grey900
                ][]
              , relativeLayout 
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color Color.black700
                    , text item.key
                    , padding $ Padding 6 12 6 12
                    , background Color.blue600
                    , visibility INVISIBLE
                    , gravity CENTER
                    ] <> FontStyle.body3 LanguageStyle
                  , textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color Color.black700
                    , padding $ Padding 6 12 6 12
                    , text item.val
                    ] <> FontStyle.body3 LanguageStyle
                ]
              ]) config.driverAdditions)       
        ]
         
     , commonTV push (getStringByKey config "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE") Color.black650 FontStyle.body3 LEFT 12 NoAction
    ]
    where showCornerRadii index = if index == 0 
                                then [cornerRadii $ Corners 8.0 true false false true ]
                              else if index == (DA.length config.driverAdditions - 1)
                                then [cornerRadii $ Corners 8.0 false true true false]
                              else []

fareUpdateView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
fareUpdateView push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 20 0 20 160
  ][  commonTV push (getStringByKey config "FARE_UPDATE_POLICY") Color.black800 FontStyle.subHeading1 LEFT 8 NoAction
    , commonTV push (getStringByKey config "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS") Color.black650 FontStyle.body3 LEFT 12 NoAction
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , textFromHtml $ getStringByKey config "REASON_CHANGE_IN_ROUTE"
      , margin $ MarginTop 20
      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , textSize FontSize.a_14
      , lineHeight "16"
      , fontStyle $ FontStyle.regular LanguageStyle
      , color Color.black650
      , textFromHtml $ getStringByKey config "WAITING_CHARGES_APPLICABLE"
      , margin $ MarginTop 12
      ]
  ]

tollOrParkingView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tollOrParkingView push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 20 0 20 160
  ][
    commonTV push (getStringByKey config "TOLL_CHARGES") Color.black800 FontStyle.subHeading1 LEFT 20 NoAction,
    commonTV push (getStringByKey config "TOLL_CHARGES_DESC") Color.black800 FontStyle.body3 LEFT 8 NoAction,
    commonTV push (getStringByKey config "PARKING_CHARGES") Color.black800 FontStyle.subHeading1 LEFT 8 NoAction,
    commonTV push (getStringByKey config "PARKING_CHARGES_DESC") Color.black800 FontStyle.body3 LEFT 8 NoAction
  ]

paymentfareBreakup :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
paymentfareBreakup push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 20 20
  ][ commonTV push (getStringByKey config "FEE_CORRESPONDING_TO_DISTANCE") Color.black700 FontStyle.body3 LEFT 20 NoAction
    , fareList push config
    , imageView
      [ width MATCH_PARENT
      , height $ V 2 
      , margin $ MarginTop 10
      , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_horizontal_dash"
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 10
      ][ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , lineHeight "18"
          , fontStyle $ FontStyle.medium LanguageStyle
          , color Color.black800
          , textSize FontSize.a_14
          , text $ getStringByKey config "TOTAL_PAYABLE"
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_14
          , color Color.black800
          , fontStyle $ FontStyle.medium LanguageStyle
          , text $ getStringByKey config "TOTAL_PAYABLE_VAL"
          , gravity RIGHT
          , weight 1.0
          ]
        ]
      , PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig config)
  ]

primaryButtonConfig :: Config -> PrimaryButton.Config 
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
      { text = state.primaryButtonConfig.text
      , color = state.primaryButtonConfig.color
      }
      , margin = state.primaryButtonConfig.margin
      , cornerRadius = state.primaryButtonConfig.cornerRadius
      , background = state.primaryButtonConfig.background
       , height = state.primaryButtonConfig.height
      , id = "RateCardButton"
      , enableRipple = state.primaryButtonConfig.enableRipple
      , rippleColor = state.primaryButtonConfig.rippleColor
      }
  in primaryButtonConfig'


commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , onClick push $ const action
  ] <> theme TypoGraphy

getStringByKey :: Config -> String -> String
getStringByKey config key = do
  let arr =  DA.filter (\item -> (item.key == key) ) config.additionalStrings
  case arr DA.!! 0 of
    Just ob -> ob.val
    Nothing -> ""

rentalRateCardView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rentalRateCardView push config =
  scrollView
  [ width MATCH_PARENT
  , height if os == "IOS" then (V 350) else WRAP_CONTENT
  ] 
  [ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 16 0 16 16
    ]
    [ fareList push config
    , dottedHorizontalLineView push config
    , commonTV push (getStringByKey config "TOTAL_FARE_CHANGE") Color.black900 FontStyle.paragraphText LEFT 0 NoAction
    , commonTV push (getStringByKey config "EXCESS_DISTANCE_CHARGE_DESCRIPTION") Color.black650 FontStyle.paragraphText LEFT 16 NoAction
    , commonTV push (getStringByKey config "NIGHT_TIME_FEE_DESCRIPTION") Color.black650 FontStyle.paragraphText LEFT 16 NoAction
    , commonTV push (getStringByKey config "PARKING_FEES_AND_TOLLS_NOT_INCLUDED") Color.black650 FontStyle.paragraphText LEFT 16 NoAction
    , dottedHorizontalLineView push config
    , commonTV push ("* " <> getStringByKey config "FARE_ACCORDING_TO_GOVERNMENT") Color.black650 FontStyle.paragraphText LEFT 0 NoAction
    ]
  ]

dottedHorizontalLineView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
dottedHorizontalLineView push config =
  imageView
  [ width MATCH_PARENT
  , height $ V 2 
  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_horizontal_dash"
  , margin $ MarginVertical 16 16
  ]