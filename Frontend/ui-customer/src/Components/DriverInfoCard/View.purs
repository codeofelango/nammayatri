{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.View where

import Common.Types.App
import Animation (fadeIn, fadeInWithDelay, scaleYAnimWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String (replaceAll, Pattern, Replacement)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth, getCurrentUTC, getUTCAfterNSeconds, convertUTCtoISC, getUTCAfterNSecondsImpl)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getAssetsBaseUrl, getPaymentMethod, secondsToHms, makeNumber, getVariantRideType, getTitleConfig, getCityNameFromCode)
import Language.Strings (getString)
import Resources.Localizable.EN (getEN)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (<), (-), (*), bind, pure, discard, not, (&&), (||), (/=),(+), (+), void)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (BottomSheetState(..), Accessiblity(..), Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), accessibility, accessibilityHint, afterRender, alignParentBottom, alignParentLeft, alignParentRight, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gradient, gravity, height, id, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onAnimationEnd, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, shimmerFrameLayout, rippleColor, layoutGravity)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..), SheetState(..),City(..))
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Storage (KeyStore(..))
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import JBridge(fromMetersToKm, differenceBetweenTwoUTC)
import Engineering.Helpers.Suggestions (getMessageFromKey)
import Helpers.Utils (parseFloat)
import Data.Int(toNumber)
import MerchantConfig.Types (DriverInfoConfig)
import Mobility.Prelude (boolToVisibility)
import Locale.Utils
import Components.DriverInfoCard.Common.View
import Components.DriverInfoCard.Common.Types
import Common.Types.App (RideType(..)) as RideType
import Timers (rideDurationTimer)
import Data.Function.Uncurried (runFn2)
import Data.Int (floor, toNumber)
import Effect.Unsafe (unsafePerformEffect)

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  linearLayout
  [ height WRAP_CONTENT
  , width $ V (screenWidth unit)
  , background Color.transparent
  , orientation VERTICAL
  , id $ getNewIDWithTag "BottomSheetLayout"
  , afterRender push $ const $ NoAction
  ][ driverInfoViewSpecialZone push state
  , driverInfoView push state
   ]

driverInfoViewSpecialZone :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoViewSpecialZone push state =
  linearLayout
  [ width  MATCH_PARENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ state.props.currentSearchResultType == QUOTES OneWaySpecialZoneAPIDetails
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.grey700
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][linearLayout
            [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , orientation VERTICAL
            , id $ getNewIDWithTag "driverInfoViewSpecialZone"
            ][ linearLayout
               [ height $ WRAP_CONTENT
               , width $ MATCH_PARENT
               , gravity CENTER
               ][linearLayout
                 [ gravity CENTER
                 , background Color.transparentGrey
                 , height $ V 4
                 , width $ V 34
                 , accessibility ENABLE
                 , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                 , clickable true
                 , onClick push $ const ToggleBottomSheet
                 , margin (MarginVertical 8 6)
                 , cornerRadius if os == "IOS" then 2.0 else 4.0
                 ][]
               ]
              , titleAndETA push state
              , driverDetailsView (getDriverDetails state) "SpecialDriverDetailsView"
              , navigateView push state
              , paymentMethodView push state (getString FARE_ESTIMATE) true "SpecialPaymentMethodView"
            ]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , background Color.grey700
            ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push (getTripDetails state)
              , cancelRideLayout push state
              , brandingBannerView state.data.config.driverInfoConfig INVISIBLE Nothing
            ]
          ]
      ]
  ]

titleAndETA :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
titleAndETA push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  ][ if state.props.currentStage == RideAccepted then specialZoneHeader (getValueToLocalStore SELECTED_VARIANT)
     else distanceView push state
  ]

specialZoneHeader :: forall w. String -> PrestoDOM ( Effect Unit) w
specialZoneHeader vehicleVariant =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , margin $ MarginTop 6
  , accessibility ENABLE
  , accessibilityHint $ "Board the first" <> (getTitleConfig vehicleVariant).text <> (getEN $ TAXI_FROM_ZONE "TAXI_FROM_ZONE")
  , accessibility DISABLE_DESCENDANT
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ][  textView $
          [ text $ getString BOARD_THE_FIRST <> " "
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ (getTitleConfig vehicleVariant).text <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if ((getLanguageLocale languageKey)  == "ML_IN") then GONE else VISIBLE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL ]
      [ textView $
          [ text $ (getTitleConfig vehicleVariant).text <> " "
          , color $ (getTitleConfig vehicleVariant).color
          , height WRAP_CONTENT
          , visibility if ((getLanguageLocale languageKey) == "ML_IN") then VISIBLE else GONE
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy
        , textView $
          [ text $ getString $ TAXI_FROM_ZONE "TAXI_FROM_ZONE"
          , color Color.black800
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          ] <> FontStyle.h2 TypoGraphy]

  ]

navigateView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
navigateView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ V 44
  , background Color.white900
  , margin $ Margin 16 12 16 0
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , gravity CENTER
  , accessibility ENABLE
  , accessibilityHint $ (getEN $ GO_TO_ZONE "GO_TO_ZONE") <> " : Button"
  , accessibility DISABLE_DESCENDANT
  , visibility $ boolToVisibility $ state.props.currentSearchResultType == QUOTES OneWaySpecialZoneAPIDetails && state.props.currentStage == RideAccepted
  , onClick push $ const $ OnNavigateToZone
  ][ imageView
     [ width $ V 20
     , height $ V 20
     , margin $ MarginRight 8
     , imageWithFallback $ fetchImage FF_ASSET "ic_navigation_blue"
     ]
   , textView $ 
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , gravity CENTER
     , text $ getString $ GO_TO_ZONE "GO_TO_ZONE"
     , color Color.blue900
     ] <> FontStyle.subHeading1 TypoGraphy
  ]

driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility $ boolToVisibility $ state.props.currentSearchResultType /= QUOTES OneWaySpecialZoneAPIDetails
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , background if state.props.zoneType == METRO then Color.blue800 else Color.grey900
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ state.data.config.driverInfoConfig.cardStroke
         ][ linearLayout
            [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , orientation VERTICAL
            ][linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , id $ getNewIDWithTag "driverInfoView"
              ][linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , background Color.blue800
                , cornerRadii $ Corners 24.0 true true false false
                , gravity CENTER
                , orientation HORIZONTAL
                , padding (PaddingVertical 4 4)
                , visibility $ boolToVisibility $ state.props.zoneType == METRO
                ][imageView
                  [ width (V 15)
                  , height (V 15)
                  , margin (MarginRight 6)
                  , accessibility DISABLE
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_metro_white"
                  ]
                , textView
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , textSize FontSize.a_14
                  , accessibility if state.props.zoneType == METRO then ENABLE else DISABLE
                  , accessibilityHint "Metro Ride"
                  , text (getString METRO_RIDE)
                  , color Color.white900
                  ]
                ]
              , linearLayout
                [ orientation VERTICAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                , background Color.grey700
                , gravity CENTER
                , cornerRadii $ Corners 24.0 true true false false
                ][ linearLayout
                  [ gravity CENTER
                  , background Color.transparentGrey
                  , height $ V 4
                  , width $ V 34
                  , accessibility ENABLE
                  , accessibilityHint $ "Bottom Sheet : Scrollable element : " <> if state.data.bottomSheetState == EXPANDED then "Scroll down to collapse details" else  "Scroll up to expand for more ride actions"
                  , margin $ MarginTop 8
                  , clickable true
                  , onClick push $ const ToggleBottomSheet
                  , cornerRadius if os == "IOS" then 2.0 else 4.0
                  ][]
                  , contactView push state
                  , if state.props.currentStage == RideStarted then distanceView push state else dummyView push
                  , addStopView push state
                  , rentalDetailsView push state
                  , driverDetailsView (getDriverDetails state) "DriverDetailsView"
                  , paymentMethodView push state (getString FARE_ESTIMATE) true "PaymentMethodView"
                ]
              ]
              , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , background Color.grey700
                ][if not state.data.config.showPickUpandDrop then dummyView push else sourceDestinationView push (getTripDetails state)
                , cancelRideLayout push state
                , brandingBannerView state.data.config.driverInfoConfig INVISIBLE Nothing
                ]
            ]
        ]
      ]
  ]

distanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
distanceView push state = let 
  feature = state.data.config.feature
  in
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , onAnimationEnd push $ const $ NoAction
  , padding $ Padding 16 8 16 14
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , accessibility ENABLE
    , accessibilityHint $ getEN ENJOY_THE_RIDE
    ][ textView $
       [ text $ getTitleText
       , color Color.black900
       , ellipsize true
       , maxLines 2
       ] <> FontStyle.body7 TypoGraphy
     ]
  , linearLayout
    [ width MATCH_PARENT
    , gravity RIGHT
    , height WRAP_CONTENT
    , visibility $ boolToVisibility $ state.props.rideType == RideType.RENTAL_RIDE
    ][linearLayout
      [ height $ V 40
      , width $ V 64
      , gravity CENTER
      , cornerRadius if os == "IOS" then 20.0 else 32.0
      , background state.data.config.driverInfoConfig.callBackground
      , stroke state.data.config.driverInfoConfig.callButtonStroke
      , onClick push $ const $ MessageDriver
      , accessibilityHint "Chat and Call : Button"
      , accessibility ENABLE
      , rippleColor Color.rippleShade
      ][ imageView
          [ imageWithFallback  $ if feature.enableChat then if state.props.unReadMessages then fetchImage FF_ASSET "ic_chat_badge_green" else fetchImage FF_ASSET "ic_call_msg" else fetchImage FF_COMMON_ASSET "ny_ic_call"
          , height $ V state.data.config.driverInfoConfig.callHeight
          , width $ V state.data.config.driverInfoConfig.callWidth
          ]
      ]
    ]
  ]
  where 
    getTitleText :: String
    getTitleText = do
      if state.props.rideType /= RideType.RENTAL_RIDE || state.data.rentalData.startTimeUTC == "" then (getString ENJOY_THE_RIDE)
      else 
        let startTime = state.data.rentalData.startTimeUTC
            nhiHaiStartTime = startTime /= ""
            baseDuration = state.data.rentalData.baseDuration            
            endUTC = if nhiHaiStartTime then  runFn2 getUTCAfterNSecondsImpl startTime (baseDuration * 60 * 60) else ""
            endTimeInHH = if nhiHaiStartTime then convertUTCtoISC endUTC "h" <> ":" <> convertUTCtoISC endUTC "mm" <> " " <> convertUTCtoISC endUTC "A" else ""
        in if state.data.destination /= "" then getString RENTAL_RIDE_UNTIL <> " " <> endTimeInHH
          else "Add Stop to continue ride" <> (if state.props.endOTPShown then " or share End-OTP to end ride" else "")

brandingBannerView :: forall w. DriverInfoConfig -> Visibility -> Maybe String -> PrestoDOM (Effect Unit) w
brandingBannerView driverInfoConfig isVisible uid = 
  let brandingVisibility = if not driverInfoConfig.footerVisibility then GONE else isVisible
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    , gravity BOTTOM
    , visibility $ brandingVisibility
    ][ separator (MarginTop 0) (V 1) Color.grey900 true
     , linearLayout
       ([ width MATCH_PARENT
       , height WRAP_CONTENT
       , gravity CENTER
       , background driverInfoConfig.footerBackgroundColor
       , padding $ Padding 12 12 12 (12+safeMarginBottom)
       ] <> if isJust uid then [id $ getNewIDWithTag $ fromMaybe "" uid] else [])
       [textView $
        [ text $ getString POWERED_BY 
        , width WRAP_CONTENT    
        , height WRAP_CONTENT
        , color Color.black800
        , padding $ PaddingRight 6
        ] <> FontStyle.body3 TypoGraphy
      , imageView
        [ imageWithFallback $ driverInfoConfig.footerImageUrl
        , width $ V 62
        , height $ V 20
        ]
      ]
    ]

cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
cancelRideLayout push state =
  PrestoAnim.animationSet [ scaleYAnimWithDelay (getAnimationDelay FunctionCall)] $ 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , onAnimationEnd push $ const $ NoAction
  , margin $ if state.data.config.showPickUpandDrop then MarginTop 0 else MarginTop 12
  , padding $ PaddingBottom if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0
  , visibility $ boolToVisibility $ Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]
  ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ Padding 10 14 10 16
    , accessibilityHint "Cancel Ride : Button"
    , accessibility ENABLE
    , margin $ if os == "IOS" then MarginVertical 0 24 else MarginVertical 2 8
    , onClick push $ const $ CancelRide state
    , rippleColor Color.rippleShade
    , cornerRadius 20.0
    ][ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , color Color.black700
      , textFromHtml $ "<u>" <> (getString CANCEL_RIDE) <> "</u>"
      , alpha $ if (getMerchant FunctionCall) == MOBILITY_PM then 0.54 else 1.0
      ] <> FontStyle.body1 TypoGraphy
    ]
  ]

---------------------------------- contactView ---------------------------------------
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
contactView push state =
  let
    feature = state.data.config.feature
    eta = secondsToHms (fromMaybe 0 state.data.eta)
  in
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 4 16 16
    , visibility if (Array.any (_ == state.props.currentStage) [ RideAccepted, ChatWithDriver ]) then VISIBLE else GONE
    ][  linearLayout
        [ width (V (((screenWidth unit)/3 * 2)-27))
        , height WRAP_CONTENT
        , accessibilityHint $ "Ride Status : " <> if eta /= "--" then (state.data.driverName <> " is " <> eta <> " Away") else if state.data.waitingTime == "--" then (state.data.driverName <> " is on the way") else (state.data.driverName <> " is waiting for you.") 
        , accessibility ENABLE
        , orientation if length state.data.driverName > 16 then VERTICAL else HORIZONTAL
        ][  textView $
            [ text $ state.data.driverName <> " "
            , color Color.black900
            , ellipsize true
            , singleLine true
            ] <> FontStyle.body7 TypoGraphy
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ][ textView $
                [ text $"is " <> eta
                , color Color.black900
                , visibility $ boolToVisibility $ eta /= "--"
                ] <> FontStyle.body7 TypoGraphy
              , textView $
                [ text case eta /= "--" of
                    true -> getString AWAY
                    false -> if state.data.waitingTime == "--" then getString IS_ON_THE_WAY else getString IS_WAITING_AT_PICKUP
                , color Color.black900
                ] <> FontStyle.body7 TypoGraphy
              ]
          ]
      , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        , height WRAP_CONTENT
        ][linearLayout
          [ height $ V 40
          , width $ V 64
          , gravity CENTER
          , cornerRadius if os == "IOS" then 20.0 else 32.0
          , background state.data.config.driverInfoConfig.callBackground
          , stroke state.data.config.driverInfoConfig.callButtonStroke
          , onClick push $ const $ MessageDriver
          , accessibilityHint "Chat and Call : Button"
          , accessibility ENABLE
          , rippleColor Color.rippleShade
          ][ imageView
              [ imageWithFallback  $ if feature.enableChat then if state.props.unReadMessages then fetchImage FF_ASSET "ic_chat_badge_green" else fetchImage FF_ASSET "ic_call_msg" else fetchImage FF_COMMON_ASSET "ny_ic_call"
              , height $ V state.data.config.driverInfoConfig.callHeight
              , width $ V state.data.config.driverInfoConfig.callWidth
              ]
          ]
       ]
    ]


---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
ratingView push state =
  linearLayout
  [ orientation HORIZONTAL
  , margin $ MarginTop 40
  , height $ V 19
  , width $ V 50
  , padding $ Padding 6 3 6 3
  , background state.data.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  state.data.config.driverInfoConfig.ratingStroke
  , cornerRadius state.data.config.driverInfoConfig.ratingCornerRadius
  , accessibility DISABLE
  ][textView $
    [ text $ if state.data.rating == 0.0 then (getString NEW_) else show state.data.rating
    , color state.data.config.driverInfoConfig.ratingTextColor
    , gravity CENTER_VERTICAL
    , margin $ MarginLeft if os == "IOS" then 0 else 3
    , color Color.black700
    , accessibility DISABLE
    ] <> FontStyle.body16 TypoGraphy
  , imageView
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_star_active_rounded"
    , height $ V 11
    , width $ V 11
    , margin $ MarginLeft 3
    , accessibility DISABLE
    ]
  ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> Boolean -> String -> PrestoDOM (Effect Unit) w
paymentMethodView push state title shouldShowIcon uid =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , id $ getNewIDWithTag uid
  , margin $ Margin 16 12 16 12
  , background Color.white900
  , padding $ Padding 16 16 16 16
  , accessibility ENABLE
  , accessibilityHint $ "Fare Estimate :" <> state.data.config.currency <> show state.data.price <> " : Pay by cash or U P I"
  , cornerRadius 8.0
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity LEFT
      , accessibility DISABLE_DESCENDANT
      ][  textView $
          [ text title
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ state.data.config.currency <> show state.data.price
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout
          [ orientation HORIZONTAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , visibility if shouldShowIcon then VISIBLE else GONE
          ][  imageView
              [ imageWithFallback $ fetchImage FF_ASSET  "ny_ic_wallet_filled"
              , height $ V 20
              , width $ V 20
              ]
            , textView $
              [ text $ getString PAY_BY_CASH_OR_UPI
              , color Color.black700
              , padding $ PaddingLeft 4
              ] <> FontStyle.body3 TypoGraphy
            ]
    ]

specialZoneShimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
specialZoneShimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , linearLayout
      [ height $ V 40
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ customTextView (if state.props.currentStage == RideAccepted then 40 else 20) ((screenWidth unit) / 10 * 6) 0]
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ V 44
        , visibility $ boolToVisibility $ state.props.currentStage == RideAccepted
        , margin $ MarginVertical 4 12
        , cornerRadius 8.0
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0]
      , if state.props.currentStage == RideStarted then driverInfoShimmer else dummyView push
      , paymentMethodShimmer push state
      , addressShimmerView
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

shimmerView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
shimmerView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , background Color.grey700
  , padding $ PaddingHorizontal 16 16
  , cornerRadii $ Corners 24.0 true true false false
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , margin $ MarginVertical 8 12
    ][linearLayout
      [ gravity CENTER
      , background Color.transparentGrey
      , height $ V 4
      , width $ V 34
      , cornerRadius if os == "IOS" then 2.0 else 4.0
      ][] 
    ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ customTextView 20 ((screenWidth unit) / 10 * 6) 0
       , linearLayout [weight 1.0][]
       , shimmerFrameLayout
         [ height $ V 40
         , width $ V 64
         , cornerRadius 52.0
         , visibility $ boolToVisibility $ state.props.currentStage == RideAccepted
         ][linearLayout
           [ height $ V 40
           , width $ V 64
           , cornerRadius 52.0 
           , background Color.grey900
           ][]
         ]
      ]
      , driverInfoShimmer
      , paymentMethodShimmer push state
      , addressShimmerView
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 12 16
        ][ customTextView 20 80 0 ]
    ]

customTextView :: forall w. Int -> Int -> Int -> PrestoDOM (Effect Unit) w
customTextView height' width' bottomMargin' =
  shimmerFrameLayout
  [ cornerRadius 8.0
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , background Color.grey900
    , margin $ MarginBottom bottomMargin'
    , cornerRadius 8.0
    ][]
  ]

sfl :: forall w. Int -> Int -> Number -> PrestoDOM (Effect Unit) w
sfl height' width' radius' =
  shimmerFrameLayout
  [ cornerRadius radius'
  , stroke $ "1," <> Color.grey900
  , margin $ MarginBottom 12
  ][linearLayout 
    [ width $ V width'
    , height $ V height'
    , cornerRadius radius'
    , background Color.grey900
    ][]
  ]

paymentMethodShimmer :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
paymentMethodShimmer push state = 
  linearLayout
  [ height $ WRAP_CONTENT
  , width $ MATCH_PARENT
  , margin $ MarginBottom 12
  , padding $ Padding 16 16 16 16
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , gravity CENTER
  ][linearLayout
    [ height $ WRAP_CONTENT
    , width $ V ((screenWidth unit) / 2)
    , orientation VERTICAL
    ][ customTextView 16 80 4
     , customTextView 16 40 4
    ]
   , linearLayout[weight 1.0][]
   , customTextView 16 120 4
  ]

---------------------------------- separator ---------------------------------------

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]

---------------------------------- primaryButtonConfig ---------------------------------------

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let
    config' = PrimaryButton.config
    primaryButtonConfig' = config'
      { width = WRAP_CONTENT
      , height = WRAP_CONTENT
      , background = Color.mint
      , cornerRadius = 17.0
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 18
        , width = V 18
        , imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_call"
        , margin = Margin 20 10 20 10
        }
      , id = "CallButton"
      }
  in primaryButtonConfig'


---------------------------------- sourceToDestinationConfig ---------------------------------------

sourceToDestinationConfig :: DriverInfoCardState -> SourceToDestination.Config
sourceToDestinationConfig state = let
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = Margin 16 0 40 0
    , id = Just "DriverInfoCardSTDC"
    , overrideSeparatorCount = 6
    , separatorMargin = 19
    , sourceImageConfig {
        imageUrl = fetchImage FF_ASSET "ny_ic_pickup"
      , height = V 14
      , width = V 14
      }
    , sourceTextConfig {
        text = state.data.source
      , textStyle = FontStyle.Body1
      , ellipsize = true
      , margin = MarginLeft 10
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl =fetchImage FF_ASSET "ny_ic_drop"
      , height = V 14
      , width = V 14
      }
    , destinationTextConfig {
        text = state.data.destination
      , maxLines = 1
      , textStyle = FontStyle.Body1
      , margin = MarginLeft 10
      , ellipsize = true
      }
    , distanceConfig {
        distanceVisibility = VISIBLE
      , distanceValue = state.data.estimatedDistance <> " km"
      , background = Color.grey700
  }
    }
  in sourceToDestinationConfig'

destinationView ::  forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
destinationView push state=
  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , margin $ MarginBottom (if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0)
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text if true then "Drop :-" else  getString RIDE_FARE
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.destination
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 4
            ][ textView (
                [ text $ state.data.estimatedDistance <> " km"
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
              , linearLayout
                [height $ V 4
                , width $ V 4
                , cornerRadius 2.5
                , background Color.black600
                , margin (Margin 6 2 6 0)
                ][]
              , textView (
                [ text state.props.estimatedTime
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
            ]
      ]
openGoogleMap :: forall w . (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
openGoogleMap push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity LEFT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 20 15 20 15
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][ textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString NAVIGATE)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
        , imageView
          [ width $ V 20
          , height $ V 20
          , margin (MarginLeft 6)
          , imageWithFallback $ fetchImage FF_COMMON_ASSET  "ny_ic_navigation"
          ]
      ]
  ]

dummyView :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push  =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

configurations ∷ { letterSpacing ∷ Number , paddingOTP ∷ Padding , paddingOTPText ∷ Padding }
configurations =
  case os of
    "IOS" -> {paddingOTPText : PaddingVertical 4 4
              , letterSpacing : 6.0
              , paddingOTP : Padding 20 5 18 7}
    _     -> {paddingOTPText : PaddingVertical 2 2
              , letterSpacing : 3.0
              , paddingOTP : Padding 11 0 11 7
              }

getAnimationDelay :: LazyCheck -> Int
getAnimationDelay dummy = 100

getDriverDetails :: DriverInfoCardState -> DriverDetailsType
getDriverDetails state = {
  searchType : state.props.currentSearchResultType
  , rating : state.data.rating
  , driverName : state.data.driverName
  , vehicleDetails : state.data.vehicleDetails
  , vehicleVariant : state.data.vehicleVariant
  , merchantCity : state.props.merchantCity
  , registrationNumber : state.data.registrationNumber
  , config : state.data.config
  , rideStarted : state.props.currentStage == RideStarted
}

getTripDetails :: DriverInfoCardState -> TripDetails Action
getTripDetails state = {
  rideStarted : state.props.currentStage == RideStarted
  , source : state.data.source
  , destination : state.data.destination
  , onAnimationEnd : NoAction
  , backgroundColor : Color.white900
  , rideType : state.props.rideType
}
rentalDetailsView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
rentalDetailsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 8.0
  , padding $ Padding 16 16 16 16
  , afterRender push $ const NoAction
  , margin $ Margin 16 12 16 12
  , background Color.white900
  , visibility $ boolToVisibility $ state.props.rideType == RideType.RENTAL_RIDE
  ]
  [ rentalTimeView push state TIME false
  , separatorView true
  , rentalTimeView push state DISTANCE false
  , separatorView true
  , rentalTimeView push state STARTING_ODO true
  -- (Array.mapWithIndex (\index item -> TODO-codex : Refactor
  --   linearLayout
  --   [ height WRAP_CONTENT
  --   , width WRAP_CONTENT
  --   ]
  --   [ rentalTimeView push state item.text
  --   , separatorView $ index /= 2
  --   ]
  -- ) [{text : TIME}, {text : DISTANCE}, {text : STARTING_ODO}])
  ]

rentalTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> STR -> Boolean -> PrestoDOM (Effect Unit) w
rentalTimeView push state showText showInfo =
  let rentalData = state.data.rentalData
      isRideStarted = state.props.currentStage == RideStarted
  in 
    PrestoAnim.animationSet [ fadeIn true] $
    linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity $ if isTime showText then LEFT else CENTER
    , orientation VERTICAL
    ]
    [ linearLayout[height WRAP_CONTENT
      , width WRAP_CONTENT][textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString showText
      , color Color.black650
      , singleLine true
      ] <> FontStyle.body3 TypoGraphy
    , imageView 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue_lg"
            , height $ V 15
            , visibility $ boolToVisibility $ showInfo
            , width $ V 15 
            , margin $ MarginLeft 4 
            , onClick push $ const $ RentalInfo
            ]]
    , linearLayout
      [ height MATCH_PARENT
      , width $ if isTime showText then WRAP_CONTENT else MATCH_PARENT
      , orientation HORIZONTAL
      ] 
      [ textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ case showText of 
                TIME -> (if isRideStarted then state.props.rideDurationTimer else "0") <> " hr"
                DISTANCE -> show rentalData.baseDistance <> " km"
                _ -> if rentalData.startOdometer == "" then "-" else rentalData.startOdometer <> " km"
            , color Color.black800
            ] <> FontStyle.body1 TypoGraphy

      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , visibility $ boolToVisibility $ isTime showText
        , text $ " / " <> show rentalData.baseDuration <> " hr"
        , color Color.black600
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]
    where
      isTime :: STR -> Boolean
      isTime str =
        case str of
          TIME -> true
          _ -> false

addStopView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
addStopView push state =
  let isDestinationTextGiven = state.data.destination /= ""
  in 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginHorizontal 16 16
    , padding $ Padding 16 12 16 12
    , background Color.white900
    , orientation VERTICAL
    , cornerRadius 8.0
    , visibility $ boolToVisibility $ state.props.rideType == RideType.RENTAL_RIDE
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER_VERTICAL
      ]
      [ imageView 
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_circle"
        , height $ V 8
        , width $ V 8  
        , margin $ MarginRight 8
        ]
      , textView $
        [ text $ getString NEXT_STOP
        ] <> FontStyle.body3 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , gravity LEFT
      , margin $ MarginTop 2
      , accessibility DISABLE_DESCENDANT
      ]
      [ textView $
        [ text $ if isDestinationTextGiven then state.data.destination else getString NOT_ADDED_YET
        , ellipsize true
        , singleLine true
        , color Color.black800
        , weight 1.0
        , gravity LEFT
        ] <> FontStyle.body1 TypoGraphy
      , textView $
        [ text $ if isDestinationTextGiven then getString EDIT else getString ADD_NOW
        , color Color.blue800
        , onClick push $ const AddStop
        , width WRAP_CONTENT
        , padding $ Padding 16 4 16 4
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]

separatorView :: forall w . Boolean -> PrestoDOM (Effect Unit) w
separatorView visibility' =
  linearLayout
    [ height MATCH_PARENT
    , gravity CENTER
    , weight 1.0
    , visibility $ boolToVisibility $ visibility'
    ][ linearLayout
      [ width $ V 1
      , background Color.lightGrey
      , height MATCH_PARENT
      ][]
    ]
