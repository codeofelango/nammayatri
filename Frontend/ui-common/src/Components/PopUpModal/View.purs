{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopUpModal.View where

import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), (/=),  not, (<<<), bind, discard, show, pure, map, when, mod)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), JustifyContent(..), FlexDirection(..), FlexWrap(..), AlignItems(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, accessibilityHint, accessibility, textFromHtml, shimmerFrameLayout, onAnimationEnd, id, flexBoxLayout, justifyContent, flexDirection, flexWrap, alignItems, rippleColor)
import Components.PopUpModal.Controller (Action(..), Config, CoverMediaConfig)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Components.TipsView as TipsView
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth, getNewIDWithTag, getVideoID, getYoutubeData)
import PrestoDOM.Properties (cornerRadii)
import Common.Types.App
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, getNewIDWithTag)
import Data.Array ((!!), mapWithIndex, null, length, findIndex)
import Data.Maybe (Maybe(..),fromMaybe)
import Control.Monad.Trans.Class (lift)
import JBridge (setYoutubePlayer, supportsInbuildYoutubePlayer, addMediaPlayer)
import Animation (fadeIn) as Anim
import Data.String (replaceAll, Replacement(..), Pattern(..))
import Data.Function.Uncurried (runFn5)
import PrestoDOM.Animation as PrestoAnim
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Timers
import Mobility.Prelude (boolToVisibility)
import Engineering.Helpers.Utils(splitIntoEqualParts)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , accessibility DISABLE
    , background state.backgroundColor
    , afterRender
        ( \action -> do
            when 
              (state.option2.enableTimer || state.option1.enableTimer)
                $ do
                  let timerValue' = if state.option2.enableTimer then state.option2.timerValue else state.option1.timerValue
                  startTimer timerValue' state.timerId "1" push CountDown
        )
        (const NoAction)
    , onClick
        ( \action -> do
            _ <- push action
            clearTheTimer state
            pure unit
        )
        if state.backgroundClickable && state.dismissPopup then const DismissPopup else if state.backgroundClickable then const OnButton1Click else const NoAction
    , gravity state.gravity
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin state.dismissIconMargin
        , gravity RIGHT
        , visibility state.dismissIconVisibility
        ][ imageView
            [ height $ V 21
            , width $ V 21
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_dismiss" 
            ]
        ]
     , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadii state.cornerRadius
        , orientation VERTICAL
        , background Color.white900
        , margin state.margin
        , padding state.padding
        , accessibility DISABLE
        , clickable true
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility if state.topTextVisibility then VISIBLE else state.coverImageConfig.visibility
            , cornerRadii state.cornerRadius
            , accessibility DISABLE_DESCENDANT
            , orientation VERTICAL
            ][  textView $
                [ width state.topTitle.width
                , height state.topTitle.height
                , margin state.topTitle.margin
                , color state.topTitle.color
                , gravity state.topTitle.gravity
                , text state.topTitle.text
                , visibility state.topTitle.visibility
                ] <> (FontStyle.h2 LanguageStyle)
              , imageView
                [ height state.coverImageConfig.height
                , width state.coverImageConfig.width
                , margin state.coverImageConfig.margin
                , padding state.coverImageConfig.padding
                , imageWithFallback state.coverImageConfig.imageUrl
                , visibility state.coverImageConfig.visibility
                , onClick push $ const OnCoverImageClick
                ]
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility state.coverMediaConfig.visibility
            , cornerRadii state.cornerRadius
            , accessibility DISABLE_DESCENDANT
            , orientation VERTICAL
            ][  textView $
                [ width state.topTitle.width
                , height state.topTitle.height
                , margin state.topTitle.margin
                , color state.topTitle.color
                , gravity state.topTitle.gravity
                , text state.topTitle.text
                , visibility $ boolToVisibility $ state.topTitle.visibility == VISIBLE && state.onlyTopTitle == VISIBLE
                ] <> (FontStyle.h2 LanguageStyle) 
             , linearLayout
                [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , gravity CENTER
                    , margin $ state.coverMediaConfig.coverMediaText.margin
                    , padding state.coverMediaConfig.coverMediaText.padding
                    , visibility $ state.coverMediaConfig.coverMediaText.visibility
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , color $ state.coverMediaConfig.coverMediaText.color
                    , gravity $ state.coverMediaConfig.coverMediaText.gravity
                    , textFromHtml state.coverMediaConfig.coverMediaText.text
                    , accessibility ENABLE
                    , accessibilityHint $ replaceAll (Pattern " ,") (Replacement ":") state.coverMediaConfig.coverMediaText.text
                    , visibility $ state.coverMediaConfig.coverMediaText.visibility
                    ]  <> (FontStyle.getFontStyle state.coverMediaConfig.coverMediaText.textStyle LanguageStyle)
                    , imageView [
                    width state.coverMediaConfig.coverMediaText.suffixImage.width
                    , height state.coverMediaConfig.coverMediaText.suffixImage.height
                    , imageWithFallback state.coverMediaConfig.coverMediaText.suffixImage.imageUrl
                    , visibility state.coverMediaConfig.coverMediaText.suffixImage.visibility
                    , margin state.coverMediaConfig.coverMediaText.suffixImage.margin
                    ]
                ]
              , linearLayout[
                 height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin state.coverMediaConfig.margin
                , background state.coverMediaConfig.background
                , stroke state.coverMediaConfig.stroke
                , visibility state.coverMediaConfig.visibility
                , cornerRadius state.coverMediaConfig.cornerRadius
                , padding state.coverMediaConfig.padding
                ][  PrestoAnim.animationSet [Anim.fadeIn (state.coverMediaConfig.visibility == VISIBLE) ] $   linearLayout
                    [ height state.coverMediaConfig.height
                    , width state.coverMediaConfig.width
                    , gravity CENTER
                    , id (getNewIDWithTag  state.coverMediaConfig.id)
                    , onAnimationEnd
                        ( \action -> do
                            let
                                mediaType = state.coverMediaConfig.mediaType
                                id = getNewIDWithTag state.coverMediaConfig.id
                                url = state.coverMediaConfig.mediaUrl
                                audioAutoPlay = state.coverMediaConfig.audioAutoPlay
                            if (supportsInbuildYoutubePlayer unit) then 
                                case mediaType of
                                    "VideoLink" -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig  "VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                    "PortraitVideoLink" -> pure $ runFn5 setYoutubePlayer (getYoutubeDataConfig  "PORTRAIT_VIDEO" (getVideoID url)) id (show PLAY) push YoutubeVideoStatus
                                    "Audio" -> addMediaPlayer id url audioAutoPlay
                                    "AudioLink" -> addMediaPlayer id url audioAutoPlay
                                    _ -> pure unit
                                else pure unit
                        )(const NoAction)
                    ][]
                  ]
                ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView $
                [ text $ state.primaryText.text
                , accessibilityHint state.primaryText.text
                , accessibility ENABLE
                , color $ state.primaryText.color
                , margin $ state.primaryText.margin
                , gravity $ state.primaryText.gravity
                , width if state.dismissPopupConfig.visibility == VISIBLE then WRAP_CONTENT else MATCH_PARENT
                , height WRAP_CONTENT
                , visibility $ state.primaryText.visibility
                ] <> (FontStyle.getFontStyle state.primaryText.textStyle LanguageStyle)
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                , visibility state.dismissPopupConfig.visibility
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , margin state.dismissPopupConfig.margin
                    , onClick push $ const OnImageClick
                    , padding state.dismissPopupConfig.padding
                    ]
                    [ imageView
                        [ width state.dismissPopupConfig.width
                        , height state.dismissPopupConfig.height
                        , imageWithFallback state.dismissPopupConfig.imageUrl
                        , visibility state.dismissPopupConfig.visibility
                        ]
                    ]
                ]
            ]
        , linearLayout
          [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity state.secondaryText.gravity
            , margin $ state.secondaryText.margin
            , padding state.secondaryText.padding
            , visibility $ state.secondaryText.visibility
            , onClick push $ const OnSecondaryTextClick
          ][ imageView [
               width state.secondaryText.prefixImage.width
               , height state.secondaryText.prefixImage.height
               , imageWithFallback state.secondaryText.prefixImage.imageUrl
               , visibility state.secondaryText.prefixImage.visibility
               , margin state.secondaryText.prefixImage.margin
             ]
            , textView $
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color $ state.secondaryText.color
             , gravity $ state.secondaryText.gravity
             , textFromHtml state.secondaryText.text
             , accessibility ENABLE
             , accessibilityHint $ replaceAll (Pattern " ,") (Replacement ":") state.secondaryText.text
             , visibility $ state.secondaryText.visibility
             ]  <> (FontStyle.getFontStyle state.secondaryText.textStyle LanguageStyle)
            , imageView [
               width state.secondaryText.suffixImage.width
               , height state.secondaryText.suffixImage.height
               , imageWithFallback state.secondaryText.suffixImage.imageUrl
               , visibility state.secondaryText.suffixImage.visibility
               , margin state.secondaryText.suffixImage.margin
             ]
          ]
        , if (null state.listViewArray) then textView[height $ V 0] else listView push state
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , orientation HORIZONTAL
            , margin state.buttonLayoutMargin
            ]
            [   linearLayout
                [ width $ if state.optionButtonOrientation == "VERTICAL" then MATCH_PARENT else if (not state.option1.visibility) || (not state.option2.visibility) then MATCH_PARENT else WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation if state.optionButtonOrientation == "VERTICAL" then VERTICAL else HORIZONTAL
                ]
                [ linearLayout
                    ([ if state.option2.visibility then width state.option1.width 
                       else weight 1.0
                    , background state.option1.background
                    , height $ state.option1.height
                    , cornerRadius 8.0
                    , visibility $ if state.option1.visibility then VISIBLE else GONE
                    , stroke $ "1," <> state.option1.strokeColor
                    , clickable state.option1.isClickable
                    , alpha $ if state.option1.isClickable then 1.0 else 0.5
                    , margin  state.option1.margin
                    , padding  state.option1.padding
                    , gravity CENTER
                    , accessibility DISABLE
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton1Click)
                    ] <> (if state.option1.enableRipple then [rippleColor state.option1.rippleColor] else []))
                    [   shimmerFrameLayout
                        [ width MATCH_PARENT
                        , height MATCH_PARENT
                        , visibility $  if state.option1.showShimmer then VISIBLE else GONE
                        , cornerRadius 8.0
                        ][ linearLayout
                            [ width MATCH_PARENT
                            , height MATCH_PARENT
                            , background Color.white200
                            , cornerRadius 8.0
                            ][]
                        ]
                        , linearLayout
                        [ width $ MATCH_PARENT
                        , height $ MATCH_PARENT
                        , visibility $ if state.option1.showShimmer then GONE else VISIBLE
                        , gravity state.option1.gravity
                        ]
                        [ imageView [
                            imageWithFallback state.option1.image.imageUrl
                            , height state.option1.image.height
                            , width state.option1.image.width
                            , margin state.option1.image.margin
                            , visibility state.option1.image.visibility
                            , padding state.option1.image.padding
                            ]
                            , textView $
                            [ width WRAP_CONTENT
                            , height WRAP_CONTENT
                            , accessibility ENABLE
                            , text $ if state.option1.enableTimer && state.option1.timerValue > 0 then (state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else state.option1.text
                            , accessibilityHint $ (if state.option1.enableTimer && state.option1.timerValue > 0 then ( state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else (replaceAll (Pattern ",") (Replacement ":") state.option1.text)) <> " : Button"
                            , color $ state.option1.color
                            , gravity CENTER
                            ] <> (FontStyle.getFontStyle state.option1.textStyle LanguageStyle)
                        ]
                    ]
                , linearLayout
                    ([ if state.dontShowRetry then width state.option1.width 
                       else if state.option1.visibility then width state.option2.width else weight 1.0
                    , height state.option2.height
                    , background state.option2.background
                    , cornerRadius 8.0
                    , visibility if state.option2.visibility then VISIBLE else GONE
                    , stroke ("1," <> state.option2.strokeColor)
                    , margin state.option2.margin
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton2Click)
                    , padding state.option2.padding
                    , accessibility DISABLE
                    , orientation VERTICAL
                    , gravity state.option2.gravity
                    , clickable state.option2.isClickable
                    , alpha (if state.option2.isClickable then 1.0 else 0.5)
                    ] <> (if state.option2.enableRipple then [rippleColor state.option2.rippleColor] else []))
                    [   imageView [
                            imageWithFallback state.option2.image.imageUrl
                            , height state.option2.image.height
                            , width state.option2.image.width
                            , margin state.option2.image.margin
                            , visibility state.option2.image.visibility
                            , padding state.option2.image.padding
                        ]
                        , textView $ 
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , accessibility ENABLE
                        , text $ if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else state.option2.text
                        , accessibilityHint $ (if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else (replaceAll (Pattern ",") (Replacement ":") state.option2.text)) <> " : Button"
                        , color state.option2.color
                        , gravity CENTER
                        ] <> (FontStyle.getFontStyle state.option2.textStyle LanguageStyle)
                    ]
                ]
            ]
        , linearLayout [
            height state.optionWithHtml.height
            , width  state.optionWithHtml.width
            , margin state.optionWithHtml.margin
            , clickable state.optionWithHtml.isClickable
            , background state.optionWithHtml.background
            , cornerRadius state.optionWithHtml.cornerRadius
            , visibility if state.optionWithHtml.visibility then VISIBLE else GONE
            , stroke ("1," <> state.optionWithHtml.strokeColor)
            , gravity CENTER
            , alpha (if state.optionWithHtml.isClickable then 1.0 else 0.5)
            , onClick
                ( \action -> do
                    _ <- push action
                    clearTheTimer state
                    pure unit
                )
                (const OptionWithHtmlClick)
          ][
                textView $
                [ textFromHtml $ state.optionWithHtml.textOpt1.text
                , accessibilityHint state.optionWithHtml.textOpt1.text
                , accessibility ENABLE
                , color $ state.optionWithHtml.textOpt1.color
                , margin $ state.optionWithHtml.textOpt1.margin
                , gravity $ state.optionWithHtml.textOpt1.gravity
                , visibility $ state.optionWithHtml.textOpt1.visibility
                ] <> (FontStyle.getFontStyle state.optionWithHtml.textOpt1.textStyle LanguageStyle)
                , imageView [
                    imageWithFallback state.optionWithHtml.image.imageUrl
                    , height state.optionWithHtml.image.height
                    , width state.optionWithHtml.image.width
                    , margin state.optionWithHtml.image.margin
                    , visibility state.optionWithHtml.image.visibility
                    , padding state.optionWithHtml.image.padding
                ]
                , textView $
                [ textFromHtml $ state.optionWithHtml.textOpt2.text
                , accessibilityHint state.optionWithHtml.textOpt2.text
                , accessibility ENABLE
                , color $ state.optionWithHtml.textOpt2.color
                , margin $ state.optionWithHtml.textOpt2.margin
                , gravity $ state.optionWithHtml.textOpt2.gravity
                , visibility $ state.optionWithHtml.textOpt2.visibility
                ] <> (FontStyle.getFontStyle state.optionWithHtml.textOpt2.textStyle LanguageStyle)
            ]
        ]
    ]
    where 
    getYoutubeDataConfig videoType videoId = getYoutubeData {
        videoType = videoType,
        videoId = videoId
        }

tipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
tipsView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility $ boolToVisibility state.isTipPopup
  ][ TipsView.view (push <<< TipsViewActionController) $ tipsViewConfig state ]

listView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
listView push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL 
    , margin $ Margin 16 0 16 20
    ](map (\item-> 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 12 0 0 4
        , orientation HORIZONTAL
        ][  imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_circle"
            , height $ V 6 
            , alpha 0.7
            , width $ V 6
            , margin $ MarginTop 8
            ]
          , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , margin $ MarginLeft 8
            , text item 
            , accessibility ENABLE
            , accessibilityHint item
            ] <> (FontStyle.getFontStyle FontStyle.ParagraphText LanguageStyle)

        ]
        ) state.listViewArray)

clearTheTimer :: Config -> Effect Unit
clearTheTimer config =
  if config.option1.enableTimer then do
    pure $ clearTimerWithId config.option1.timerID
  else if config.option2.enableTimer then do
    pure $ clearTimerWithId config.option2.timerID
  else
    pure unit

contactView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
contactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin state.contactViewMargin
    , stroke ("1," <> Color.borderColorLight)
    , padding state.contactViewPadding
    , cornerRadius 8.0
    , visibility state.contactViewConfig.visibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        ]
        [ linearLayout
            [ height $ V 24
            , width $ V 24
            , background Color.yellow900
            , cornerRadius 12.0
            , gravity CENTER
            ]
            [ textView $
                [ text state.contactViewConfig.nameInitials
                , color Color.black800
                ] <> FontStyle.body3 TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding state.contactViewConfig.padding
            ]
            [ textView $
                [ text state.contactViewConfig.fullName
                , color Color.black800
                ] <> FontStyle.subHeading1 TypoGraphy
            ]
        ]
    ]

tipsViewConfig :: Config -> TipsView.Config
tipsViewConfig state = let  
  config = TipsView.config
  tipsViewConfig' = config {
    activeIndex = state.activeIndex
  , isVisible = state.isVisible
  , customerTipArray = state.customerTipArray
  , customerTipArrayWithValues = state.customerTipArrayWithValues
  , tipLayoutMargin = (Margin 22 2 22 22)
  , fareEstimate = state.fareEstimate
  , fareEstimateText = state.fareEstimateText
  , tipSelected = state.tipSelected
  , tipSelectedText = state.tipSelectedText
  , showTipInfo = true
  , enableTips = state.isTipEnabled
  , searchExpired = true
  }
  in tipsViewConfig'

