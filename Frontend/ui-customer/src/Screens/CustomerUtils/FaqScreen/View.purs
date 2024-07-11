{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTIEHULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FaqScreen.View where

import Common.Types.App
import Screens.CustomerUtils.FaqScreen.ComponentConfig
import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IssueList as IssueList
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.SourceToDestination as SourceToDestination
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array as DA
import Data.Either (Either(..))
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toStringJSON, strLenWithSpecificCharacters, fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (when, Unit, bind, const, discard, map, pure, unit, show, not, ($), (-), (/=), (<<<), (<=), (<>), (==), (||), (<), (<>), (&&))
import Presto.Core.Types.Language.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), afterRender, alignParentRight, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, width, imageWithFallback, weight, layoutGravity, clickable, alignParentBottom, scrollView, adjustViewWithKeyboard, lineHeight, singleLine, alpha, accessibility, accessibilityHint, textFromHtml)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Screens.FaqScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.FaqScreen.Transformer 
import Screens.Types as ST
import Services.API (RideBookingListRes(..), FetchIssueListResp(..), FetchIssueListReq(..))
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Mobility.Prelude (boolToVisibility)
import Locale.Utils
import Screens.FaqScreen.ScreenData (FaqScreenState)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Components.DropDownCard as DropDownCard
import Data.Array as DA
import Helpers.CommonView (emptyTextView)

screen :: FaqScreenState -> Screen Action FaqScreenState ScreenOutput
screen initialState =
  {
    initialState
  , view
  , name : "FaqScreen"
  , globalEvents : [
      ( \push -> do
        -- void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
          -- if initialState.data.config.feature.enableSelfServe && initialState.props.needIssueListApiCall then do
          --   let language = EHU.fetchLanguage $ getLanguageLocale languageKey
          --   (FetchIssueListResp issueListResponse) <- Remote.fetchIssueListBT language
          --   lift $ lift $ doAff do liftEffect $ push $ FetchIssueListApiCall issueListResponse.issues
          -- else pure unit
          -- when (initialState.data.source == "") $ lift $ lift $  getPastRides RideBookingListAPIResponseAction push initialState
        pure $ pure unit
      )
  ]
  , eval : \state  action -> do
      let _ = spy  "FaqScreen action " state
      let _ = spy  "FaqScreen state " action
      eval state action
  }

view :: forall w . (Action -> Effect Unit) -> FaqScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation $
 relativeLayout
 [  height MATCH_PARENT
  , width MATCH_PARENT
 ]$[linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
    , onBackPressed push $ const BackPressed state.props.isCallConfirmation
    , afterRender push (const AfterRender)
    , visibility $ boolToVisibility $ state.data.issueListType == ST.HELP_AND_SUPPORT_SCREEN_MODAL 
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ](DA.mapWithIndex (\index item -> dropDownCardView state item push) $ state.data.dropDownList)
            
          ]
      , apiFailureView state push  
    ]
  ]

apiFailureView :: forall w. FaqScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w
apiFailureView state push=
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , visibility if state.props.apiFailure then VISIBLE else GONE
  ][  ErrorModal.view (push <<< APIFailureActionController) (apiErrorModalConfig state)]

dropDownCardView :: forall w. FaqScreenState -> DropDownInfo -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit ) w
dropDownCardView state dropDownCardInfo push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , gravity CENTER
  , margin $ Margin 10 10 10 10
  ][  DropDownCard.view (push <<< DropDownCardActionController) (dropDownCardConfig state dropDownCardInfo (dropDownCardData state push dropDownCardInfo))]

dropDownCardData :: FaqScreenState  -> (Action -> Effect Unit) -> DropDownInfo -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardData state push cardInfo =
  let stringsWithTypes = spy "hello world" $ dropDownStringTypes $ cardInfo.description
  in 
   linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 8 16 16
    ][  linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ] (map (\item -> 
          case item.type of
            "{HEADING}" -> dropDownCardHeadingView item.value
            "{BODY}" -> dropDownCardBodyView item.value
            -- "{IMAGE}" -> dropDownCardImageView item.value
            _ -> emptyTextView
        ) stringsWithTypes) 
      , PrimaryButton.view (push <<< PrimaryButtonRaiseTicketAC) (primaryButtonConfig state) 
    ] 
      

dropDownCardHeadingView :: String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardHeadingView title =
  textView $
    [ text title
    , color Color.darkCharcoal
    , width MATCH_PARENT
    , margin $ MarginVertical 12 4
    ] <> FontStyle.subHeading1 TypoGraphy

dropDownCardBodyView :: String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardBodyView description =
  textView $
    [ textFromHtml description
    , color Color.black700
    , width MATCH_PARENT
    -- , margin $ Margin 16 8 16 16
    ] <> FontStyle.body1 TypoGraphy

dropDownCardImageView :: String -> forall w . PrestoDOM (Effect Unit ) w
dropDownCardImageView imageUrl =
  imageView $
    [ imageWithFallback imageUrl
    , height $ V 100
    , width MATCH_PARENT
    , margin $ Margin 16 8 16 16
    ]