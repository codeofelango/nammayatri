{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Handler where

import Components.IndividualRideCard.View as IndividualRideCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..), fromMaybe)
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons 
import ModifyScreenState (modifyScreenState, FlowState(..))
import Prelude
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.List as PrestoList
import Screens.RideSelectionScreen.ScreenData
import Screens.RideSelectionScreen.View as RideSelectionScreen
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Engineering.Helpers.Utils
import Locale.Utils
import Services.Backend as Remote
import Services.API 
import Data.Array
import Components.ChatView.Controller
import Screens.HelpAndSupportScreen.Transformer 
import Screens.RideSelectionScreen.Controller 
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.Types as ST
import Components.ServiceTierCard.View as ServiceTierCard
import Helpers.Utils as HU
import Storage (getValueToLocalStore, KeyStore(..))
import MerchantConfig.Types as MT
import ConfigProvider (getAppConfig)
import Constants (appConfig)
import Debug

rideSelection :: FlowBT String FlowState
rideSelection = do
  (GlobalState globalState) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideSelectionScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view (push <<< IndividualRideCardActionController) dummyIndividualCard
  logField_ <- lift $ lift $ getLogFields
  act <- lift $ lift $ runScreen $ RideSelectionScreen.screen globalState.rideSelectionScreen{shimmerLoader = AnimatedIn} listItemm

  case act of 
    GoBack state -> goBackHandler state
    LoaderOutput state -> loaderOutputHandler state
    SelectRide state -> selectRideHandler state
    RefreshScreen state -> refreshScreenHandler state
      



-- ##########################################################  Handlers ##########################################################


goBackHandler :: RideSelectionScreenState -> FlowBT String FlowState
goBackHandler state = do 
  modifyScreenState $ RideSelectionScreenStateType (\_ -> state )
  App.BackT $ App.NoBack <$> (pure HelpAndSupportScreenFlow) 


loaderOutputHandler :: RideSelectionScreenState -> FlowBT String FlowState
loaderOutputHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (
    \_ -> state {
      data {
        offsetValue = state.data.offsetValue + 8
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure RideSelectionScreenFlow)


selectRideHandler :: RideSelectionScreenState -> FlowBT String FlowState
selectRideHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (\_ -> state )
  let language = fetchLanguage $ getLanguageLocale languageKey
      config = getAppConfig appConfig
      tripId' = state.selectedItem <#> (_.rideId) 
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language state.selectedCategory.categoryId (fromMaybe "" state.data.selectedOptionId) (fromMaybe "" tripId') ""
  (GlobalState globalState) <- getState
  let 
    -- VaibhavD : need to shift this check to backend
    transformedOptions = transformIssueOptions getOptionsRes.options state.selectedItem globalState.helpAndSupportScreen.data.ongoingIssueList config.cityConfig
    getOptionsRes' = mapWithIndex (
      \index (Option optionObj) -> optionObj { 
        option = optionObj.option 
      }
    ) transformedOptions

    messages' = mapWithIndex (
      \index (Message currMessage) -> makeChatComponent' currMessage.message currMessage.messageTitle currMessage.messageAction "Bot" (getCurrentUTC "") "Text" (500*(index + 1))
    ) getOptionsRes.messages

    chats' = globalState.reportIssueChatScreen.data.chats <>
      [ Chat {
            chatId : fromMaybe "" state.data.selectedOptionId
          , chatType : "IssueOption"
          , timestamp : getCurrentUTC ""
        }
      ] <>
      map (
        \(Message currMessage) -> Chat {
          chatId : currMessage.id, 
          chatType : "IssueMessage", 
          timestamp : getCurrentUTC ""
        } 
      ) getOptionsRes.messages

    merchantExoPhone' = case state.selectedItem of
      Just item -> Just item.merchantExoPhone
      _         -> Nothing

  modifyScreenState $ ReportIssueChatScreenStateType (\_ -> 
    ReportIssueChatScreenData.initData { 
      data {
        entryPoint = ReportIssueChatScreenData.RideSelectionScreenEntry
      , chats = chats'
      , tripId = tripId'
      , merchantExoPhone = merchantExoPhone'
      , selectedCategory = state.selectedCategory
      , options = getOptionsRes'
      , chatConfig { 
        messages = (globalState.reportIssueChatScreen.data.chatConfig.messages <> messages')
      }
      ,  selectedRide = state.selectedItem 
      } 
    } 
  )
  App.BackT $ App.NoBack <$> (pure IssueReportChatScreenFlow)


refreshScreenHandler :: RideSelectionScreenState -> FlowBT String FlowState
refreshScreenHandler state = do
  modifyScreenState $ RideSelectionScreenStateType (
    \_ -> state{
      data{
        offsetValue = 0
      }
    }
  )
  App.BackT $ App.NoBack <$> (pure  RideSelectionScreenFlow)

transformIssueOptions :: Array Option -> Maybe IndividualRideCardState -> Array ST.IssueInfo -> Array MT.CityConfig -> Array Option
transformIssueOptions options mbRide ongoingIssues arrCityConfig = filteredOptionsForAc
  where
    filteredOptionsForAc = case mbRide of
      Just ride -> let alreadyHasAcIssue = any (\issue -> issue.optionLabel == Just "AC_RELATED_ISSUE" && issue.rideId == Just ride.rideId) ongoingIssues
                   in
                    case ride.serviceTierName of
                      (Just name) -> if not alreadyHasAcIssue && ServiceTierCard.showACDetails name Nothing && cityConfig.enableAcViews
                                then options 
                                else filteredOptions
                      Nothing -> filteredOptions
      Nothing -> filteredOptions
    filteredOptions = filter (\(Option option) -> option.label /= "AC_RELATED_ISSUE") options
    cityConfig = HU.getCityConfig arrCityConfig (getValueToLocalStore CUSTOMER_LOCATION)