{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectFaqScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Screens.SelectFaqScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SelectFaqScreen.View as SelectFaqScreen
import Components.SettingSideBar.Controller as SettingSideBar
import ModifyScreenState (modifyScreenState, FlowState(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.SelectFaqScreen.Transformer
import Data.Array as DA
import Data.String as DS 
import Services.Backend as Remote
import JBridge
import Engineering.Helpers.Utils
import Locale.Utils
import Services.API
import Mobility.Prelude
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.Types as ST
import Data.Maybe
import Components.ChatView (makeChatComponent')
import Screens.SelectFaqScreen.Transformer
import Engineering.Helpers.Commons (getCurrentUTC)
import Screens.RideSelectionScreen.Controller (getTitle)
import Screens.SelectFaqScreen.ScreenData
import Components.IssueView (IssueInfo)

selectFaqScreen :: FlowBT String FlowState
selectFaqScreen = do
  (GlobalState globalState) <- getState
  let selectFaqScreenState = globalState.selectFaqScreen
  if DA.null selectFaqScreenState.data.categories then do 
    let language = fetchLanguage $ getLanguageLocale languageKey 
    (GetCategoriesRes response) <- Remote.getCategoriesBT language
    let categories' = map (\(Category catObj) ->{ categoryName : if (language == "en") then capitalize catObj.category else catObj.category , categoryId : catObj.issueCategoryId, categoryAction : catObj.label, categoryImageUrl : catObj.logoUrl}) response.categories
    modifyScreenState $ SelectFaqScreenStateType (\selectFaqScreen -> selectFaqScreen { data {categories = categories' } } )
  else pure unit
  (GlobalState updatedGlobalState) <- getState
  act <- lift $ lift $ runScreen $ SelectFaqScreen.screen updatedGlobalState.selectFaqScreen
  
  case act of
    GoBack updatedState -> goBackHandler updatedState
    GoHome updatedState -> goHomeHandler updatedState
    GoToChatScreen selectedCategory updatedState -> goToChatScreenHandler selectedCategory updatedState
    GoToFaqScreen selectedIssue updatedState -> goToFaqScreenHandler selectedIssue updatedState 

goBackHandler :: SelectFaqScreenState -> FlowBT String FlowState
goBackHandler updatedState =  do 
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HelpAndSupportScreenFlow)


goHomeHandler :: SelectFaqScreenState -> FlowBT String FlowState
goHomeHandler updatedState = do 
  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBar.CLOSED}}}) 
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  App.BackT $ App.BackPoint <$> (pure HomeScreenFlow)

goToFaqScreenHandler :: CategoryListType -> SelectFaqScreenState -> FlowBT String FlowState -- to see: set all FAQs isExpanded to falsegit 
goToFaqScreenHandler selectedCategory updatedState = do
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState)
  let categoryName' = getTitle selectedCategory.categoryAction
  modifyScreenState $ SelectFaqScreenStateType (
    \updatedState ->  updatedState {
      data {
        categoryName = categoryName'
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure FaqScreenFlow)

goToChatScreenHandler :: CategoryListType ->  SelectFaqScreenState -> FlowBT String FlowState 
goToChatScreenHandler selectedCategory updatedState =  do
  modifyScreenState $ SelectFaqScreenStateType (\_ -> updatedState) 
  let language = fetchLanguage $ getLanguageLocale languageKey
  (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT language selectedCategory.categoryId "" ""
  let 
    options' = DA.mapWithIndex (\index (Option optionObj) -> optionObj{ option = (show (index + 1)) <> ". " <> (optionObj.option)}) getOptionsRes.options
    messages' = DA.mapWithIndex (\index (Message currMessage) -> makeChatComponent' (currMessage.message) "Bot" (getCurrentUTC "") "Text" (500 * (index + 1)))getOptionsRes.messages
    chats' = map (
      \(Message currMessage) -> Chat {
        chatId : currMessage.id,
        chatType : "IssueMessage",
        timestamp : getCurrentUTC ""
      }
    ) getOptionsRes.messages
    categoryName = getTitle selectedCategory.categoryAction

  modifyScreenState $ ReportIssueChatScreenStateType (
    \updatedState ->  updatedState {
      data {
        chats = chats'
      , categoryName = categoryName
      , categoryId = selectedCategory.categoryId
      , options = options'
      , chatConfig = ReportIssueChatScreenData.initData.data.chatConfig{
          messages = messages'
        }
      }
    }
  )
  App.BackT $ App.BackPoint <$> (pure $ IssueReportChatScreenFlow)



