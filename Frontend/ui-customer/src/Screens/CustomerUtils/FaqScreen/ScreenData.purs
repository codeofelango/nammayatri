{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FaqScreen.ScreenData where

import Screens.Types
import Common.Types.App 
import MerchantConfig.DefaultConfig as DC
import ConfigProvider
import Data.Maybe(Maybe(..))
import Foreign.Object (empty)
import MerchantConfig.Types (AppConfig)
import Foreign (Foreign)
import Foreign.Object (Object)

initData :: FaqScreenState
initData = {
  data: {
    source : "",
    destination : "",
    date : "",
    time : "",
    rating : 0,
    driverName : "",
    totalAmount : "",
    isNull : true,
    status : "",
    rideStartTime : "",
    rideEndTime : "",
    vehicleNumber : "",
    rideId : "",
    tripId : "",
    bookingId : "",
    faresList : [],
    email : "",
    description : "",
    accountStatus : ACTIVE,
    config : getAppConfig appConfig,
    vehicleVariant : Nothing,
    logField : empty,
    dropDownList : [],
    issueListType : HELP_AND_SUPPORT_SCREEN_MODAL,
    categories : [],
    categoryName : "",
    merchantExoPhone : ""
  },
  props:{
    apiFailure : false
  , isCallConfirmation : false
  , showDeleteAccountView : false
  , btnActive : false
  , needIssueListApiCall : true
  }

}

-- ################################################  Types   ################################################

type FaqScreenState =
  {
    data :: FaqScreenData,
    props :: FaqScreenProps
  }

type FaqScreenData =
  {
    date :: String,
    time :: String,
    rating :: Int,
    source :: String,
    destination :: String,
    driverName :: String,
    totalAmount :: String,
    isNull :: Boolean,
    faresList :: Array FareComponent,
    status :: String,
    rideStartTime :: String,
    rideEndTime :: String,
    rideId :: String,
    vehicleNumber :: String,
    tripId :: String,
    bookingId :: String,
    email :: String,
    description :: String,
    accountStatus :: DeleteStatus ,
    config :: AppConfig,
    vehicleVariant :: Maybe VehicleVariant,
    dropDownList :: Array DropDownInfo,
    issueListType :: IssueModalType,
    categories :: Array CategoryListType,
    categoryName :: String,
    merchantExoPhone :: String,
    logField :: Object Foreign
  }

type FaqScreenProps =
  {
    apiFailure :: Boolean
  , isCallConfirmation :: Boolean
  , showDeleteAccountView :: Boolean
  , btnActive :: Boolean
  , needIssueListApiCall :: Boolean
  }

