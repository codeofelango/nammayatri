{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.RemoteConfig.Types where

import Prelude
import Data.Maybe (Maybe)
import Foreign.Class  (class Decode, decode, class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding  (defaultDecode, defaultDecode, defaultEncode)
import Control.Monad.Except (runExcept, except)
import Foreign.Index (readProp)
import Data.Either as Either

type RemoteConfig a
  = { bangalore :: Maybe a
    , kolkata :: Maybe a
    , chennai :: Maybe a
    , tumakuru :: Maybe a
    , mysore :: Maybe a
    , kochi :: Maybe a
    , delhi :: Maybe a
    , hyderabad :: Maybe a
    , mumbai :: Maybe a
    , coimbatore :: Maybe a
    , pondicherry :: Maybe a
    , goa :: Maybe a
    , pune :: Maybe a
    , tamilnaducities :: Maybe a
    , default :: a
    , noida :: Maybe a
    , gurugram :: Maybe a
    , config :: Maybe Config
    }

newtype RCCarousel
  = RCCarousel
  { text_color :: String
  , text :: String
  , cta_text :: String
  , cta_action :: Maybe String
  , cta_link :: String
  , cta_icon :: String
  , banner_color :: String
  , banner_image :: String
  , cta_background_color :: String
  , cta_text_color :: String
  , cta_corner_radius :: String
  , cta_image_url :: String
  , whitelist :: Maybe (Array String)
  , categoryFilter :: Maybe (Array String)
  , image_banner :: Maybe String
  , dynamic_action :: Maybe RemoteAC
  }

derive instance genericRCCarousel :: Generic RCCarousel _

instance decodeRCCarousel :: Decode RCCarousel where
  decode = defaultDecode

newtype Config
  = Config
  { randomOrder :: Boolean
  }

derive instance genericConfig :: Generic Config _

instance decodeConfig :: Decode Config where
  decode = defaultDecode


newtype ForwardBatchConfigData = ForwardBatchConfigData
  { is_Forward_Dispatch_Feature_Enabled :: Boolean,
    advancedRidePopUpYoutubeLink :: String,
    callDriverInfoPost :: Boolean
  }
derive instance genericForwardBatchConfigData :: Generic ForwardBatchConfigData _

instance decodeForwardBatchConfigData :: Decode ForwardBatchConfigData where
  decode = defaultDecode

defaultForwardBatchConfigData :: ForwardBatchConfigData
defaultForwardBatchConfigData = ForwardBatchConfigData
  { is_Forward_Dispatch_Feature_Enabled: false,
    advancedRidePopUpYoutubeLink: "",
    callDriverInfoPost: false
  }

type TipsConfig
  = { autoRickshaw :: Maybe (Array Int),
      suv :: Maybe (Array Int),
      sedan :: Maybe (Array Int),
      hatchback :: Maybe (Array Int),
      bookAny :: Maybe (Array Int),
      taxi :: Maybe (Array Int),
      taxiPlus :: Maybe (Array Int),
      bike :: Maybe (Array Int),
      default :: Maybe (Array Int)
    }


---------------------------------Remote Config Dynamic AC-----------------------------------------------

data RemoteAC = Destination DestinationParams | WhereTo | Profile | MetroBooking | WebLink WebLinkParams | UpdateProfile | NoAction | Safety | ZooBooking | Rentals | Intercity

derive instance genericRemoteAC :: Generic RemoteAC _
instance decodeRemoteAC :: Decode RemoteAC where 
  decode body = 
    let default = runExcept $ defaultDecode body
    in except $ if Either.isRight default then default else Either.Right $ NoAction
instance encodeRemoteAC :: Encode RemoteAC where encode = defaultEncode

newtype DestinationParams = DestinationParams {
  lat :: Number,
  lng :: Number,
  description :: Maybe String,
  fullAddress :: Maybe String,
  placeId :: Maybe String
}

derive instance genericDestinationParams :: Generic DestinationParams _
instance decodeDestinationParams :: Decode DestinationParams where decode = defaultDecode
instance encodeDestinationParams :: Encode DestinationParams where encode = defaultEncode

newtype WebLinkParams = WebLinkParams {
  url :: String
}

derive instance genericWebLinkParams :: Generic WebLinkParams _
instance decodeWebLinkParams :: Decode WebLinkParams where decode = defaultDecode
instance encodeWebLinkParams :: Encode WebLinkParams where encode = defaultEncode
