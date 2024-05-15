module Components.ChooseVehicle.Controller where

import Prelude (class Eq, class Show )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import PrestoDOM (Margin(..))
import Common.Types.App as CT
import Data.Maybe (Maybe(..))

data Action
  = NoAction
  | OnSelect Config
  | OnImageClick
  | ShowRateCard Config

type Config
  = { vehicleImage :: String
    , isSelected :: Boolean
    , vehicleVariant :: String
    , vehicleType :: String
    , capacity :: String
    , price :: String
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    , maxPrice :: Maybe Int
    , basePrice :: Int
    , showInfo :: Boolean
    , searchResultType :: CT.SearchResultType
    , isBookingOption :: Boolean
    , pickUpCharges :: Int 
    , layoutMargin :: Margin 
    , showStroke :: Boolean
    , serviceTierName :: Maybe String
    , validTill :: String
    , specialLocationTag :: Maybe String
    , selectedServices :: Array String
    }


config :: Config
config =
  { vehicleImage: ""
  , isSelected: false
  , vehicleVariant: ""
  , vehicleType: ""
  , capacity: ""
  , price: ""
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: ""
  , maxPrice : Nothing
  , basePrice : 0 
  , showInfo : false
  , searchResultType : CT.QUOTES CT.ONE_WAY
  , isBookingOption : false
  , pickUpCharges : 0
  , layoutMargin : MarginHorizontal 12 12
  , showStroke : true
  , serviceTierName : Nothing
  , validTill : ""
  , specialLocationTag : Nothing
  , selectedServices : []
  }
