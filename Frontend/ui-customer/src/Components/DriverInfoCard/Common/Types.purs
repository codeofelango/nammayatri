module Components.DriverInfoCard.Common.Types where

import Prelude

import MerchantConfig.Types (AppConfig)
import Screens.Types (City)
import Data.Maybe (Maybe(..))
import Common.Types.App 

type DriverDetailsType
  = { searchType :: SearchResultType
  , rating :: Number
  , driverName :: String
  , vehicleDetails :: String
  , vehicleVariant :: String
  , merchantCity :: City
  , registrationNumber :: String
  , config :: AppConfig
  , rideStarted :: Boolean
    }

type TripDetails a
  = { rideStarted :: Boolean
  , source :: String
  , destination :: String
  , onAnimationEnd :: a
  , backgroundColor :: String
  , rideType :: RideType
    }
