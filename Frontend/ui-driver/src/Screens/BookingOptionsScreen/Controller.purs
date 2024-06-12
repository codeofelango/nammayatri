module Screens.BookingOptionsScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, (<>), (==), not, ($), (>))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (BookingOptionsScreenState, VehicleP)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Helpers.Utils (getVehicleVariantImage)
import Language.Strings (getString)
import Language.Types (STR(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BookingOptionsScreen"
    BackPressed -> pure unit
    _ -> pure unit

data Action = BackPressed
            | AfterRender
            | ChooseVehicleAC ChooseVehicle.Action
            | PrimaryButtonAC PrimaryButton.Action
            | DowngradeVehicle
            | ToggleRental

data ScreenOutput = GoBack | SelectCab BookingOptionsScreenState Boolean | EnableRental BookingOptionsScreenState Boolean

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = exit GoBack
eval (ChooseVehicleAC (ChooseVehicle.OnSelect config)) state = do
  let updatedConfig = map (\item -> item{isSelected = if item.vehicleVariant == config.vehicleVariant then (not item.isSelected) else item.isSelected}) state.data.downgradeOptions
      -- btnActive = length (filter (\item -> item.isSelected) updatedConfig) > 0
  continue state{data{downgradeOptions = updatedConfig}, props {isBtnActive = true}}
eval (PrimaryButtonAC PrimaryButton.OnClick) state = exit $ SelectCab state false

eval DowngradeVehicle state = do
  let downgraded = not state.props.downgraded
      updatedDowngradeOptions = map (\item -> item{isSelected = downgraded}) state.data.downgradeOptions
  exit $ SelectCab state{ data{ downgradeOptions = updatedDowngradeOptions } } true

eval ToggleRental state = do
   let isEnabled = not state.props.canSwitchToRental
   exit $ EnableRental state{ props { canSwitchToRental = isEnabled } } isEnabled

eval _ state = continue state

downgradeOptionsConfig :: Array VehicleP -> String -> ChooseVehicle.Config
downgradeOptionsConfig vehicles vehicleType =
  ChooseVehicle.config
    { vehicleImage = getVehicleVariantImage vehicleType
    , isCheckBox = true
    , vehicleVariant = vehicleType
    , isBookingOption = true
    , capacity = getVehicleCapacity vehicleType
    , isSelected = (fromMaybe dummyVehicleP $ (filter (\item -> item.vehicleName == vehicleType) vehicles) !! 0 ).isSelected
    }

getVehicleCapacity :: String -> String
getVehicleCapacity vehicleType = case vehicleType of
  "TAXI" -> getString ECONOMICAL <> " · " <>  "4 " <> getString PEOPLE
  "SUV"  -> getString SPACIOUS <> " · " <> "6 " <> getString PEOPLE
  "BIKE" -> "Bike" <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_TAXI" -> "Ambulance" <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_TAXI_OXY" -> "Ambulance Taxi" <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_AC" -> "Ambulance AC" <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_AC_OXY" -> "Ambulance AC Oxy" <> " · " <> "1 " <> getString PEOPLE
  "AMBULANCE_VENTILATOR" -> "Ambulance Ventilator" <> " · " <> "1 " <> getString PEOPLE
  _      -> getString COMFY <> " · " <> "4 " <> getString PEOPLE

dummyVehicleP :: VehicleP
dummyVehicleP = {
  vehicleName : "",
  isSelected : false
}