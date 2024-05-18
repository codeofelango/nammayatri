{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resources.Constants where

import ConfigProvider

import Data.Array (filter, length, null, reverse, (!!), head, all, elem, foldl, mapMaybe, find)
import Accessor (_description, _amount, _amountWithCurrency)
import Common.Types.App (LazyCheck(..), OptionButtonList, Price)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (Pattern(..), Replacement(..), contains, joinWith, replaceAll, split, trim)
import Data.String as DS
import Engineering.Helpers.Commons (os)
import Helpers.Utils (parseFloat, toStringJSON, extractKeyByRegex, formatFareType)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (map, show, negate, (&&), (-), (<>), (==), (>), ($), (+), (/=), (<), (/), (*))
import Resources.Localizable.EN (getEN)
import Screens.Types as ST
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), SavedReqLocationAPIEntity(..), FareBreakupAPIEntity(..))

type Language
  = { name :: String
    , value :: String
    }

getDelayForAutoComplete :: Int
getDelayForAutoComplete = 800

getDelayForLocateOnMap :: Int
getDelayForLocateOnMap = 1000

getLanguages :: Array Language
getLanguages =
  [ { name: "English", value: "EN_US" }
  , { name: "മലയാളം", value: "ML_IN" }
  ]

data DecodeAddress
  = Booking BookingLocationAPIEntity
  | SavedLoc SavedReqLocationAPIEntity

decodeAddress :: DecodeAddress -> String
decodeAddress addressWithCons =
  let
    (BookingLocationAPIEntity address) = case addressWithCons of
      Booking bookingLocation -> bookingLocation
      SavedLoc savedLocation -> getBookingEntity savedLocation
  in
    if (all isNothing [address.city, address.area, address.country, address.building, address.door, address.street, address.city, address.areaCode, address.ward]) then
      ""
    else if (trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
      ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else if (trim (fromMaybe "" address.door) == "") then
      ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
    else
      ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))

encodeAddress :: String -> Array AddressComponents -> Maybe String -> Number -> Number -> ST.Address
encodeAddress fullAddress addressComponents placeId lat lon =
  let
    totalAddressComponents = length $ split (Pattern ", ") fullAddress
    areaCodeFromFullAdd = runFn2 extractKeyByRegex areaCodeRegex fullAddress
    areaCodeFromAddComp = getValueByComponent addressComponents "postal_code"
    areaCodeComp = if (trim areaCodeFromAddComp) /= "" then areaCodeFromAddComp else areaCodeFromFullAdd
    areaCodeVal = Just if (trim areaCodeComp) == "" then (runFn2 extractKeyByRegex areaCodeRegex $ runFn2 JB.getLocationNameV2 lat lon) else areaCodeComp
    splitedAddress = split (Pattern ", ") fullAddress
    gateName = getValueByComponent addressComponents "sublocality"
  in
    { area: if DS.null gateName then splitedAddress !! (totalAddressComponents - 4) else (Just gateName)
    , areaCode: Just if (trim areaCodeFromAddComp) /= "" then areaCodeFromAddComp else areaCodeFromFullAdd
    , building: splitedAddress !! (totalAddressComponents - 6)
    , city: splitedAddress !! (totalAddressComponents - 3)
    , country: splitedAddress !! (totalAddressComponents - 1)
    , state: splitedAddress !! (totalAddressComponents - 2)
    , door:
        if totalAddressComponents > 7 then
          splitedAddress !! 0 <> Just ", " <> splitedAddress !! 1
        else if totalAddressComponents == 7 then
          splitedAddress !! 0
        else
          Nothing
    , street: splitedAddress !! (totalAddressComponents - 5)
    , ward:
        if null addressComponents then
          getWard Nothing (splitedAddress !! (totalAddressComponents - 4)) (splitedAddress !! (totalAddressComponents - 5)) (splitedAddress !! (totalAddressComponents - 6))
        else
          Just $ getValueByComponent addressComponents "sublocality"
    , placeId: placeId
    }

getValueByComponent :: Array AddressComponents -> String -> String
getValueByComponent address componentName = getAddress $ filter (\(AddressComponents item) -> length (getByTag item.types componentName) > 0) address

getByTag :: Array String -> String -> Array String
getByTag tags componentName = (filter (\item -> contains (Pattern componentName) item) tags)

getAddress :: Array AddressComponents -> String
getAddress address = joinWith ", " (reverse (map (\(AddressComponents item) -> item.longName) address))

getBookingEntity :: SavedReqLocationAPIEntity -> BookingLocationAPIEntity
getBookingEntity (SavedReqLocationAPIEntity savedLocation) =
  BookingLocationAPIEntity
    { "area": savedLocation.area
    , "state": savedLocation.state
    , "country": savedLocation.country
    , "building": savedLocation.building
    , "door": savedLocation.door
    , "street": savedLocation.street
    , "city": savedLocation.city
    , "areaCode": savedLocation.areaCode
    , "lat": savedLocation.lat
    , "lon": savedLocation.lon
    , "ward": savedLocation.ward
    , "placeId": savedLocation.placeId
    }

getAddressFromSaved :: SavedReqLocationAPIEntity -> ST.Address
getAddressFromSaved (SavedReqLocationAPIEntity savedLocation) =
  { "area": savedLocation.area
  , "state": savedLocation.state
  , "country": savedLocation.country
  , "building": savedLocation.building
  , "door": savedLocation.door
  , "street": savedLocation.street
  , "city": savedLocation.city
  , "areaCode": savedLocation.areaCode
  , "ward": savedLocation.ward -- (getWard savedLocation.ward savedLocation.area savedLocation.street savedLocation.building)
  , "placeId": savedLocation.placeId
  }

getAddressFromBooking :: BookingLocationAPIEntity -> ST.Address
getAddressFromBooking (BookingLocationAPIEntity address) =
  { "area": address.area
  , "state": address.state
  , "country": address.country
  , "building": address.building
  , "door": address.door
  , "street": address.street
  , "city": address.city
  , "areaCode": address.areaCode
  , "ward": getWard address.ward address.area address.street address.building
  , "placeId": address.placeId
  }

getWard :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
getWard ward area street building =
  let
    actualWard = if (trim (replaceAll (Pattern ",") (Replacement "") (fromMaybe "" ward))) == "" then Nothing else ward

    actualArea = if (trim (fromMaybe "" area)) == "" then Nothing else (area <> Just ", ")

    actualStreet = if (trim (fromMaybe "" street)) == "" then Nothing else (street <> Just ", ")

    actualBuilding = if (trim (fromMaybe "" building)) == "" then Nothing else building
  in
    if isJust actualWard then actualWard else (actualArea <> actualStreet <> actualBuilding)

getKeyByLanguage :: String -> String
getKeyByLanguage language = case language of 
  "ENGLISH" -> "EN_US"
  "KANNADA" -> "KN_IN" 
  "HINDI"   -> "HI_IN" 
  "MALAYALAM" -> "ML_IN" 
  "TAMIL"   ->"TA_IN"
  "BENGALI" -> "BN_IN"
  _ -> "EN_US" 

getGender :: Maybe ST.Gender -> String -> String 
getGender gender placeHolderText = 
  case gender of 
    Just value -> case value of 
      ST.MALE -> (getString MALE)
      ST.FEMALE -> (getString FEMALE)
      ST.OTHER ->  (getString OTHER)
      _ -> (getString PREFER_NOT_TO_SAY)
    Nothing -> placeHolderText

getFaresList :: Array FareBreakupAPIEntity -> String -> Array ST.FareComponent
getFaresList fares chargeableRideDistance =
  let currency = (getAppConfig appConfig).currency
  in
  map
    ( \(FareBreakupAPIEntity item) -> let
          price = item.amountWithCurrency
          in
          { fareType : item.description
          , price : currency <> " " <> 
            (show $ case item.description of 
              "BASE_FARE" -> price.amount + getMerchSpecBaseFare fares
              "SGST" -> (price.amount * 2.0) + getFareFromArray fares "FIXED_GOVERNMENT_RATE"
              "WAITING_OR_PICKUP_CHARGES" -> price.amount + getFareFromArray fares "PLATFORM_FEE"
              _ -> price.amount)
          , title : case item.description of
                      "BASE_FARE" -> (getEN BASE_FARES) -- <> if chargeableRideDistance == "0 m" then "" else " (" <> chargeableRideDistance <> ")"
                      "EXTRA_DISTANCE_FARE" -> getEN NOMINAL_FARE
                      "DRIVER_SELECTED_FARE" -> getEN DRIVER_ADDITIONS
                      "TOTAL_FARE" -> getEN TOTAL_PAID
                      "DEAD_KILOMETER_FARE" -> getEN PICKUP_CHARGE
                      "PICKUP_CHARGES" -> getEN PICKUP_CHARGE
                      "CUSTOMER_SELECTED_FARE" -> getEN CUSTOMER_SELECTED_FARE
                      "WAITING_CHARGES" -> getEN WAITING_CHARGE
                      "EARLY_END_RIDE_PENALTY" -> getEN EARLY_END_RIDE_CHARGES
                      "WAITING_OR_PICKUP_CHARGES" -> getEN WAITING_CHARGE 
                      "SERVICE_CHARGE" -> getEN SERVICE_CHARGES
                      "FIXED_GOVERNMENT_RATE" -> getEN GOVERNMENT_CHAGRES
                      "PLATFORM_FEE" -> getEN PLATFORM_FEE
                      "SGST" -> getEN PLATFORM_GST
                      "CUSTOMER_CANCELLATION_DUES" -> getEN CUSTOMER_CANCELLATION_DUES
                      "TOLL_CHARGES" ->  getEN TOLL_CHARGES <> "⁺"
                      "DIST_BASED_FARE" -> getEN DIST_BASED_CHARGES
                      "TIME_BASED_FARE" -> getEN TIME_BASED_CHARGES
                      "EXTRA_TIME_FARE" -> getEN EXTRA_TIME_CHARGES
                      _ -> formatFareType $ item.description
          }
    )
    (getFilteredFares fares)

getMerchSpecBaseFare :: Array FareBreakupAPIEntity -> Number
getMerchSpecBaseFare fares =
  case getMerchant FunctionCall of
    YATRISATHI -> getAllFareFromArray fares ["EXTRA_DISTANCE_FARE", "NIGHT_SHIFT_CHARGE"]
    _ -> getAllFareFromArray fares ["EXTRA_DISTANCE_FARE"]


getAllFareFromArray :: Array FareBreakupAPIEntity -> Array String -> Number
getAllFareFromArray fares titles =
  let
    matchingFarePrices = mapMaybe 
                    (\(FareBreakupAPIEntity fare) -> 
                        if fare.description `elem` titles 
                          then Just fare.amountWithCurrency
                        else Nothing) fares
  in
    foldl (\acc fare -> acc + fare.amount) 0.0 matchingFarePrices

getFareFromArray :: Array FareBreakupAPIEntity -> String -> Number
getFareFromArray fareBreakUp fareType = do
  let fare = find (\fare -> fare^._description == (fareType)) fareBreakUp
  case fare of
    Just (FareBreakupAPIEntity fare) -> fare.amountWithCurrency.amount
    Nothing -> 0.0

dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amountWithCurrency : dummyPrice ,description: ""}

dummyPrice :: Price
dummyPrice = {amount: 0.0, currency: ""}

getMerchantSpecificFilteredFares :: Merchant -> Array String
getMerchantSpecificFilteredFares merchant = 
  case merchant of
    YATRISATHI -> ["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "NIGHT_SHIFT_CHARGE", "CGST", "PLATFORM_FEE", "FIXED_GOVERNMENT_RATE"]
    _ -> ["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "CGST", "NIGHT_SHIFT_CHARGE"]

getFilteredFares :: Array FareBreakupAPIEntity -> Array FareBreakupAPIEntity
getFilteredFares = filter (\(FareBreakupAPIEntity item) -> (all (_ /=  item.description) (getMerchantSpecificFilteredFares (getMerchant FunctionCall))))--["EXTRA_DISTANCE_FARE", "TOTAL_FARE", "BASE_DISTANCE_FARE", "CGST", "NIGHT_SHIFT_CHARGE"]) )

getKmMeter :: Int -> String
getKmMeter distance = if (distance < 1000) then toStringJSON distance <> " m" else (parseFloat ((toNumber distance)/ 1000.0)) 2 <> " km"

-- Info ::
-- Vehicle Variants for yatri sathi are SEDAN_TAXI (SEDAN , SUV, HATCHBACK) and NON_AC_TAXI (TAXI)
fetchVehicleVariant :: String -> Maybe ST.VehicleVariant
fetchVehicleVariant variant = 
  case variant of 
    "SUV"           -> Just ST.SUV
    "SEDAN"         -> Just ST.SEDAN
    "HATCHBACK"     -> Just ST.HATCHBACK
    "AUTO_RICKSHAW" -> Just ST.AUTO_RICKSHAW
    "TAXI"          -> Just ST.TAXI 
    "TAXI_PLUS"     -> Just ST.TAXI_PLUS
    _               -> Nothing

getVehicleCapacity :: String -> String 
getVehicleCapacity variant = 
  case fetchVehicleVariant variant of
    Just ST.SUV -> "6" 
    Just ST.AUTO_RICKSHAW -> "3"
    _ -> "4"

intMax :: Int
intMax = 2147483647

intMin :: Int
intMin = (-2147483647)

getDisabilityType :: String -> Array ST.DisabilityT -> ST.DisabilityT 
getDisabilityType disType disList = (fromMaybe dummyDisabilityList (head (filter(\item -> item.tag == disType) disList)))

dummyDisabilityList :: ST.DisabilityT
dummyDisabilityList ={
  tag : "OTHER",
  id : "8a365d73-b81e-6b21-962b-b1397aa687e0",
  description : "Other"
}

areaCodeRegex :: String 
areaCodeRegex = "\\b\\d{6}\\b"

ticketPlaceId :: String
ticketPlaceId = "1ef78db2-90de-4ed7-a38a-fcbb7ce28135"

ticketEntryId :: String
ticketEntryId = "b73378dc-427f-4efa-9b55-8efe7e3352c2"

ticketAquariumId :: String
ticketAquariumId = "a7eba6ed-99f7-442f-a9d8-00c8b380657b"

ticketCamId :: String
ticketCamId = "d8f47b42-50a5-4a97-8dda-e80a3633d7ab"

maxImageUploadInIssueReporting :: Int  
maxImageUploadInIssueReporting = 3 

-- Id for emergency contact initial chat suggestion on remote config
emergencyContactInitialChatSuggestionId :: String
emergencyContactInitialChatSuggestionId = "d6cddbb1a6aee372c0c7f05173da8f95"


cancelReasons :: Boolean -> Array OptionButtonList
cancelReasons showAcReason =
  ([ { reasonCode: "CHANGE_OF_PLANS"
    , description: getString CHANGE_OF_PLANS
    , subtext: Just $ getString NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS
    , textBoxRequired : false
    }
  ]) <>
  (if showAcReason 
      then [ { reasonCode: "AC_NOT_TURNED_ON"
              , description: getString AC_IS_NOT_AVAILABLE_ON_THIS_RIDE
              , subtext: Just $ getString AC_NOT_WORKING_DESC
              , textBoxRequired : false
            }]
      else []
  ) <>
  ([
    { reasonCode: "GOT_ANOTHER_RIDE"
    , description: getString GOT_ANOTHER_RIDE_ELSE_WHERE
    , subtext: Just $ getString CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP
    , textBoxRequired : false
    }
  , { reasonCode: "DRIVER_NOT_MOVING"
    , description: getString DRIVER_IS_NOT_MOVING
    , subtext: Just $ getString DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP
    , textBoxRequired : false
    }
  , { reasonCode: "WAIT_TIME_TOO_LONG"
    , description: getString WAIT_TIME_TOO_LONG
    , subtext: Just $ getString DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION
    , textBoxRequired : false
    }
  , { reasonCode: "WRONG_PICKUP_LOCATION"
    , description: getString WRONG_PICKUP_LOCATION
    , subtext: Just $ getString THE_PICKUP_LOCATION_ENTERED_WAS_WRONG
    , textBoxRequired : false
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    , subtext: Just $ getString SOME_OTHER_REASON
    , textBoxRequired : true
    }
  ])

dummyCancelReason :: OptionButtonList
dummyCancelReason =
  { reasonCode: ""
  , description: ""
  , textBoxRequired: false
  , subtext: Nothing
  }