{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Utils
    ( module Helpers.Utils
    , module ReExport
    )
    where

import ConfigProvider
import DecodeUtil
import Accessor (_deeplinkOptions, _distance_meters, _payload, _search_type, _paymentMethod)
import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), Payload(..), InnerPayload, DeeplinkOptions(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (resume, runFree)
import Data.Array (cons, deleteAt, drop, filter, head, length, null, sortBy, sortWith, tail, (!!), reverse, find)
import Data.Array.NonEmpty (fromArray)
import Data.Boolean (otherwise)
import Data.Date (Date)
import Data.Either (Either(..), hush)
import Data.Eq.Generic (genericEq)
import Data.Foldable (or)
import Data.Function.Uncurried (Fn2, runFn3, Fn1, Fn3)
import Data.Generic.Rep (class Generic)
import Data.Int (round, toNumber, fromString, ceil)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pi, sin, cos, sqrt, asin, abs)
import Data.Ord (comparing, Ordering)
import Data.Profunctor.Strong (first)
import Data.Show.Generic (genericShow)
import Data.String (replace, split, Pattern(..), Replacement(..), toLower)
import Data.String as DS
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff(..), error, killFiber, launchAff, launchAff_, makeAff, nonCanceler, Fiber)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Compat (EffectFn1, EffectFnAff, fromEffectFnAff, runEffectFn1, runEffectFn2, runEffectFn3, EffectFn2)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn1(..), EffectFn5(..), mkEffectFn1, mkEffectFn4, runEffectFn5)
import Effect.Uncurried (EffectFn1, EffectFn4, EffectFn3, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getWindowVariable, isPreviousVersion, liftFlow, os)
import Engineering.Helpers.Commons (parseFloat, setText, toStringJSON) as ReExport
import Engineering.Helpers.Utils (class Serializable, serialize)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (Foreign, decodeJSON, encodeJSON)
import Foreign.Generic (decode)
import JBridge (emitJOSEvent, Location)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader as Readers
import Juspay.OTP.Reader.Flow as Reader
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, comparing, discard, identity, map, mod, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (=<<), (==), (>), (>=), (>>>), (||))
import Prelude (class EuclideanRing, Unit, bind, discard, identity, pure, unit, void, ($), (+), (<#>), (<*>), (<>), (*>), (>>>), ($>), (/=), (&&), (<=), show, (>=), (>), (<), (#))
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (FlowWrapper(..), getState, modifyState)
import Screens.Types (RecentlySearchedObject,SuggestionsMap, SuggestionsData(..), HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..), NewContacts, Contacts, FareComponent, City(..), ZoneType(..))
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM.Core (terminateUI)
import Screens.Types (AddNewAddressScreenState, Contacts, CurrentLocationDetails, FareComponent, HomeScreenState, LocationItemType(..), LocationListItemState, NewContacts, PreviousCurrentLocations, RecentlySearchedObject, Stage(..), MetroStations)
import Screens.Types (RecentlySearchedObject, HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..), NewContacts, Contacts, FareComponent, SuggestionsMap, SuggestionsData(..),SourceGeoHash, CardType(..), LocationTagBarState, DistInfo)
import Services.API (Prediction, SavedReqLocationAPIEntity(..), GateInfoFull(..))
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn)
import Types.App (GlobalState(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Function.Uncurried
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Data.Tuple(Tuple(..) ,snd, fst)
import Data.Ord
import MerchantConfig.Types (CityConfig)
import MerchantConfig.DefaultConfig (defaultCityConfig)
import Data.Function.Uncurried (runFn1)
import Constants (defaultDensity)
import Mobility.Prelude
import Data.Array as DA
import Data.Argonaut.Decode.Class as AD
import Data.Argonaut.Encode.Class as AE
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode.Parser as ADP
import Common.DefaultConfig as CC
import Common.Types.Config as CCT
import MerchantConfig.Types 

foreign import shuffle :: forall a. Array a -> Array a

foreign import withinTimeRange :: String -> String -> String -> Boolean

foreign import isWeekend :: String -> Boolean

foreign import getNewTrackingId :: Unit -> String

foreign import storeCallBackCustomer :: forall action. (action -> Effect Unit) -> (String -> action) -> String -> Effect Unit

foreign import getLocationName :: forall action. (action -> Effect Unit) -> Number -> Number -> String -> (Number -> Number -> String -> action) -> Effect Unit

foreign import getCurrentDate :: String -> String
foreign import getCurrentDatev2 :: String -> String
foreign import getNextDate :: String -> String
foreign import getNextDateV2 :: String -> String
foreign import compareDate :: EffectFn2 String String Boolean
foreign import storeCallBackContacts :: forall action. (action -> Effect Unit) -> ((Array Contacts) -> action) -> Effect Unit
foreign import parseNewContacts :: String -> (Array NewContacts)
foreign import parseSourceHashArray :: String -> Array SourceGeoHash

foreign import secondsToHms :: Int -> String

foreign import getTime :: Unit -> Int

foreign import drawPolygon :: String -> String -> Effect Unit
foreign import getDifferenceBetweenDates :: Fn2 String String Int

foreign import removeLabelFromMarker :: EffectFn1 Number Unit
-- foreign import generateSessionToken :: String -> String
foreign import requestKeyboardShow :: String -> Effect Unit

foreign import factoryResetApp :: String -> Unit

foreign import validateEmail :: String -> Boolean

foreign import getUTCDay :: Date -> Int

foreign import makePascalCase :: String -> String

foreign import validateInputPattern :: String -> String -> Boolean

foreign import strLenWithSpecificCharacters :: String -> String -> Int

foreign import decodeError :: String -> String -> String

foreign import setRefreshing :: String -> Boolean -> Unit

foreign import setEnabled :: String -> Boolean -> Unit

foreign import _generateQRCode :: EffectFn5 String String Int Int (AffSuccess String) Unit

generateQR:: EffectFn4 String String Int Int Unit
generateQR  = mkEffectFn4 \qrString viewId size margin ->  launchAff_  $ void $ makeAff $
  \cb ->
    (runEffectFn5 _generateQRCode qrString viewId size margin (Right >>> cb))
    $> nonCanceler

foreign import saveToLocalStoreImpl :: String -> String -> EffectFnAff Unit
saveToLocalStore' :: String -> String -> EffectFnAff Unit
saveToLocalStore' = saveToLocalStoreImpl

foreign import fetchFromLocalStoreImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStore' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStore' = fetchFromLocalStoreImpl

foreign import fetchFromLocalStoreTempImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStoreTemp' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStoreTemp' = fetchFromLocalStoreTempImpl

foreign import fetchAndUpdateCurrentLocation :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> action -> Effect Unit

foreign import seperateByWhiteSpaces :: String -> String

foreign import getKeyInSharedPrefKeysConfigEff :: String -> Effect String
foreign import contactPermission :: Unit -> Effect Unit
foreign import performHapticFeedback :: Unit -> Effect Unit
foreign import adjustViewWithKeyboard :: String -> Effect Unit

foreign import getDefaultPixels :: Fn1 String Number

foreign import getMobileNumber :: EffectFn2 String String String
foreign import getDateAfterNDaysv2 :: Int -> String

foreign import extractKeyByRegex :: Fn2 String String String
foreign import getPixels :: Fn1 LazyCheck Number
foreign import getDeviceDefaultDensity ::Fn1 LazyCheck Number
foreign import didReceiverMessage :: Fn1 LazyCheck Boolean

foreign import incrOrDecrTimeFrom :: Fn3 String Int Boolean String

foreign import getMockFollowerName :: String -> String

decodeGeoJson :: String -> Maybe CCT.GeoJson
decodeGeoJson stringGeoJson = 
  case (AD.decodeJson =<< ADP.parseJson stringGeoJson) of
    Right resp -> Just resp
    Left err   -> Nothing

data TimeUnit
  = HOUR
  | MINUTE
  | SECOND

type SpecialZoneTagConfig = {
    icon :: String
  , text :: String
  , infoPopUpConfig :: Maybe SpecialZoneInfoPopUp
  , backgroundColor :: String
}

type SpecialZoneInfoPopUp = {
    title :: String
  , primaryText :: String
  , secondaryText :: String
  , primaryButtonText :: String
  , icon :: String
}

type SpecialLocationList = {
    geoJson :: String
  , gates :: Array Location
  , locationName :: String
  , category :: String
  , city :: String
}


convertUTCToISTAnd12HourFormat :: String -> Maybe String
convertUTCToISTAnd12HourFormat inputTime = do
  -- Convert the input time to a 24-hour format if it's in 12-hour format (AM/PM)
  let adjustedInputTime = replace (Pattern "PM") (Replacement "") $ replace (Pattern "AM") (Replacement "") inputTime

  case split (Pattern ":") adjustedInputTime of
    [h, m, _] -> do
      hours <- fromString h
      minutes <- fromString m
      
      -- Add 5 hours and 30 minutes
      let adjustRemainder = if minutes >= 30 then 1 else 0
          adjustedHours24 = (hours + 5 + adjustRemainder) `mod` 24
          adjustedMinutes = (minutes + 30) `mod` 60
      
      -- Convert to 12-hour format with AM/PM
      let {adjustedHours, period} = if adjustedHours24 < 12 then {adjustedHours: adjustedHours24, period: "AM"} else {adjustedHours: adjustedHours24 - 12, period: "PM"}
      
      let paddingHours = if adjustedHours < 10 then "0" else ""
          paddingMinutes = if adjustedMinutes < 10 then "0" else ""

      -- Format the adjusted time
      let adjustedTime = paddingHours <> show adjustedHours <> ":" <> paddingMinutes <> show adjustedMinutes <> " " <> period
  
      pure adjustedTime
    _ -> Nothing

convertTo12HourFormat :: String -> Maybe String
convertTo12HourFormat time = do
  -- Convert the input time to a 24-hour format if it's in 12-hour format (AM/PM)
  let adjustedInputTime = replace (Pattern "PM") (Replacement "") $ replace (Pattern "AM") (Replacement "") time

  case split (Pattern ":") adjustedInputTime of
    [h, m, _] -> do
      hours <- fromString h
      minutes <- fromString m
      let {adjustedHours, period} = if hours < 12 then {adjustedHours: hours, period: "AM"} else {adjustedHours: hours - 12, period: "PM"}
      let adjustedTime = show hours <> ":" <> show minutes <> " " <> period
      pure adjustedTime
    _ -> Nothing

getMinutesBetweenTwoUTChhmmss :: String -> String -> Maybe Int
getMinutesBetweenTwoUTChhmmss time1 time2 = do
  if DS.null time1 || DS.null time2 then Nothing
  else
    case split (Pattern ":") time1 of
      [h1, m1, _] -> do
        hours1 <- fromString h1
        minutes1 <- fromString m1
        case split (Pattern ":") time2 of
          [h2, m2, _] -> do
            hours2 <- fromString h2
            minutes2 <- fromString m2
            let cal1 = hours1 * 60 + minutes1
                cal2 = if (hours2 < hours1) then (hours2 + 24) * 60 + minutes2 else hours2 * 60 + minutes2
            Just $ if cal1 > cal2 then cal1 - cal2 else cal2 - cal1
          _ -> Nothing
      _ -> Nothing
  
otpRule :: Reader.OtpRule
otpRule =
  let config = getAppConfig appConfig
  in
  Reader.OtpRule
    { matches:
        { sender: []
        , message : config.otpRegex
        }
    , otp: "\\d{4}"
    , group: Nothing
    }

startOtpReciever :: forall action. (String -> action) -> (action -> Effect Unit) -> Effect (Effect Unit)
startOtpReciever action push = do
  fiber <-
    launchAff
      $ do
          otpListener <- traverse Readers.getOtpListener $ fromArray [ Readers.smsRetriever ]
          _ <- traverse identity $ (otpListener <#> _.setOtpRules) <*> Just [ otpRule ]
          message <- traverse identity $ (otpListener <#> _.getNextOtp)
          case message of
            Just (Readers.Otp val _ _) -> liftEffect $ push $ action val
            _ -> pure unit
          void $ initiateSMSRetriever
          liftEffect $ startOtpReciever action push
  pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber

derive instance genericTimeUnit :: Generic TimeUnit _

instance showTimeUnit :: Show TimeUnit where
  show = genericShow


fetchRecents :: Decode RecentlySearchedObject => String -> Flow GlobalState (Maybe RecentlySearchedObject)
fetchRecents objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStore' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> fetchOldRecents objName
    Nothing -> pure Nothing

fetchOldRecents :: Decode RecentlySearchedObject => String -> Flow GlobalState (Maybe RecentlySearchedObject)
fetchOldRecents objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStoreTemp' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> do
          _ <- liftFlow (logShow $ "fetchRecentsTemp: Error while decoding " <> (show err))
          pure Nothing
    Nothing -> pure Nothing

getObjFromLocal :: HomeScreenState -> Flow GlobalState RecentlySearchedObject
getObjFromLocal homeScreenState = do
  (recentlySearched :: Maybe RecentlySearchedObject) <- (fetchRecents "RECENT_SEARCHES")
  case recentlySearched of
    Just recents -> pure $ recents{predictionArray =  map (\item -> item{prefixImageUrl = "ny_ic_recent_search," <> (getAssetLink FunctionCall) <> "ny_ic_recent_search.png"}) (recents.predictionArray)}
    Nothing -> pure homeScreenState.data.recentSearchs

getRecentSearches :: AddNewAddressScreenState -> Flow GlobalState RecentlySearchedObject
getRecentSearches addNewAddressScreenState = do
      (recentlySearched :: Maybe RecentlySearchedObject) <- (fetchRecents "RECENT_SEARCHES")
      case recentlySearched of
        Just recents  -> pure recents
        Nothing -> pure addNewAddressScreenState.data.recentSearchs

--------------------------------------------------------------------------------------------------

saveCurrentLocations :: forall s. Serializable s => String -> s -> Flow GlobalState Unit
saveCurrentLocations objName obj =
  doAff do
    (fromEffectFnAff <<< saveToLocalStore' objName $ (serialize obj))

fetchCurrentLocations :: Decode PreviousCurrentLocations => String -> Flow GlobalState (Maybe PreviousCurrentLocations)
fetchCurrentLocations objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStore' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> do
          _ <- liftFlow (logShow $ "fetchCurrentLocations: Error while decoding " <> (show err))
          pure Nothing
    Nothing -> pure Nothing

getCurrentLocationsObjFromLocal :: HomeScreenState -> Flow GlobalState PreviousCurrentLocations
getCurrentLocationsObjFromLocal homeScreenState = do
  (currentLocations :: Maybe PreviousCurrentLocations) <- (fetchCurrentLocations "PREVIOUS_CURRENT_LOCATION")
  case currentLocations of
    Just recents -> pure recents
    Nothing -> pure homeScreenState.data.previousCurrentLocations

checkCurrLoc :: CurrentLocationDetails -> Array CurrentLocationDetails -> Boolean
checkCurrLoc currLoc currLocArr = or ( map (\item -> (getDistanceBwCordinates currLoc.lat currLoc.lon item.lat item.lon < 0.05)) currLocArr)

addToPrevCurrLoc :: CurrentLocationDetails -> Array CurrentLocationDetails -> Array CurrentLocationDetails
addToPrevCurrLoc currLoc currLocArr =
  if (not (checkCurrLoc currLoc currLocArr))
    then if (length currLocArr == 10)
            then (fromMaybe [] (deleteAt 10 (cons currLoc currLocArr)))
            else (cons currLoc currLocArr)
    else currLocArr

 --------------------------------------------------------------------------------------------------
fetchMetroStations :: Decode MetroStations => String -> Flow GlobalState (Maybe (Array MetroStations))
fetchMetroStations objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStore' objName Just Nothing
  void $ pure $ spy "fetchMetroStations: maybeEncodedState" maybeEncodedState
  case maybeEncodedState of
    
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> do
          _ <- liftFlow (logShow $ "fetchMetroStations: Error while decoding " <> (show err))
          pure Nothing
    Nothing -> pure Nothing

getMetroStationsObjFromLocal :: String -> Flow GlobalState (Array MetroStations)
getMetroStationsObjFromLocal _ = do
  (metroStationsList :: Maybe (Array MetroStations)) <- (fetchMetroStations "METRO_STATIONS")
  case metroStationsList of
    Just stations -> pure stations
    Nothing -> pure []

 --------------------------------------------------------------------------------------------------

checkPrediction :: LocationListItemState -> Array LocationListItemState -> Boolean
checkPrediction prediction predictionArr = if (length (filter (\ ( item) -> (item.placeId) == (prediction.placeId))(predictionArr)) > 0) then false else true

getPrediction :: LocationListItemState -> Array LocationListItemState -> LocationListItemState
getPrediction prediction predictionArr = (fromMaybe locationListStateObj ((filter (\ ( item) -> (item.placeId) == (prediction.placeId))(predictionArr)) !! 0))

addSearchOnTop :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addSearchOnTop prediction predictionArr = cons prediction (filter (\ ( item) -> (item.placeId) /= (prediction.placeId))(predictionArr))

addToRecentSearches :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addToRecentSearches prediction predictionArr = 
    let prediction' = prediction {prefixImageUrl = "ny_ic_recent_search," <> (getAssetLink FunctionCall) <> "ny_ic_recent_search.png", locationItemType = Just RECENTS}
      in (if (checkPrediction prediction' predictionArr) 
           then (if length predictionArr == 30 then (fromMaybe [] (deleteAt 30 (cons prediction' predictionArr)))
          else (cons  prediction' predictionArr)) else addSearchOnTop prediction' predictionArr)

differenceOfLocationLists :: Array LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
differenceOfLocationLists arr1 arr2 = filter ( \item1 -> length (filter( \ (item2) -> (item2.placeId == item1.placeId)) arr2) == 0) arr1

filterRecentSearches :: Array LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
filterRecentSearches arr1 arr2 = filter ( \item1 -> length (filter( \ (item2) -> (item2.placeId /= item1.placeId)) arr2) /= (length arr2)) arr1

getDistanceBwCordinates :: Number -> Number -> Number -> Number -> Number
getDistanceBwCordinates lat1 long1 lat2 long2 = do
  let latPoint1 = toRad (lat1)
  let lngPoint1 = toRad (long1)
  let latPoint2 = toRad (lat2)
  let lngPoint2 = toRad (long2)
  let dlong = toRad (long2 -  (long1))
  let lati1 = toRad (lat1)
  let lati2 = toRad (lat2)
  let dist =  sin ((latPoint2 - latPoint1) / 2.0 ) * sin ((latPoint2 - latPoint1) / 2.0 ) + cos(latPoint1) * cos(latPoint2) * sin ((lngPoint2 - lngPoint1) / 2.0 ) * sin ((lngPoint2 - lngPoint1) / 2.0 )
  let dist1 = (2.0 * 6371.0 * asin ( sqrt dist))
  dist1

toRad :: Number -> Number
toRad n = (n * pi) / 180.0

getCurrentLocationMarker :: String -> String
getCurrentLocationMarker currentVersion = "ny_ic_customer_current_location"

rotateArray :: forall a. Array a -> Int -> Array a
rotateArray arr times =
  if times > 0 then case head arr of
    Just ele ->
      rotateArray
        ( ( case tail arr of
              Just tailArray -> tailArray
              Nothing -> []
          )
            <> [ ele ]
        )
        (times - 1)
    Nothing -> arr
  else
    arr

type AffSuccess s = (s -> Effect Unit)

isHaveFare :: String -> Array FareComponent -> Boolean
isHaveFare fare = not null <<< filter (\item -> item.fareType == fare)

sortPredictionByDistance :: Array Prediction -> Array Prediction
sortPredictionByDistance arr = sortBy (comparing (_^._distance_meters)) arr


getDistanceString :: Int -> Int -> String
getDistanceString distanceInMeters decimalPoint
  | distanceInMeters >= 1000 = ReExport.parseFloat (toNumber distanceInMeters / 1000.0) decimalPoint <> " km"
  | otherwise = show distanceInMeters <> " m"

updateLocListWithDistance :: Array LocationListItemState -> Number -> Number -> Boolean -> Number -> Array LocationListItemState 
updateLocListWithDistance arr currLat currLon useThreshold threshold =   
  arr
  # map updateItemDistance
  # filter withinThreshold

  where
    updateItemDistance item = 
      let 
        distance = round $ getDistanceBwCordinates currLat currLon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) * 1000.0
      in 
        item { actualDistance = Just distance, distance = Just $ getDistanceString distance 1 }

    withinThreshold item = 
      maybe true (\actualDist -> (actualDist <= round (threshold * 1000.0) && useThreshold) || not useThreshold) item.actualDistance

getAssetLink :: LazyCheck -> String
getAssetLink lazy = case (getMerchant lazy) of
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/images/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/images/"
  _ -> "https://assets.juspay.in/beckn/nammayatri/user/images/"

getCommonAssetLink :: LazyCheck -> String
getCommonAssetLink lazy = case (getMerchant lazy) of
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/yatricommon/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/passculturecommon/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/mobilityredbuscommon/"
  _ -> "https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/"

getAssetsBaseUrl :: LazyCheck -> String
getAssetsBaseUrl lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/user/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/"
  _ -> "https://assets.juspay.in/beckn/nammayatri/user/"

fetchImage :: FetchImageFrom -> String -> String
fetchImage fetchImageFrom imageName = do
  if imageName  == "" then ","
  else case fetchImageFrom of
    FF_ASSET -> imageName <> "," <> (getAssetLink FunctionCall) <> imageName <> ".png"
    FF_COMMON_ASSET -> imageName <> "," <> (getCommonAssetLink FunctionCall) <> imageName <> ".png"

data FetchImageFrom = FF_ASSET | FF_COMMON_ASSET

derive instance genericFetchImageFrom :: Generic FetchImageFrom _
instance eqFetchImageFrom :: Eq FetchImageFrom where eq = genericEq
instance showFetchImageFrom :: Show FetchImageFrom where show = genericShow
instance encodeFetchImageFrom :: Encode FetchImageFrom where encode = defaultEnumEncode
instance decodeFetchImageFrom :: Decode FetchImageFrom where decode = defaultEnumDecode

showCarouselScreen :: LazyCheck -> Boolean
showCarouselScreen a = if os == "IOS" then not ( isPreviousVersion (getValueToLocalStore VERSION_NAME) "1.3.1" ) && getMerchant FunctionCall == NAMMAYATRI else getMerchant FunctionCall == NAMMAYATRI

terminateApp :: Stage -> Boolean -> Unit
terminateApp stage exitApp = emitTerminateApp (Just $ getScreenFromStage stage) exitApp

emitTerminateApp :: Maybe String -> Boolean -> Unit
emitTerminateApp screen exitApp = runFn3 emitJOSEvent "java" "onEvent" $ encode $  EventPayload {
    event : "process_result"
  , payload : Just {
    action : "terminate"
  , trip_amount : Nothing
  , ride_status : Nothing
  , trip_id : Nothing
  , screen : screen
  , exit_app : exitApp
  }
}

makeNumber :: String -> String
makeNumber number = (DS.take 2 number) <> " " <> (DS.drop 2 (DS.take 4 number)) <> " " <>  reverse' (DS.drop 4 (reverse' (DS.drop 4 number))) <> " " <>  reverse' (DS.take 4 (reverse' number))

reverse' :: String -> String
reverse' = fromCharArray <<< reverse <<< toCharArray

getVehicleSize :: Unit -> Int
getVehicleSize unit = 
  let mapConfig = (getAppConfig appConfig).mapConfig
  in mapConfig.vehicleMarkerSize

getScreenFromStage :: Stage -> String
getScreenFromStage stage = case stage of
  HomeScreen -> "home_screen"
  SettingPrice -> "estimate_screen"
  FindingEstimate -> "finding_driver_loader"
  ConfirmingRide -> "confirm_ride_loader"
  RideAccepted -> "trip_accepted_screen"
  ReAllocated -> "trip_accepted_screen"
  RideStarted -> "trip_started_screen"
  RideCompleted -> "trip_completed_screen"
  PricingTutorial -> "estimate_screen"
  SearchLocationModel -> "search_location_screen"
  FindingQuotes -> "finding_rides_screen"
  QuoteList -> "no_rides_screen"
  PreviousRating -> "previous_ride_rating_screen"
  ConfirmingLocation -> "confirm_location_screen"
  RideRating -> "ride_rating_screen"
  FavouriteLocationModel -> "search_location_screen"
  ChatWithDriver -> "trip_accepted_screen"
  FindEstimateAndSearch -> "finding_rides_screen"
  RetryFindingQuote -> "finding_rides_screen"
  DistanceOutsideLimits -> "finding_driver_loader"
  ShortDistance -> "finding_driver_loader"
  TryAgain -> "finding_rides_screen"
  PickUpFarFromCurrentLocation -> "finding_driver_loader"
  LoadMap -> "map_loader"

getGlobalPayload :: String -> Maybe GlobalPayload
getGlobalPayload key = do
  let mBPayload = runFn3 getFromWindow key Nothing Just
  maybe (Nothing) (\payload -> decodeForeignObjImpl payload) mBPayload

getSearchType :: Unit -> String
getSearchType _ = 
  let mBPayload = getGlobalPayload globalPayload
  in maybe ("normal_search") (\payload -> fromMaybe "normal_search" $ payload ^. _payload ^. _search_type) mBPayload

getDeepLinkOptions :: LazyCheck -> Maybe DeeplinkOptions
getDeepLinkOptions _ = 
  let mBPayload = getGlobalPayload globalPayload
  in maybe Nothing (\payload -> payload ^. _payload ^. _deeplinkOptions) mBPayload

isParentView :: LazyCheck -> Boolean
isParentView lazy = maybe false (\(DeeplinkOptions options) -> fromMaybe false options.parent_view) $ getDeepLinkOptions lazy

showTitle :: LazyCheck -> Boolean
showTitle lazy = maybe true (\(DeeplinkOptions options) -> fromMaybe false options.show_title) $ getDeepLinkOptions lazy 

getPaymentMethod :: Unit -> String
getPaymentMethod _ = 
  let mBPayload = getGlobalPayload globalPayload
  in maybe ("cash") (\payload -> fromMaybe "cash" $ payload ^. _payload ^. _paymentMethod) mBPayload


triggerRideStatusEvent :: String -> Maybe Int -> Maybe String -> String -> Flow GlobalState Unit 
triggerRideStatusEvent status amount bookingId screen = do
  let (payload :: InnerPayload) = { action : "trip_status"
    , ride_status : Just status
    , trip_amount : amount
    , trip_id : bookingId
    , screen : Just screen
    , exit_app : false
    }
  pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
    event : "process_result"
  , payload : Just payload
  }

fetchDefaultPickupPoint :: Array Location -> Number -> Number -> String
fetchDefaultPickupPoint locations lati longi =
  case filter (\loc -> abs(loc.lat - lati) <= 0.0001 && abs(loc.lng - longi) <= 0.0001) locations of
    [foundLocation] -> foundLocation.place
    _ -> ""

getVehicleVariantImage :: String -> String
getVehicleVariantImage variant =
  let variantConfig = (getAppConfig appConfig).estimateAndQuoteConfig.variantInfo
      city = getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
  in 
    case variant of
      "TAXI"          ->  variantConfig.taxi.image 
      "TAXI_PLUS"     -> variantConfig.taxiPlus.image
      "SEDAN"         -> variantConfig.sedan.image
      "SUV"           -> variantConfig.suv.image
      "HATCHBACK"     -> variantConfig.hatchback.image
      "AUTO_RICKSHAW" -> case city of 
                          Kochi -> fetchImage FF_ASSET "ny_ic_single_estimate_auto_black" 
                          Chennai -> fetchImage FF_ASSET "ny_ic_single_estimate_auto_black_yellow" 
                          Hyderabad -> fetchImage FF_ASSET "ny_ic_single_estimate_auto_black_yellow"
                          _ -> variantConfig.autoRickshaw.image
      _               -> fetchImage FF_ASSET "ic_sedan_non_ac"
        
getVariantRideType :: String -> String
getVariantRideType variant =
  case getMerchant FunctionCall of
    YATRISATHI -> case variant of
                    "TAXI" -> getString NON_AC_TAXI
                    "SUV"  -> getString AC_SUV
                    _      -> getString AC_CAB
    _          -> getString AC_CAB

getTitleConfig :: forall w. String -> {text :: String , color :: String}
getTitleConfig vehicleVariant =
  case vehicleVariant of
        "TAXI" -> mkReturnObj ((getString NON_AC )<> " " <> (getString TAXI)) CommonColor.orange900
        "SUV" -> mkReturnObj ((getString AC_SUV )<> " " <> (getString TAXI)) Color.blue800 
        "AUTO_RICKSHAW" -> mkReturnObj ((getString AUTO_RICKSHAW)) Color.green600
        _ -> mkReturnObj ((getString AC) <> " " <> (getString TAXI)) Color.blue800 
  where mkReturnObj text' color' = 
          {
            text : text',
            color : color'
          }

cityCodeMap :: Array (Tuple (Maybe String) City)
cityCodeMap = 
  [ Tuple (Just "std:080") Bangalore
  , Tuple (Just "std:033") Kolkata
  , Tuple (Just "std:001") Paris
  , Tuple (Just "std:484") Kochi
  , Tuple (Just "std:0484") Kochi
  , Tuple (Just "std:011") Delhi
  , Tuple (Just "std:040") Hyderabad
  , Tuple (Just "std:022") Mumbai
  , Tuple (Just "std:044") Chennai
  , Tuple (Just "std:0422") Coimbatore
  , Tuple (Just "std:0413") Pondicherry
  , Tuple (Just "std:08342") Goa
  , Tuple (Just "std:020") Pune
  , Tuple (Just "std:0821") Mysore
  , Tuple (Just "std:0816") Tumakuru
  , Tuple Nothing AnyCity
  ]

getCityFromString :: String -> City
getCityFromString cityString =
  case cityString of 
    "Bangalore" -> Bangalore
    "Kolkata" -> Kolkata
    "Paris" -> Paris
    "Kochi" -> Kochi
    "Delhi" -> Delhi
    "Hyderabad" -> Hyderabad
    "Mumbai" -> Mumbai
    "Chennai" -> Chennai
    "Coimbatore" -> Coimbatore
    "Pondicherry" -> Pondicherry
    "Goa" -> Goa
    "Pune" -> Pune
    "Mysore" -> Mysore
    "Tumakuru" -> Tumakuru
    _ -> AnyCity

getCityNameFromCode :: Maybe String -> City
getCityNameFromCode mbCityCode =
  let 
    cityCodeTuple = find (\ tuple -> (fst tuple) == mbCityCode) cityCodeMap
  in maybe AnyCity (\tuple -> snd tuple) cityCodeTuple

getCityCodeFromCity :: City -> Maybe String
getCityCodeFromCity city =
    let 
      cityCodeTuple = find (\tuple -> (snd tuple) == city) cityCodeMap
    in maybe Nothing (\tuple -> fst tuple) cityCodeTuple

getCard :: CardType -> String 
getCard cardType = case cardType of 
  HOME_TAG -> "Home"
  WORK_TAG -> "Work"
  _ -> ""

getSavedLocationByTag :: Array LocationListItemState -> CardType -> Maybe LocationListItemState
getSavedLocationByTag list tag = 
  find (\item -> item.tag == getCard tag) list

calculateSavedLocDist :: Array LocationListItemState -> String -> Number -> Number -> Array DistInfo
calculateSavedLocDist savedLocs excludeTag lat lon =
  sortBy compareByDistance $ map (\item -> getDistInfo item) $ listAfterExcludedTag excludeTag savedLocs 
  where 
    compareByDistance :: DistInfo -> DistInfo -> Ordering
    compareByDistance a b = compare (a.distanceDiff) (b.distanceDiff)

    getDistInfo :: LocationListItemState -> DistInfo
    getDistInfo item = do 
      let x = getDistanceBwCordinates (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) lat lon
      {locationName : item.tag, distanceDiff : x} 


isValidLocation :: Array LocationListItemState -> String -> String -> Array DistInfo
isValidLocation savedLocations excludeTag placeId = 
  map (\item -> {locationName : item.tag, distanceDiff : 100.0}) validList
  where
    validList :: Array LocationListItemState
    validList = filter (\x -> placeIdExists x.placeId) (listAfterExcludedTag excludeTag savedLocations)

    placeIdExists :: Maybe String -> Boolean
    placeIdExists = maybe false (\item -> not (DS.null placeId) && item == placeId)


listAfterExcludedTag :: String -> Array LocationListItemState -> Array LocationListItemState
listAfterExcludedTag excludeTag = filter (\item -> (DS.toLower item.tag) /= (DS.toLower excludeTag))

getDistInfo savedLoc excludeLocation lat lon placeId = do 
  let distArr = calculateSavedLocDist savedLoc excludeLocation lat lon
      rslt = isValidLocation savedLoc excludeLocation placeId
      placeIdExists = maybe { locationName: "", distanceDiff : 1.0 } identity $ head rslt
      minDist = maybe { locationName: "", distanceDiff : 1.0 } identity $ head distArr
      locExistsAs = case (DS.null placeIdExists.locationName) , minDist.distanceDiff <= 0.020 of
                      false , _ -> placeIdExists.locationName
                      true  , true -> minDist.locationName
                      _ , _ -> ""
      tagExists = not (null rslt) || minDist.distanceDiff <= 0.020 
  {tagExists, locExistsAs}

getExistingTags :: Array LocationListItemState -> Array String 
getExistingTags savedLoc = map (\item -> DS.toLower $ item.tag) savedLoc

getCityConfig :: Array CityConfig -> String -> CityConfig
getCityConfig cityConfigs cityName = do
  fromMaybe defaultCityConfig $ find (\item -> item.cityName == cityName) cityConfigs
  
getDefaultPixelSize :: Int -> Int
getDefaultPixelSize size =
  if os == "IOS" then size
  else let pixels = runFn1 getPixels FunctionCall
           androidDensity = (runFn1 getDeviceDefaultDensity FunctionCall) / defaultDensity
       in ceil $ (toNumber size / pixels) * androidDensity


formatFareType :: String -> String
formatFareType fareType = 
  let str = DS.replace (DS.Pattern "_") (DS.Replacement " ") fareType
  in
  spaceSeparatedPascalCase str
  
specialZoneTagConfig :: ZoneType -> SpecialZoneTagConfig
specialZoneTagConfig zoneType =
  case zoneType of
    SPECIAL_PICKUP -> 
      { icon : "ny_ic_location_pin_white"
      , text : if isLocalStageOn ConfirmingLocation then getString SPECIAL_PICKUP_ZONE else getString SPECIAL_PICKUP_ZONE_RIDE
      , infoPopUpConfig : Just $ { title : getString SPECIAL_PICKUP_ZONE
                                 , primaryText : getString WE_WILL_TRY_TO_CONNECT_YOU_WITH_DRIVER_IN_CLOSEST_PICKUP_ZONE
                                 , secondaryText : getString THIS_PROVIDES_YOU_AN_INSTANT_PICKUP_EXPERIENCE
                                 , primaryButtonText : getString GOT_IT
                                 , icon : "ny_ic_sp_pickup_zone_map" }
      , backgroundColor : Color.green900
      }
    AUTO_BLOCKED -> 
      { icon : "ny_ic_zone_walk"
      , text : getString GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    METRO ->
      { icon : "ny_ic_metro_white"
      , text : getString METRO_RIDE
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    HOTSPOT onSpot ->
      { icon : if onSpot then "ny_ic_zone_walk" else "ny_ic_select_spot"
      , text : getString $ if onSpot then GO_TO_SELECTED_SPOT_FOR_PICKUP else SELECT_POPULAR_SPOT_FOR_HASSLE_FREE_PICKUP
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }
    _ ->
      { icon : "ny_ic_zone_walk"
      , text : getString GO_TO_SELECTED_SPOT_FOR_PICKUP
      , infoPopUpConfig : Nothing
      , backgroundColor : Color.blue800
      }

zoneLabelIcon :: ZoneType -> String
zoneLabelIcon zoneType =
  case zoneType of
    METRO -> "ny_ic_metro_white"
    _ -> ""

transformGeoJsonFeature :: Maybe String -> Array GateInfoFull -> String
transformGeoJsonFeature geoJson gateInfoFulls =
  if DS.null (fromMaybe "" geoJson) && DA.null gateInfoFulls then ""
  else
    AC.stringify $ AE.encodeJson CC.defaultGeoJson { features = geoJsonFeatures }
    where
      geoJsonFeatures :: Array CCT.GeoJsonFeature
      geoJsonFeatures = 
        DA.foldr (\(GateInfoFull gateInfoFull) specialZones -> 
                  case gateInfoFull.geoJson of
                    Just _ -> specialZones <> [createGeoJsonFeature (GateInfoFull gateInfoFull)]
                    Nothing -> specialZones
                 ) [] gateInfoFulls
        <> case geoJson of
              Just geoJson' -> DA.singleton CC.defaultGeoJsonFeature{ geometry = fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson') }
              Nothing -> []

      decodeGeoJsonGeometry :: String -> Maybe CCT.GeoJsonGeometry
      decodeGeoJsonGeometry stringGeometry =
        case (AD.decodeJson =<< ADP.parseJson stringGeometry) of
          Right resp -> Just resp
          Left err   -> Nothing
      
      createGeoJsonFeature :: GateInfoFull -> CCT.GeoJsonFeature
      createGeoJsonFeature (GateInfoFull gateInfoFull) = 
        CC.defaultGeoJsonFeature {
            properties {
                name = gateInfoFull.name
              , defaultDriverExtra = fromMaybe 0 gateInfoFull.defaultDriverExtra
              , canQueueUpOnGate = fromMaybe false gateInfoFull.canQueueUpOnGate
            }
          , geometry = case gateInfoFull.geoJson of
                          Just geoJson -> fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson)
                          Nothing      -> CC.defaultGeoJsonGeometry
        }

findSpecialPickupZone :: Maybe String -> Maybe (Array Location) -> Number -> Number -> Maybe SpecialLocationList
findSpecialPickupZone stringGeoJson gates lat lon =
  case stringGeoJson, gates of
    Just stringGeoJson', Just gates' -> 
      case decodeGeoJson stringGeoJson' of
        Just geoJson -> 
          let gate = DA.find (\gate -> gate.lat == lat && gate.lng == lon) gates'
          in case gate of
                Just gate' -> 
                  let feature = DA.find (\feature -> feature.properties.name == gate'.place) geoJson.features
                  in Just { geoJson : AC.stringify $ AE.encodeJson geoJson{ features = [feature] }, gates : [gate'], locationName : "", category : "", city : "" }
                Nothing -> Nothing
        Nothing -> Nothing
    _, _ -> Nothing

newtype CityMetroConfig = CityMetroConfig { 
    logoImage :: String
  , title :: String
  , mapImage :: String
  , bannerImage :: String
  , bannerBackgroundColor :: String
  , bannerTextColor :: String
  , termsAndConditionsUrl :: String
  , termsAndConditions :: Array String
  , errorPopupTitle :: String
  , showCancelButton :: Boolean
}

getMetroConfigFromAppConfig :: AppConfig -> String -> MetroConfig
getMetroConfigFromAppConfig config city = do
  let cityConfig = find (\cityCfg -> cityCfg.cityName == toLower city) config.metroTicketingConfig
  case cityConfig of
    Nothing -> {
        cityName : ""
      , cityCode : ""
      , metroStationTtl : 10080
      , bookingStartTime : "04:30:00"
      , bookingEndTime : "22:30:00"
     }
    Just cfg -> cfg

getMetroConfigFromCity :: City -> CityMetroConfig
getMetroConfigFromCity city =
  case city of
    Kochi -> mkCityBasedConfig "ny_ic_kochi_metro" (getString TICKETS_FOR_KOCHI_METRO) "ny_ic_kochi_metro_map" "ny_ic_kochi_metro_banner" "#F5FFFF" "#02B0AF" "https://metro-terms.triffy.in/kochi/index.html" [getString KOCHI_METRO_TERM_1 ,getString KOCHI_METRO_TERM_2] (getString KOCHI_METRO_TIME)  true
    Chennai -> mkCityBasedConfig "ny_ic_chennai_metro" (getString TICKETS_FOR_CHENNAI_METRO) "ny_ic_chennai_metro_map" "ny_ic_chennai_metro_banner" "#D8E2FF" "#2250BF" "https://metro-terms.triffy.in/chennai/index.html" [getString CHENNAI_METRO_TERM_2 ,getString CHENNAI_METRO_TERM_1] (getString $ CHENNAI_METRO_TIME "04:30:00" "22:30:00") false
    _ -> mkCityBasedConfig "" "" "" "" "" "" "" [] "" false
  where
    mkCityBasedConfig logoImage title mapImage bannerImage bannerBackgroundColor bannerTextColor termsAndConditionsUrl termsAndConditions errorPopupTitle showCancelButton = 
      CityMetroConfig
        { logoImage
        , title
        , mapImage
        , bannerImage
        , bannerBackgroundColor
        , bannerTextColor
        , termsAndConditionsUrl
        , termsAndConditions
        , errorPopupTitle
        , showCancelButton
        }
