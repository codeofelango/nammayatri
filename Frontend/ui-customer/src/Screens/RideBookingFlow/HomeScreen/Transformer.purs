{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Transformer where

import ConfigProvider
import Data.Eq
import Data.Ord
import Debug
import Engineering.Helpers.LogEvent
import Locale.Utils
import Prelude

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance, _rideRating, _driverName, _computedPrice, _otpCode, _distance, _maxFare, _estimatedFare, _estimateId, _vehicleVariant, _estimateFareBreakup, _title, _price, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _specialLocationTag, _fareProductType, _stopLocation)
import Common.Types.App (LazyCheck(..), Paths)
import Common.Types.App (RideType(..)) as RideType
import Components.ChooseVehicle (Config, config) as ChooseVehicle
import Components.QuoteListItem.Controller (config) as QLI
-- import Components.RideActionModal (estimatedFareView)
import Components.SettingSideBar.Controller (SettingSideBarState, Status(..))
import Control.Monad.Except.Trans (lift)
import Data.Array (mapWithIndex, filter, head, find, foldl)
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Data.Int (toNumber, round)
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), drop, indexOf, length, split, trim, null)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (convertUTCtoISC, getExpiryTime, getCurrentUTC, getMapsLanguageFormat)
import Helpers.Utils (parseFloat, withinTimeRange, isHaveFare, getVehicleVariantImage,fetchImage, FetchImageFrom(..), getSelectedServices, getCityConfig, intersection)
import JBridge (fromMetersToKm, getLatLonFromAddress)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types (EstimateAndQuoteConfig)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM (Visibility(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getValueByComponent, getWard, getVehicleCapacity, getFaresList, getKmMeter, fetchVehicleVariant, getAddressFromBooking)
import Resources.Localizable.EN (getEN)
import Screens.HomeScreen.ScreenData (dummyAddress, dummyLocationName, dummySettingBar, dummyZoneType, dummyRentalBookingConfig)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails, dummyIndividualCard)
import Screens.Types (DriverInfoCard, LocationListItemState, LocItemType(..), LocationItemType(..), NewContacts, Contact, VehicleVariant(..), TripDetailsScreenState, EstimateInfo, SpecialTags, ZoneType(..), HomeScreenState(..), MyRidesScreenState(..), Trip(..), QuoteListItemState(..), City(..),Stage(..))
import Services.API (AddressComponents(..), BookingLocationAPIEntity(..), DeleteSavedLocationReq(..), DriverOfferAPIEntity(..), EstimateAPIEntity(..), GetPlaceNameResp(..), LatLong(..), OfferRes, OfferRes(..), PlaceName(..), Prediction, QuoteAPIContents(..), QuoteAPIEntity(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingRes(..), SavedReqLocationAPIEntity(..), SpecialZoneQuoteAPIDetails(..), FareRange(..), LatLong(..), EstimateFares(..))
import Services.Backend as Remote
import Storage (isLocalStageOn)
import Storage (setValueToLocalStore, getValueToLocalStore, KeyStore(..))
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Common.Types.App as CT
import Screens.RideBookingFlow.HomeScreen.Config (getTipConfig)
import Data.Foldable (maximum)

getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList prediction = map (\x -> getLocation x) prediction

getLocation :: Prediction -> LocationListItemState
getLocation prediction = {
    prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
  , postfixImageUrl : fetchImage FF_ASSET "ny_ic_fav"
  , postfixImageVisibility : true
  , title : (fromMaybe "" ((split (Pattern ",") (prediction ^. _description)) DA.!! 0))
  , subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (prediction ^. _description))) + 2) (prediction ^. _description))
  , placeId : prediction ^._place_id
  , lat : Nothing
  , lon : Nothing
  , description : prediction ^. _description
  , tag : ""
  , tagType : Just $ show LOC_LIST
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : dummyAddress
  , locationItemType : Just PREDICTION
  , distance : Just (fromMetersToKm (fromMaybe 0 (prediction ^._distance)))
  , showDistance : Just $ checkShowDistance (fromMaybe 0 (prediction ^._distance))
  , actualDistance : (prediction ^._distance)
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
}

checkShowDistance :: Int ->  Boolean
checkShowDistance distance = (distance > 0 && distance <= 50000)

getQuoteList :: Array QuoteAPIEntity -> City -> Array QuoteListItemState
getQuoteList quotesEntity city = (map (\x -> (getQuote x city)) quotesEntity)

getQuote :: QuoteAPIEntity -> City -> QuoteListItemState
getQuote (QuoteAPIEntity quoteEntity) city = do
  case (quoteEntity.quoteDetails)^._contents of
    (ONE_WAY contents) -> QLI.config
    (SPECIAL_ZONE contents) -> QLI.config
    (DRIVER_OFFER contents) -> 
      let (DriverOfferAPIEntity quoteDetails) = contents
          expiryTime = (getExpiryTime quoteDetails.validTill isForLostAndFound) -4
      in {  seconds : expiryTime
          , id : quoteEntity.id
          , timer : show expiryTime
          , timeLeft : quoteDetails.durationToPickup/60
          , driverRating : fromMaybe 0.0 quoteDetails.rating
          , profile : ""
          , price :  show quoteEntity.estimatedTotalFare
          , vehicleType : quoteEntity.vehicleVariant
          , driverName : quoteDetails.driverName
          , selectedQuote : Nothing
          , appConfig : getAppConfig appConfig
          , city : city
        }
    (RENTAL contents) -> QLI.config
    (INTER_CITY contents) -> QLI.config
    
getDriverInfo :: Maybe String -> RideBookingRes -> Boolean -> DriverInfoCard
getDriverInfo vehicleVariant (RideBookingRes resp) isQuote =
  let (RideAPIEntity rideList) = fromMaybe  dummyRideAPIEntity ((resp.rideList) DA.!! 0)
      fareProductType = resp.bookingDetails ^._fareProductType
      stopLocation = if fareProductType == "RENTAL" then _stopLocation else _toLocation
      (BookingLocationAPIEntity toLocation) = fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)
  in  {
        otp : if isQuote then fromMaybe "" ((resp.bookingDetails)^._contents ^._otpCode) else if (((fareProductType == "RENTAL")|| (fareProductType == "INTER_CITY")) && isLocalStageOn RideStarted) then fromMaybe "" rideList.endOtp else rideList.rideOtp
      , driverName : if length (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) < 4 then
                        (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0)) <> " " <> (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 1)) else
                          (fromMaybe "" ((split (Pattern " ") (rideList.driverName)) DA.!! 0))
      , eta : Nothing
      , currentSearchResultType : if isQuote then CT.QUOTES CT.OneWaySpecialZoneAPIDetails else CT.ESTIMATES
      , vehicleDetails : rideList.vehicleModel
      , registrationNumber : rideList.vehicleNumber
      , rating : (fromMaybe 0.0 rideList.driverRatings)
      , startedAt : (convertUTCtoISC resp.createdAt "h:mm A")
      , endedAt : (convertUTCtoISC resp.updatedAt "h:mm A")
      , source : decodeAddress (Booking resp.fromLocation)
      , destination : decodeAddress (Booking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation)))
      , rideId : rideList.id
      , price : resp.estimatedTotalFare
      , sourceLat : resp.fromLocation ^._lat
      , sourceLng : resp.fromLocation ^._lon
      , destinationLat : (toLocation.lat)
      , destinationLng : (toLocation.lon)
      , sourceAddress : getAddressFromBooking resp.fromLocation
      , destinationAddress : getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.stopLocation))
      , estimatedDistance : parseFloat ((toNumber (fromMaybe 0 (resp.bookingDetails ^._contents ^._estimatedDistance)))/1000.0) 2
      , createdAt : resp.createdAt
      , driverLat : 0.0
      , driverLng : 0.0
      , distance : 0
      , waitingTime : "--"
      , driverArrived : false
      , driverArrivalTime : 0
      , bppRideId : rideList.bppRideId
      , driverNumber : rideList.driverNumber
      , merchantExoPhone : resp.merchantExoPhone
      , initDistance : Nothing
      , config : getAppConfig appConfig
      -- , providerName : resp.agencyName
      -- , providerType : CT.ONUS -- maybe CT.ONUS (\valueAdd -> if valueAdd then CT.ONUS else CT.OFFUS) resp.isValueAddNP -- get from API
      , vehicleVariant : if rideList.vehicleVariant /= "" 
                            then rideList.vehicleVariant 
                         else
                            fromMaybe "" vehicleVariant
      , rentalData : dummyRentalBookingConfig{
          baseDistance = (fromMaybe 20000 resp.estimatedDistance)/1000
        , baseDuration = (fromMaybe 7200 resp.estimatedDuration)/3600
        , startTimeUTC = fromMaybe "" resp.rideStartTime
        }
        }

encodeAddressDescription :: String -> String -> Maybe String -> Maybe Number -> Maybe Number -> Array AddressComponents -> SavedReqLocationAPIEntity
encodeAddressDescription address tag placeId lat lon addressComponents = do
    let totalAddressComponents = DA.length $ split (Pattern ", ") address
        splitedAddress = split (Pattern ", ") address

    SavedReqLocationAPIEntity{
                    "area": (splitedAddress DA.!!(totalAddressComponents-4) ),
                    "areaCode": Just (getValueByComponent addressComponents "postal_code") ,
                    "building": (splitedAddress DA.!!(totalAddressComponents-6) ),
                    "city": (splitedAddress DA.!!(totalAddressComponents-3) ),
                    "country": (splitedAddress DA.!!(totalAddressComponents-1) ),
                    "state" : (splitedAddress DA.!!(totalAddressComponents-2) ),
                    "door": if totalAddressComponents > 7  then (splitedAddress DA.!!0 ) <>(splitedAddress DA.!!1) else if totalAddressComponents == 7 then (splitedAddress DA.!!0 ) else  Just "",
                    "street": (splitedAddress DA.!!(totalAddressComponents-5) ),
                    "lat" : (fromMaybe 0.0 lat),
                    "lon" : (fromMaybe 0.0 lon),
                    "tag" : tag,
                    "placeId" : placeId,
                    "ward" : if DA.null addressComponents then
                        getWard Nothing (splitedAddress DA.!! (totalAddressComponents - 4)) (splitedAddress DA.!! (totalAddressComponents - 5)) (splitedAddress DA.!! (totalAddressComponents - 6))
                      else
                        Just $ getValueByComponent addressComponents "sublocality"
                }


dummyRideAPIEntity :: RideAPIEntity
dummyRideAPIEntity = RideAPIEntity{
  computedPrice : Nothing,
  status : "",
  vehicleModel : "",
  createdAt : "",
  driverNumber : Nothing,
  shortRideId : "",
  driverRegisteredAt : "",
  vehicleNumber : "",
  rideOtp : "",
  driverName : "",
  chargeableRideDistance : Nothing,
  vehicleVariant : "",
  driverRatings : Nothing,
  vehicleColor : "",
  id : "",
  updatedAt : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  rideRating : Nothing,
  driverArrivalTime : Nothing,
  bppRideId : "",
  endOtp : Nothing,
  startOdometerReading : Nothing,
  endOdometerReading : Nothing
  }

isForLostAndFound :: Boolean
isForLostAndFound = false



getPlaceNameResp :: String -> Maybe String -> Number -> Number -> LocationListItemState -> FlowBT String GetPlaceNameResp
getPlaceNameResp address placeId lat lon item = do
  case item.locationItemType of
    Just PREDICTION -> getPlaceNameRes
    _ -> checkLatLon
  where
    getPlaceNameRes :: FlowBT String GetPlaceNameResp
    getPlaceNameRes =
      case placeId of
        Just placeID  -> checkLatLonFromAddress placeID
        Nothing       ->  pure $ makePlaceNameResp lat lon
    
    checkLatLonFromAddress :: String -> FlowBT String GetPlaceNameResp
    checkLatLonFromAddress placeID = do
      let {latitude, longitude} = runFn1 getLatLonFromAddress address
      config <- getAppConfigFlowBT appConfig
      logField_ <- lift $ lift $ getLogFields
      if latitude /= 0.0 && longitude /= 0.0 && config.geoCoder.enableAddressToLL then do
        void $ liftFlowBT $ logEvent logField_ "ny_geocode_address_ll_found"
        pure $ makePlaceNameResp latitude longitude
      else do
        void $ liftFlowBT $ logEvent logField_ "ny_geocode_address_ll_fallback"
        Remote.placeNameBT (Remote.makePlaceNameReqByPlaceId placeID $ getMapsLanguageFormat $ getLanguageLocale languageKey)
    
    checkLatLon :: FlowBT String GetPlaceNameResp
    checkLatLon = 
      case item.lat, item.lon of
        Nothing, Nothing -> getPlaceNameRes
        Just 0.0, Just 0.0 -> getPlaceNameRes
        _ , _ -> pure $ makePlaceNameResp lat lon

makePlaceNameResp :: Number ->  Number -> GetPlaceNameResp
makePlaceNameResp lat lon =
  GetPlaceNameResp
  ([  PlaceName {
          formattedAddress : "",
          location : LatLong {
            lat : lat,
            lon : lon
          },
          plusCode : Nothing,
          addressComponents : [],
          placeId : Nothing
        }
        ])

getUpdatedLocationList :: Array LocationListItemState -> Maybe String -> Array LocationListItemState
getUpdatedLocationList locationList placeId = (map
                            (\item ->
                                ( item  
                                  { postfixImageUrl = 
                                      if (  item.placeId == placeId 
                                          || item.postfixImageUrl == "ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ic_fav_red.png" 
                                          || item.postfixImageUrl == "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" ) 
                                        then "ny_ic_fav_red,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_fav_red.png" 
                                        else "ic_fav,https://assets.juspay.in/beckn/nammayatri/user/images/ic_fav.png" } )
                            ) (locationList))

transformSavedLocations :: Array LocationListItemState -> FlowBT String Unit
transformSavedLocations array = case DA.head array of
            Just item -> do
              case item.lat , item.lon , item.fullAddress.ward of
                Just 0.0 , Just 0.0 , Nothing ->
                  updateSavedLocation item 0.0 0.0
                Just 0.0 , Just 0.0 , Just _ ->
                  updateSavedLocation item 0.0 0.0
                Just lat , Just lon , Nothing ->
                  updateSavedLocation item lat lon
                Nothing, Nothing, Nothing ->
                  updateSavedLocation item 0.0 0.0
                _ , _ , _-> pure unit
              transformSavedLocations (DA.drop 1 array)
            Nothing -> pure unit

updateSavedLocation :: LocationListItemState -> Number -> Number -> FlowBT String Unit
updateSavedLocation item lat lon = do
  let placeId = item.placeId
      address = item.description
      tag = item.tag
  resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim item.tag))
  (GetPlaceNameResp placeNameResp) <- getPlaceNameResp item.address item.placeId lat lon item
  let (PlaceName placeName) = (fromMaybe dummyLocationName (placeNameResp DA.!! 0))
  let (LatLong placeLatLong) = (placeName.location)
  _ <- Remote.addSavedLocationBT (encodeAddressDescription address tag (item.placeId) (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents)
  _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
  pure unit

transformContactList :: Array NewContacts -> Array Contact
transformContactList contacts = map (\x -> getContact x) contacts

getContact :: NewContacts -> Contact
getContact contact = {
    name : contact.name
  , phoneNo : contact.number
}

getSpecialZoneQuotes :: Array OfferRes -> EstimateAndQuoteConfig -> Array ChooseVehicle.Config
getSpecialZoneQuotes quotes estimateAndQuoteConfig = mapWithIndex (\index item -> getSpecialZoneQuote item index) (getFilteredQuotes quotes estimateAndQuoteConfig)

getSpecialZoneQuote :: OfferRes -> Int -> ChooseVehicle.Config
getSpecialZoneQuote quote index =
  let estimatesConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      _ = spy "quoteee" quote
  in 
  case quote of
    Quotes body -> let (QuoteAPIEntity quoteEntity) = body.onDemandCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = false -- estimatesConfig.showInfoIcon
      , searchResultType = CT.QUOTES CT.OneWaySpecialZoneAPIDetails
      , pickUpCharges = 0
      , serviceTierName = quoteEntity.serviceTierName
      -- , serviceTierShortDesc = quoteEntity.serviceTierShortDesc
      -- , airConditioned = Nothing
      , providerType = CT.ONUS -- maybe CT.OFFUS (\valueAdd -> if valueAdd then CT.ONUS else CT.OFFUS) quoteEntity.isValueAddNP
      , specialLocationTag = quoteEntity.specialLocationTag
      }
    RentalQuotes body -> let (QuoteAPIEntity quoteEntity) = body.onRentalCab
      in ChooseVehicle.config {
        vehicleImage = getVehicleVariantImage quoteEntity.vehicleVariant
      , isSelected = (index == 0)
      , vehicleVariant = quoteEntity.vehicleVariant
      , price = (getCurrency appConfig) <> (show quoteEntity.estimatedTotalFare)
      , activeIndex = 0
      , index = index
      , id = trim quoteEntity.id
      , capacity = getVehicleCapacity quoteEntity.vehicleVariant
      , showInfo = estimatesConfig.showInfoIcon
      , searchResultType = CT.QUOTES CT.RENTAL
      , pickUpCharges = 0
      }
    Metro body -> ChooseVehicle.config
    Public body -> ChooseVehicle.config

isFareRangePresent :: Array EstimateAPIEntity -> Boolean
isFareRangePresent estimates = DA.length (DA.filter (\(EstimateAPIEntity estimate) ->
         case estimate.totalFareRange of
                Nothing -> false
                Just (FareRange fareRange) -> not (fareRange.minFare == fareRange.maxFare )) estimates) > 0

getFilteredEstimate :: Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Array EstimateAPIEntity
getFilteredEstimate estimates estimateAndQuoteConfig =
  let filteredEstimate = case (getMerchant FunctionCall) of
                            YATRISATHI -> DA.concat (map (\variant -> filterEstimateByVariants variant estimates) (estimateAndQuoteConfig.variantTypes :: Array (Array String)))
                            _          -> estimates
      sortWithFare = DA.sortWith (\(EstimateAPIEntity estimate) -> getFareFromEstimate (EstimateAPIEntity estimate)) filteredEstimate
  in sortEstimateWithVariantOrder sortWithFare estimateAndQuoteConfig.variantOrder

sortEstimateWithVariantOrder :: Array EstimateAPIEntity -> Array String -> Array EstimateAPIEntity
sortEstimateWithVariantOrder estimates orderList =
  let orderListLength = DA.length orderList
      mappedEstimates = map (\(EstimateAPIEntity estimate) -> 
                          let orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex estimate.vehicleVariant orderList)
                          in {item : (EstimateAPIEntity estimate), order : orderNumber}
                        ) estimates
      sortedEstimates = DA.sortWith (\mappedEstimate -> mappedEstimate.order) mappedEstimates
  in map (\sortedEstimate -> sortedEstimate.item) sortedEstimates

getEstimateList :: Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Int -> Array ChooseVehicle.Config
getEstimateList estimates estimateAndQuoteConfig activeIndex = 
  let estimatesWithOrWithoutBookAny = (createEstimateForBookAny estimates) <> estimates
      filteredWithVariantAndFare = filterWithFareAndVariant estimatesWithOrWithoutBookAny estimateAndQuoteConfig
      estimatesConfig = mapWithIndex (\index item -> getEstimates item filteredWithVariantAndFare index activeIndex) filteredWithVariantAndFare
  in
    updateBookAnyEstimate estimatesConfig 

filterWithFareAndVariant :: Array EstimateAPIEntity -> EstimateAndQuoteConfig -> Array EstimateAPIEntity
filterWithFareAndVariant estimates estimateAndQuoteConfig =
  let
    filteredEstimate = 
      case (getMerchant FunctionCall) of
        YATRISATHI -> DA.concat (map (\variant -> filterEstimateByVariants variant estimates) (estimateAndQuoteConfig.variantTypes :: Array (Array String)))
        _ -> estimates
    sortWithFare = DA.sortWith (\(EstimateAPIEntity estimate) -> getFareFromEstimate (EstimateAPIEntity estimate)) filteredEstimate
  in
    sortEstimateWithVariantOrder sortWithFare estimateAndQuoteConfig.variantOrder
  where
    sortEstimateWithVariantOrder :: Array EstimateAPIEntity -> Array String -> Array EstimateAPIEntity
    sortEstimateWithVariantOrder estimates orderList =
      let orderListLength = DA.length orderList
          mappedEstimates =
            map
              ( \(EstimateAPIEntity estimate) ->
                  let
                    orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex estimate.vehicleVariant orderList)
                    isNY = if estimate.isValueAddNP == Just true then 0 else 1
                  in
                    { item: (EstimateAPIEntity estimate), order: orderNumber * 10 + isNY }
              )
              estimates
          sortedEstimates = DA.sortWith (\mappedEstimate -> mappedEstimate.order) mappedEstimates
      in
          map (\sortedEstimate -> sortedEstimate.item) sortedEstimates

filterEstimateByVariants :: Array String -> Array EstimateAPIEntity -> Array EstimateAPIEntity
filterEstimateByVariants variant estimates = DA.take 1 (sortEstimateWithVariantOrder (DA.filter (\(EstimateAPIEntity item) -> DA.any (_ == item.vehicleVariant) variant) estimates) variant)


getFareFromEstimate :: EstimateAPIEntity -> Int
getFareFromEstimate (EstimateAPIEntity estimate) = do
  case estimate.totalFareRange of
    Nothing -> estimate.estimatedTotalFare
    Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then estimate.estimatedTotalFare
                                                      else fareRange.minFare


getFilteredQuotes :: Array OfferRes -> EstimateAndQuoteConfig -> Array OfferRes
getFilteredQuotes quotes estimateAndQuoteConfig =
  let filteredArray = (case (getMerchant FunctionCall) of
                          YATRISATHI -> DA.concat (map (\variant -> filterQuoteByVariants variant quotes) (estimateAndQuoteConfig.variantTypes :: Array (Array String)))
                          _ -> quotes)
  in sortQuoteWithVariantOrder filteredArray estimateAndQuoteConfig.variantOrder
  where
    sortQuoteWithVariantOrder :: Array OfferRes -> Array String -> Array OfferRes
    sortQuoteWithVariantOrder quotes orderList =
      let orderListLength = DA.length orderList
          mappedQuotes =
            map
              ( \quote -> case quote of
                  Quotes body ->
                    let
                      (QuoteAPIEntity quoteEntity) = body.onDemandCab
                      orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex quoteEntity.vehicleVariant orderList)
                    in
                      { item: Just quote, order: orderNumber }
                  RentalQuotes body -> 
                    let (QuoteAPIEntity quoteEntity) = body.onRentalCab
                        orderNumber = fromMaybe (orderListLength + 1) (DA.elemIndex quoteEntity.vehicleVariant orderList)
                    in {item : Just quote, order : orderNumber}
                  _ -> {item : Nothing, order : orderListLength}
              )
              quotes
          filterMappedQuotes = filter (\quote -> isJust quote.item) mappedQuotes
          sortedQuotes = DA.sortWith (\mappedQuote -> mappedQuote.order) filterMappedQuotes
      in
        DA.catMaybes $ map (\sortedEstimate -> sortedEstimate.item) sortedQuotes

    filterQuoteByVariants :: Array String -> Array OfferRes -> Array OfferRes
    filterQuoteByVariants variant quotes =
      DA.take 1
        ( sortQuoteWithVariantOrder
            ( DA.filter
                ( \item -> case item of
                    Quotes body -> do
                      let
                        (QuoteAPIEntity quoteEntity) = body.onDemandCab
                      DA.any (_ == quoteEntity.vehicleVariant) variant
                    RentalQuotes body -> do
                      let (QuoteAPIEntity quoteEntity) = body.onRentalCab
                      DA.any (_ == quoteEntity.vehicleVariant) variant
                    _ -> false
                )
                quotes
            )
            variant
        )

getEstimates :: EstimateAPIEntity -> Array EstimateAPIEntity -> Int -> Int -> ChooseVehicle.Config
getEstimates (EstimateAPIEntity estimate) estimates index activeIndex =
  let currency = getCurrency appConfig
      estimateAndQuoteConfig = (getAppConfig appConfig).estimateAndQuoteConfig
      config = getCityConfig (getAppConfig appConfig).cityConfig (getValueToLocalStore CUSTOMER_LOCATION)
      tipConfig = getTipConfig estimate.vehicleVariant
      maxTip = fromMaybe 0 (maximum tipConfig.customerTipArrayWithValues)
      -- breakupConfig = getFareBreakupList (EstimateAPIEntity estimate) maxTip
      additionalFare = maybe 20 calculateFareRangeDifference (estimate.totalFareRange)
      extractFare f = case estimate.totalFareRange of
                        Just (FareRange fareRange) -> Just (f fareRange)
                        _ -> Nothing
      calculateFareRangeDifference fareRange = fareRange ^. _maxFare - fareRange ^. _minFare
  in
    ChooseVehicle.config
      { vehicleImage = getVehicleVariantImage estimate.vehicleVariant
      , vehicleVariant = estimate.vehicleVariant
      , price = case estimate.totalFareRange of
                Nothing -> currency <> (show estimate.estimatedTotalFare)
                Just (FareRange fareRange) -> if fareRange.minFare == fareRange.maxFare then currency <> (show estimate.estimatedTotalFare)
                                              else  currency <> (show fareRange.minFare) <> " - " <> currency <> (show fareRange.maxFare)
      , activeIndex = 0
      , index = index
      , id = trim estimate.id
      , capacity = getVehicleCapacity estimate.vehicleVariant
      , showInfo = estimateAndQuoteConfig.showInfoIcon
      , basePrice = estimate.estimatedTotalFare
      , searchResultType = CT.ESTIMATES
      , serviceTierName =  mapServiceTierName estimate.vehicleVariant estimate.isValueAddNP estimate.serviceTierName
      -- , serviceTierShortDesc = mapServiceTierShortDesc estimate.vehicleVariant estimate.isValueAddNP estimate.serviceTierShortDesc
      -- , extraFare = breakupConfig.fareList
      -- , additionalFare = additionalFare
      , providerName = fromMaybe "" estimate.providerName
      , providerId = fromMaybe "" estimate.providerId
      , providerType = CT.ONUS -- maybe CT.OFFUS (\valueAdd -> if valueAdd then CT.ONUS else CT.OFFUS) estimate.isValueAddNP
      , maxPrice = extractFare _.maxFare
      -- , minPrice = extractFare _.minFare
      , priceShimmer = false
      -- , fareInfoDescription = breakupConfig.fareInfo
      -- , isNightShift = breakupConfig.isNightShift
      -- , nightChargeFrom = breakupConfig.nightChargeStart
      -- , nightChargeTill = breakupConfig.nightChargeEnd
      -- , driverAdditions = breakupConfig.driverAdditions
      , availableServices = []
      , selectedServices = []
      , validTill = estimate.validTill
      , specialLocationTag = estimate.specialLocationTag
      }

getEstimateIdFromSelectedServices :: Array ChooseVehicle.Config -> ChooseVehicle.Config -> Array String
getEstimateIdFromSelectedServices estimates config =
  foldl (\acc item -> if DA.elem (fromMaybe "" item.serviceTierName) config.selectedServices 
                        then acc <> [item.id] 
                        else acc
        ) [] estimates

updateBookAnyEstimate :: Array ChooseVehicle.Config -> Array ChooseVehicle.Config
updateBookAnyEstimate estimates =
    map
      ( \estimate -> 
          if estimate.vehicleVariant == "BOOK_ANY" then
            let availableServices = foldl
                                      ( \acc item -> case item.serviceTierName of
                                          Just service -> acc <> [ service ]
                                          Nothing -> acc
                                      )
                                      []
                                      estimates
                allSelectedServices = getSelectedServices FunctionCall
                selectedServices = intersection allSelectedServices availableServices
                headEstimateId = (fromMaybe ChooseVehicle.config (DA.find (\item -> DA.any (_ == fromMaybe "" item.serviceTierName) selectedServices) estimates)).id
                validTill = (fromMaybe ChooseVehicle.config (DA.find (\item -> item.id == headEstimateId) estimates)).validTill
            in estimate { availableServices = availableServices
                        , selectedServices = selectedServices
                        , validTill = validTill
                        , id = headEstimateId
                        }
          else
            estimate
      )
      estimates

mapServiceTierName :: String -> Maybe Boolean -> Maybe String -> Maybe String
mapServiceTierName vehicleVariant isValueAddNP serviceTierName = 
  case isValueAddNP of
    Just true -> serviceTierName -- NY Service Tier Name
    _ -> case vehicleVariant of
      "HATCHBACK" -> Just "Non - AC Mini"
      "SEDAN" -> Just "Sedan"
      "SUV" -> Just "XL Cab"
      "AUTO_RICKSHAW" -> Just "Auto"
      _ -> serviceTierName

mapServiceTierShortDesc :: String -> Maybe Boolean -> Maybe String -> Maybe String
mapServiceTierShortDesc vehicleVariant isValueAddNP serviceTierShortDesc = 
  case isValueAddNP of
    Just true -> serviceTierShortDesc -- NY Service Tier Short Desc
    _ -> case vehicleVariant of
      "HATCHBACK" -> Just "Budget friendly"
      "SEDAN" -> Just "AC, Premium Comfort"
      "SUV" -> Just "AC, Extra Spacious"
      "AUTO_RICKSHAW" -> Just "Easy Commute"
      _ -> serviceTierShortDesc

getTripDetailsState :: RideBookingRes -> TripDetailsScreenState -> TripDetailsScreenState
getTripDetailsState (RideBookingRes ride) state = do
  let (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0))
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)
      estimatedDistance = ride.estimatedDistance
      baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
      updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
      (RideBookingAPIDetails bookingDetails) = ride.bookingDetails
      rideType = case bookingDetails.fareProductType of
                    "RENTAL" -> RideType.RENTAL_RIDE
                    "INTER_CITY" -> RideType.INTERCITY
                    _ -> RideType.NORMAL_RIDE
  state {
    data {
      tripId = rideDetails.shortRideId,
      date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
      time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
      source= decodeAddress (Booking ride.fromLocation),
      destination= (decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^._contents^._toLocation)))),
      rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
      driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName) ,
      totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
      selectedItem = dummyIndividualCard{
        status = ride.status,
        rideType = rideType,
        estimatedDistance = fromMaybe 0 estimatedDistance,
        faresList = getFaresList ride.fareBreakup (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance))),
        rideId = rideDetails.id,
        date = (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
        time = (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
        source= decodeAddress (Booking ride.fromLocation),
        destination= (decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^._contents^._toLocation)))),
        rating= (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _rideRating)),
        driverName =((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _driverName),
        rideStartTime = (convertUTCtoISC (fromMaybe "" rideDetails.rideStartTime ) "h:mm A"),
        rideEndTime = (convertUTCtoISC (fromMaybe "" rideDetails.rideEndTime) "h:mm A"),
        vehicleNumber = rideDetails.vehicleNumber,
        totalAmount = ("₹ " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList DA.!!0) )^. _computedPrice))),
        shortRideId = rideDetails.shortRideId,
        baseDistance = baseDistanceVal,
        referenceString = (if (nightChargesVal && (getMerchant FunctionCall) /= YATRI) then "1.5" <> (getEN DAYTIME_CHARGES_APPLICABLE_AT_NIGHT) else "")
                        <> (if (isHaveFare "DRIVER_SELECTED_FARE" (updatedFareList)) then "\n\n" <> (getEN DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO) else "")
                        <> (if (isHaveFare "WAITING_OR_PICKUP_CHARGES" updatedFareList) then "\n\n" <> (getEN WAITING_CHARGE_DESCRIPTION) else "")
                        <> (if (isHaveFare "EARLY_END_RIDE_PENALTY" (updatedFareList)) then "\n\n" <> (getEN EARLY_END_RIDE_CHARGES_DESCRIPTION) else "")
                        <> (if (isHaveFare "CUSTOMER_SELECTED_FARE" ((updatedFareList))) then "\n\n" <> (getEN CUSTOMER_TIP_DESCRIPTION) else ""),
        merchantExoPhone = ride.merchantExoPhone
      },
      vehicleVariant = fetchVehicleVariant rideDetails.vehicleVariant
    }
  }


getNearByDrivers :: Array EstimateAPIEntity -> Array Paths
getNearByDrivers estimates = DA.nub (getCoordinatesFromEstimates [] estimates)
  where
    getCoordinatesFromEstimates :: Array Paths -> Array EstimateAPIEntity -> Array Paths
    getCoordinatesFromEstimates paths [] = paths
    getCoordinatesFromEstimates paths estimates =
      let firstItem = estimates DA.!! 0
          remainingItem = DA.drop 1 estimates
      in
        case firstItem of
          Just estimate -> getCoordinatesFromEstimates (paths <> (getCoordinatesFromEstimate estimate)) remainingItem
          Nothing       -> paths

    getCoordinatesFromEstimate :: EstimateAPIEntity -> Array Paths
    getCoordinatesFromEstimate (EstimateAPIEntity estimate) =
      let latLngs = estimate.driversLatLong
      in
        map (\(LatLong item) -> { lat : item.lat, lng : item.lon }) latLngs

-- getEstimatesInfo :: Array EstimateAPIEntity -> String -> HomeScreenState -> EstimateInfo
-- getEstimatesInfo estimates vehicleVariant state =
--   { additionalFare: additionalFare
--   , estimatedPrice: estimatedPrice
--   , quoteList: quoteList
--   , defaultQuote: defaultQuote
--   , estimateId: estimateId
--   , pickUpCharges: pickUpCharges
--   , estimatedVarient: estimatedVariant
--   , nightShiftMultiplier: nightShiftMultiplier
--   , nightCharges: nightCharges
--   , baseFare: baseFare
--   , extraFare: extraFare
--   , showRateCardIcon: showRateCardIcon
--   , zoneType: zoneType
--   }
--   where
--     estimatedVariant = 
--       if null vehicleVariant 
--       then estimates 
--       else filter (\estimate -> estimate ^. _vehicleVariant == vehicleVariant) estimates

--     estimatedPrice = maybe 0 (view _estimatedFare) (head estimatedVariant)
--     quoteList = getEstimateList estimates state.data.config.estimateAndQuoteConfig
--     defaultQuote = fromMaybe ChooseVehicle.config (head quoteList)
--     estimateId = maybe "" (view _estimateId) (head estimatedVariant)
--     estimateFareBreakup = maybe [] identity (head estimatedVariant >>= view _estimateFareBreakup)
--     pickUpCharges = fetchPickupCharges estimateFareBreakup 

--     additionalFare = maybe 20 calculateFareRangeDifference (head estimatedVariant >>= view _totalFareRange)
--     calculateFareRangeDifference fareRange = fareRange ^. _maxFare - fareRange ^. _minFare

--     nightShiftRate = head estimates >>= view _nightShiftRate
--     nightShiftStart = maybe "" (view _nightShiftStart >>> fromMaybe "") nightShiftRate
--     nightShiftEnd = maybe "" (view _nightShiftEnd >>> fromMaybe "") nightShiftRate
--     nightShiftMultiplier = maybe 0.0 (view _nightShiftMultiplier >>> fromMaybe 0.0) nightShiftRate
--     nightCharges = withinTimeRange nightShiftStart nightShiftEnd (convertUTCtoISC(getCurrentUTC "") "HH:mm:ss")

--     baseFare = maybe 0 calculateBaseFare (find hasBaseDistanceFare estimateFareBreakup)
--     hasBaseDistanceFare item = item ^. _title == "BASE_DISTANCE_FARE"
--     calculateBaseFare baseDistFare = round $ (toNumber $ baseDistFare ^. _price) * fareMultiplier
--     fareMultiplier = if nightCharges then nightShiftMultiplier else 1.0

--     extraFare = maybe 0 calculateExtraFare (find hasExtraPerKmFare estimateFareBreakup)
--     hasExtraPerKmFare item = item ^. _title == "EXTRA_PER_KM_FARE"
--     calculateExtraFare extraPerKmFare = round $ (toNumber $ extraPerKmFare ^. _price) * fareMultiplier

--     showRateCardIcon = not (DA.null estimateFareBreakup)
--     zoneType = getSpecialTag $ case head estimatedVariant of
--                   Just entity -> view _specialLocationTag entity
--                   Nothing -> Nothing


dummyEstimateEntity :: EstimateAPIEntity
dummyEstimateEntity =
  EstimateAPIEntity
    { agencyNumber: ""
    , createdAt: ""
    , discount: Nothing
    , estimatedTotalFare: 0
    , agencyName: ""
    , vehicleVariant: ""
    , estimatedFare: 0
    , tripTerms: []
    , id: ""
    , agencyCompletedRidesCount: Nothing
    , estimateFareBreakup: Nothing
    , totalFareRange: Nothing
    , nightShiftRate: Nothing
    , specialLocationTag: Nothing
    , driversLatLong : []
    , serviceTierShortDesc: Nothing
    , serviceTierName : Nothing
    , airConditioned : Nothing
    , providerName : Nothing
    , providerId : Nothing
    , isValueAddNP : Nothing
    , validTill : ""
    }

getSpecialTag :: Maybe String -> SpecialTags
getSpecialTag specialTag =
  case specialTag of
    Just tag ->
      let zones = split (Pattern "_") tag
          sourceTag = getZoneType $ zones DA.!! 0
          destinationTag = getZoneType $ zones DA.!! 1
          priorityTag = if zones DA.!! 2 == Just "PriorityPickup" then sourceTag else destinationTag
      in { sourceTag : sourceTag, destinationTag : destinationTag, priorityTag : priorityTag}
    Nothing -> dummyZoneType

getZoneType :: Maybe String -> ZoneType
getZoneType tag =
  case tag of
    Just "SureMetro" -> METRO
    Just "SureBlockedAreaForAutos" -> AUTO_BLOCKED
    _                -> NOZONE
    
createEstimateForBookAny :: Array EstimateAPIEntity -> Array EstimateAPIEntity
createEstimateForBookAny estimates = []
  -- let config = getAppConfig appConfig
  --     selectedServices = getSelectedServices FunctionCall
  --     filteredEstimates = filter (\(EstimateAPIEntity item) -> ((DA.elem (fromMaybe "" item.serviceTierName) selectedServices) && (fromMaybe false item.isValueAddNP))) estimates
  -- in  if DA.length estimates > 1 && not ( DA.null filteredEstimates) then
  --       let bookAnyEstimate =
  --             EstimateAPIEntity
  --               { agencyNumber: ""
  --               , createdAt: "2024-04-18T09:46:37.579497Z"
  --               , discount: Nothing
  --               , estimatedTotalFare: 0
  --               , agencyName: "NAMMA_YATRI"
  --               , vehicleVariant: "BOOK_ANY"
  --               , estimatedFare: 0
  --               , tripTerms: []
  --               , id: ""
  --               , providerName : Nothing
  --               , providerId : Nothing
  --               , agencyCompletedRidesCount: 0
  --               , estimateFareBreakup: Just []
  --               , totalFareRange: Nothing
  --               , nightShiftRate: Nothing
  --               , specialLocationTag: Nothing
  --               , driversLatLong: []
  --               , serviceTierShortDesc: Just "Get Instantly"
  --               , serviceTierName: Just "Book Any"
  --               , airConditioned: Nothing
  --               , isValueAddNP: Just true
  --               , validTill : ""
  --               }
  --       in DA.singleton bookAnyEstimate
  --     else []

getTripFromRideHistory :: MyRidesScreenState -> Trip
getTripFromRideHistory state = {
    source :  state.data.selectedItem.source
  , destination : state.data.selectedItem.destination
  , sourceAddress : getAddressFromBooking state.data.selectedItem.sourceLocation
  , destinationAddress : getAddressFromBooking state.data.selectedItem.destinationLocation
  , sourceLat : state.data.selectedItem.sourceLocation^._lat
  , sourceLong : state.data.selectedItem.sourceLocation^._lon
  , destLat : state.data.selectedItem.destinationLocation^._lat
  , destLong : state.data.selectedItem.destinationLocation^._lon
  , isSpecialZone : state.data.selectedItem.isSpecialZone
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  }

fetchPickupCharges :: Array EstimateFares -> Int 
fetchPickupCharges estimateFareBreakup = 
  let 
    deadKmFare = find (\a -> a ^. _title == "DEAD_KILOMETER_FARE") estimateFareBreakup
  in 
    maybe 0 (\fare -> fare ^. _price) deadKmFare

getOneWaySpecialZoneAPIDetailsQuotes :: Array OfferRes -> Array OfferRes
getOneWaySpecialZoneAPIDetailsQuotes quotes = 
  filter 
  (\quote -> case quote of 
      Quotes body ->
        let (QuoteAPIEntity quoteEntity) = body.onDemandCab
            fareProductType = quoteEntity.quoteDetails^._fareProductType
        in fareProductType == "OneWaySpecialZoneAPIDetails" || fareProductType == "INTER_CITY" 
      _ -> false )
  quotes