{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.OnDemand.Utils.Common where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnSearch as OS
import qualified BecknV2.OnDemand.Enums as Enum
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Beckn
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Utils.Payment
import qualified BecknV2.Utils as Utils
import Control.Lens
import Data.Aeson
import qualified Data.Aeson as A
import Data.Default.Class
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import Domain.Action.Beckn.Search
import qualified Domain.Action.UI.Person as SP
import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.Common as DCT
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverStats as DDriverStats
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.Vehicle as Variant
import qualified Domain.Types.VehicleServiceTier as DVST
import EulerHS.Prelude hiding (id, state, view, (%~), (^?))
import qualified EulerHS.Prelude as Prelude
import GHC.Float (double2Int)
import qualified Kernel.External.Maps as Maps
import Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude as KP hiding (find, length, map, null, readMaybe)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common hiding (mkPrice)
import qualified Kernel.Types.Common as Common
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Utils.Common hiding (mkPrice)
import SharedLogic.DriverPool.Types
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import Tools.Error

data Pricing = Pricing
  { pricingId :: Text,
    pricingMaxFare :: HighPrecMoney,
    pricingMinFare :: HighPrecMoney,
    vehicleServiceTier :: DVST.ServiceTierType,
    serviceTierName :: Text,
    serviceTierDescription :: Maybe Text,
    vehicleVariant :: Variant.Variant,
    tripCategory :: DTC.TripCategory,
    fareParams :: Maybe Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    estimatedDistance :: Maybe Meters,
    specialLocationTag :: Maybe Text,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    fulfillmentType :: Text,
    distanceToNearestDriver :: Maybe Meters,
    tollNames :: Maybe [Text],
    currency :: Currency,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    specialLocationName :: Maybe Text
  }

data RateCardBreakupItem = RateCardBreakupItem
  { title :: Text,
    value :: Text
  }

firstStop :: [Spec.Stop] -> Maybe Spec.Stop
firstStop = find (\stop -> Spec.stopType stop == Just (show Enums.START))

lastStop :: [Spec.Stop] -> Maybe Spec.Stop
lastStop = find (\stop -> Spec.stopType stop == Just (show Enums.END))

mkStops :: Maps.LatLong -> Maybe Maps.LatLong -> Maybe [Spec.Stop]
mkStops origin mbDestination = do
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps destination = Gps.Gps {lat = destination.lat, lon = destination.lon}
  Just $
    catMaybes
      [ Just $
          Spec.Stop
            { stopLocation =
                Just $
                  Spec.Location
                    { locationAddress = Nothing, -- For start and end in on_search, we send address as nothing
                      locationAreaCode = Nothing,
                      locationCity = Nothing,
                      locationCountry = Nothing,
                      locationGps = Utils.gpsToText originGps,
                      locationState = Nothing,
                      locationId = Nothing,
                      locationUpdatedAt = Nothing
                    },
              stopType = Just $ show Enums.START,
              stopAuthorization = Nothing,
              stopTime = Nothing
            },
        ( \destination ->
            Spec.Stop
              { stopLocation =
                  Just $
                    Spec.Location
                      { locationAddress = Nothing, -- For start and end in on_search, we send address as nothing
                        locationAreaCode = Nothing,
                        locationCity = Nothing,
                        locationCountry = Nothing,
                        locationGps = Utils.gpsToText $ destinationGps destination,
                        locationState = Nothing,
                        locationId = Nothing,
                        locationUpdatedAt = Nothing
                      },
                stopType = Just $ show Enums.END,
                stopAuthorization = Nothing,
                stopTime = Nothing
              }
        )
          <$> mbDestination
      ]

parseLatLong :: MonadFlow m => Text -> m Maps.LatLong
parseLatLong a =
  case T.splitOn "," a of
    [latStr, longStr] ->
      let lat = fromMaybe 0.0 $ readMaybe $ T.unpack latStr
          lon = fromMaybe 0.0 $ readMaybe $ T.unpack longStr
       in return $ Maps.LatLong lat lon
    _ -> throwError . InvalidRequest $ "Unable to parse LatLong"

getContextCity :: MonadFlow m => Spec.Context -> m Context.City
getContextCity context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  city <- location.locationCity & fromMaybeM (InvalidRequest "Missing locationCity")
  cityText <- city.cityCode & fromMaybeM (InvalidRequest "Missing cityCode")
  decode (encode cityText) & fromMaybeM (InvalidRequest $ "Error in parsing cityCode: " <> cityText)

getContextCountry :: MonadFlow m => Spec.Context -> m Context.Country
getContextCountry context = do
  location <- context.contextLocation & fromMaybeM (InvalidRequest "Missing contextLocation")
  country <- location.locationCountry & fromMaybeM (InvalidRequest "Missing locationCountry")
  countryCodeText <- country.countryCode & fromMaybeM (InvalidRequest "Missing countryCode")
  decode (encode countryCodeText) & fromMaybeM (InvalidRequest $ "Error in parsing countryCode: " <> countryCodeText)

getContextBapUri :: MonadFlow m => Spec.Context -> m BaseUrl
getContextBapUri context = do
  bapUriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing contextBapUri")
  decode (encode bapUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBapUri: " <> bapUriText)

getContextBppUri :: MonadFlow m => Spec.Context -> m (Maybe BaseUrl)
getContextBppUri context = do
  let mbBppUriText = context.contextBppUri
  case mbBppUriText of
    Nothing -> pure Nothing
    Just bppUriText -> Just <$> A.decode (A.encode bppUriText) & fromMaybeM (InvalidRequest $ "Error in parsing contextBppUri: " <> bppUriText)

withTransactionIdLogTag :: (Log m) => Text -> m a -> m a
withTransactionIdLogTag = withTransactionIdLogTag'

getContextBapId :: MonadFlow m => Spec.Context -> m Text
getContextBapId context = do
  context.contextBapId & fromMaybeM (InvalidRequest "Missing contextBapId")

mkBppUri ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Text ->
  m BaseUrl
mkBppUri merchantId =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId)

castVariant :: Variant.Variant -> (Text, Text)
castVariant Variant.SEDAN = (show Enums.CAB, "SEDAN")
castVariant Variant.HATCHBACK = (show Enums.CAB, "HATCHBACK")
castVariant Variant.SUV = (show Enums.CAB, "SUV")
castVariant Variant.AUTO_RICKSHAW = (show Enums.AUTO_RICKSHAW, "AUTO_RICKSHAW")
castVariant Variant.TAXI = (show Enums.CAB, "TAXI")
castVariant Variant.TAXI_PLUS = (show Enums.CAB, "TAXI_PLUS")
castVariant Variant.PREMIUM_SEDAN = (show Enums.CAB, "PREMIUM_SEDAN")
castVariant Variant.BLACK = (show Enums.CAB, "BLACK")
castVariant Variant.BLACK_XL = (show Enums.CAB, "BLACK_XL")
castVariant Variant.BIKE = (show Enums.MOTORCYCLE, "BIKE")
castVariant Variant.AMBULANCE_TAXI = (show Enums.AMBULANCE, "AMBULANCE_TAXI")
castVariant Variant.AMBULANCE_TAXI_OXY = (show Enums.AMBULANCE, "AMBULANCE_TAXI_OXY")
castVariant Variant.AMBULANCE_AC = (show Enums.AMBULANCE, "AMBULANCE_AC")
castVariant Variant.AMBULANCE_AC_OXY = (show Enums.AMBULANCE, "AMBULANCE_AC_OXY")
castVariant Variant.AMBULANCE_VENTILATOR = (show Enums.AMBULANCE, "AMBULANCE_VENTILATOR")
castVariant Variant.SUV_PLUS = (show Enums.CAB, "SUV_PLUS")

mkFulfillmentType :: DCT.TripCategory -> Text
mkFulfillmentType = \case
  DCT.OneWay DCT.OneWayRideOtp -> show Enums.RIDE_OTP
  DCT.RideShare DCT.RideOtp -> show Enums.RIDE_OTP
  DCT.Rental _ -> show Enums.RENTAL
  DCT.InterCity _ _ -> show Enums.INTER_CITY
  DCT.Ambulance _ -> show Enums.AMBULANCE_FLOW
  _ -> show Enums.DELIVERY

rationaliseMoney :: Money -> Text
rationaliseMoney = OS.valueToString . OS.DecimalValue . toRational

castDPaymentType :: DMPM.PaymentType -> Text
castDPaymentType DMPM.ON_FULFILLMENT = show Enums.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = show Enums.ON_FULFILLMENT

parseVehicleVariant :: Maybe Text -> Maybe Text -> Maybe Variant.Variant
parseVehicleVariant mbCategory mbVariant = case (mbCategory, mbVariant) of
  (Just "CAB", Just "SEDAN") -> Just Variant.SEDAN
  (Just "CAB", Just "SUV") -> Just Variant.SUV
  (Just "CAB", Just "HATCHBACK") -> Just Variant.HATCHBACK
  (Just "CAB", Just "PREMIUM_SEDAN") -> Just Variant.PREMIUM_SEDAN
  (Just "CAB", Just "BLACK") -> Just Variant.BLACK
  (Just "AUTO_RICKSHAW", Just "AUTO_RICKSHAW") -> Just Variant.AUTO_RICKSHAW
  (Just "CAB", Just "TAXI") -> Just Variant.TAXI
  (Just "CAB", Just "TAXI_PLUS") -> Just Variant.TAXI_PLUS
  (Just "MOTORCYCLE", Just "BIKE") -> Just Variant.BIKE
  (Just "AMBULANCE", Just "AMBULANCE_TAXI") -> Just Variant.AMBULANCE_TAXI
  (Just "AMBULANCE", Just "AMBULANCE_TAXI_OXY") -> Just Variant.AMBULANCE_TAXI_OXY
  (Just "AMBULANCE", Just "AMBULANCE_AC") -> Just Variant.AMBULANCE_AC
  (Just "AMBULANCE", Just "AMBULANCE_AC_OXY") -> Just Variant.AMBULANCE_AC_OXY
  (Just "AMBULANCE", Just "AMBULANCE_VENTILATOR") -> Just Variant.AMBULANCE_VENTILATOR
  _ -> Nothing

parseAddress :: MonadFlow m => Spec.Location -> m (Maybe DL.LocationAddress)
parseAddress loc@Spec.Location {..} = do
  let areaCode = locationAreaCode
  let city' = locationCity >>= (.cityName)
  let state' = locationState >>= (.stateName)
  let country' = locationCountry >>= (.countryName)
  locationAddress' <- locationAddress & fromMaybeM (InvalidRequest $ "Missing locationAddress:-" <> show loc)
  address@OS.Address {..} <- buildAddressFromText locationAddress'
  let fullAddress = mkFullAddress address
  pure $
    Just $
      DL.LocationAddress
        { area = ward, -- TODO: Fetch this, discuss with ONDC
          city = city',
          state = state',
          country = country',
          ..
        }
  where
    mkFullAddress OS.Address {..} = do
      let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, city, state, area_code, country]
      if null strictFields
        then Nothing
        else Just $ T.intercalate ", " strictFields
    -- mkFullAddress city state country = do
    --   let strictFields = catMaybes $ filter (not . isEmpty) [locationAddress, city, state, country]
    --   if null strictFields
    --     then Nothing
    --     else Just $ T.intercalate ", " strictFields

    isEmpty :: Maybe Text -> Bool
    isEmpty = maybe True (T.null . T.replace " " "")

mkStops' :: DLoc.Location -> Maybe DLoc.Location -> Maybe Text -> Maybe [Spec.Stop]
mkStops' origin mbDestination mAuthorization =
  let originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopAuthorization = mAuthorization >>= mkAuthorization,
                  stopTime = Nothing
                },
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Just $ mkAddress destination.address,
                            locationAreaCode = destination.address.areaCode,
                            locationCity = Just $ Spec.City Nothing destination.address.city,
                            locationCountry = Just $ Spec.Country Nothing destination.address.country,
                            locationGps = Utils.gpsToText $ destinationGps destination,
                            locationState = Just $ Spec.State destination.address.state,
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing
                  }
            )
              <$> mbDestination
          ]
  where
    mkAuthorization :: Text -> Maybe Spec.Authorization
    mkAuthorization auth =
      Just $
        Spec.Authorization
          { authorizationToken = Just auth,
            authorizationType = Just $ show Enums.OTP
          }

mkAddress :: DLoc.LocationAddress -> Text
mkAddress DLoc.LocationAddress {..} =
  let res = map replaceEmpty [door, building, street, area, city, state, country]
   in T.intercalate ", " $ catMaybes res

data DriverInfo = DriverInfo
  { mobileNumber :: Text,
    name :: Text,
    tags :: Maybe [Spec.TagGroup]
  }

showVariant :: DVeh.Variant -> Maybe Text
showVariant = A.decode . A.encode

-- common for on_update & on_status
mkStopsOUS :: DBooking.Booking -> DRide.Ride -> Text -> Maybe [Spec.Stop]
mkStopsOUS booking ride rideOtp =
  let origin = booking.fromLocation
      mbDestination = booking.toLocation
      originGps = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = Just $ mkAddress origin.address,
                          locationAreaCode = origin.address.areaCode,
                          locationCity = Just $ Spec.City Nothing origin.address.city,
                          locationCountry = Just $ Spec.Country Nothing origin.address.country,
                          locationGps = Utils.gpsToText originGps,
                          locationState = Just $ Spec.State origin.address.state,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.START,
                  stopAuthorization =
                    Just $
                      Spec.Authorization
                        { authorizationToken = Just rideOtp,
                          authorizationType = Just $ show Enums.OTP
                        },
                  stopTime = ride.tripStartTime <&> \tripStartTime' -> Spec.Time {timeTimestamp = Just tripStartTime', timeDuration = Nothing}
                },
            Just $
              Spec.Stop
                { stopLocation =
                    Just $
                      Spec.Location
                        { locationAddress = (\dest -> Just $ mkAddress dest.address) =<< mbDestination,
                          locationAreaCode = (\dest -> dest.address.areaCode) =<< mbDestination,
                          locationCity = (\dest -> Just $ Spec.City Nothing $ dest.address.city) =<< mbDestination,
                          locationCountry = (\dest -> Just $ Spec.Country Nothing $ dest.address.country) =<< mbDestination,
                          locationGps = (\dest -> Utils.gpsToText (destinationGps dest)) =<< mbDestination,
                          locationState = (\dest -> Just $ Spec.State dest.address.state) =<< mbDestination,
                          locationId = Nothing,
                          locationUpdatedAt = Nothing
                        },
                  stopType = Just $ show Enums.END,
                  stopAuthorization = Nothing,
                  stopTime = ride.tripEndTime <&> \tripEndTime' -> Spec.Time {timeTimestamp = Just tripEndTime', timeDuration = Nothing}
                }
          ]

type IsValueAddNP = Bool

-- common for on_update & on_status
mkFulfillmentV2 ::
  (MonadFlow m, EncFlow m r) =>
  Maybe SP.Person ->
  Maybe DDriverStats.DriverStats ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Maybe [Spec.TagGroup] ->
  Bool ->
  Bool ->
  Maybe Payment.AccountId ->
  Maybe Text ->
  IsValueAddNP ->
  Maybe Text ->
  m Spec.Fulfillment
mkFulfillmentV2 mbDriver mbDriverStats ride booking mbVehicle mbImage mbTags mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP riderPhone = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
        fulfillmentStops = mkStopsOUS booking ride rideOtp,
        fulfillmentType = Just $ mkFulfillmentType booking.tripCategory,
        fulfillmentAgent =
          Just $
            Spec.Agent
              { agentContact =
                  mbDInfo >>= \dInfo ->
                    Just $
                      Spec.Contact
                        { contactPhone = Just dInfo.mobileNumber
                        },
                agentPerson =
                  Just $
                    Spec.Person
                      { personId = Nothing,
                        personImage =
                          mbImage <&> \mbImage' ->
                            Spec.Image
                              { imageHeight = Nothing,
                                imageSizeType = Nothing,
                                imageUrl = Just mbImage',
                                imageWidth = Nothing
                              },
                        personName = mbDInfo >>= Just . (.name),
                        personTags = mbDInfo >>= (.tags) & (mbPersonTags <>)
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              Spec.Vehicle
                { vehicleColor = Just vehicle.color,
                  vehicleModel = Just vehicle.model,
                  vehicleRegistration = Just vehicle.registrationNo,
                  vehicleCategory = Just category,
                  vehicleVariant = Just variant,
                  vehicleMake = Nothing,
                  vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = tfCustomer riderPhone booking.riderName,
        fulfillmentState =
          mbEvent
            >> ( Just $
                   Spec.FulfillmentState
                     { fulfillmentStateDescriptor =
                         Just $
                           Spec.Descriptor
                             { descriptorCode = mbEvent,
                               descriptorName = Nothing,
                               descriptorShortDesc = Nothing
                             }
                     }
               ),
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            name = dName,
            tags = if isValueAddNP then dTags else Nothing
          }

tfCustomer :: Maybe Text -> Maybe Text -> Maybe Spec.Customer
tfCustomer riderPhone riderName =
  Just
    Spec.Customer
      { customerContact =
          Just
            Spec.Contact
              { contactPhone = riderPhone
              },
        customerPerson = do
          Just $
            Spec.Person
              { personId = Nothing,
                personImage = Nothing,
                personName = riderName,
                personTags = Nothing
              }
      }

mkDriverDetailsTags :: SP.Person -> DDriverStats.DriverStats -> Bool -> Bool -> Maybe Payment.AccountId -> Maybe [Spec.TagGroup]
mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId =
  Just
    [ Spec.TagGroup
        { tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.DRIVER_DETAILS,
                  descriptorName = Just "Driver Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupDisplay = Just False,
          tagGroupList =
            Just $
              registeredAtSingleton
                ++ driverRatingSingleton
                ++ isDriverBirthDaySingleton
                ++ isFreeRideSingleton
                ++ driverAccountIdSingleton
        }
    ]
  where
    registeredAtSingleton =
      List.singleton $
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tags.REGISTERED_AT,
                    descriptorName = Just "Registered At",
                    descriptorShortDesc = Nothing
                  },
            tagDisplay = Just False,
            tagValue = Just $ show driver.createdAt
          }

    driverRatingSingleton
      | isNothing driverStats.rating = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.RATING,
                      descriptorName = Just "rating",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = show <$> driverStats.rating
            }

    isDriverBirthDaySingleton
      | not isDriverBirthDay = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_DRIVER_BIRTHDAY,
                      descriptorName = Just "Is Driver BirthDay",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show isDriverBirthDay
            }

    driverAccountIdSingleton
      | isNothing driverAccountId = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DRIVER_ACCOUNT_ID,
                      descriptorName = Just "Driver Account Id",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = driverAccountId
            }

    isFreeRideSingleton
      | not isFreeRide = []
      | otherwise =
        List.singleton $
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_FREE_RIDE,
                      descriptorName = Just "Is Free Ride",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show isFreeRide
            }

mkLocationTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkLocationTagGroupV2 location' =
  location' <&> \location ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.CURRENT_LOCATION,
                  descriptorName = Just "Current Location",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.CURRENT_LOCATION_LAT,
                            descriptorName = Just "Current Location Lat",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show location.lat
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.CURRENT_LOCATION_LON,
                            descriptorName = Just "Current Location Lon",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show location.lon
                  }
              ]
        }
    ]

mkVehicleTags :: Maybe Double -> Maybe Bool -> Maybe [Spec.TagGroup]
mkVehicleTags vehicleServiceTierAirConditioned' isAirConditioned =
  vehicleServiceTierAirConditioned' <&> \vehicleServiceTierAirConditioned ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just True,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.VEHICLE_INFO,
                  descriptorName = Just "Vehicle Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just True,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.IS_AIR_CONDITIONED,
                            descriptorName = Just "isAirConditioned",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show vehicleServiceTierAirConditioned
                  },
                Spec.Tag
                  { tagDisplay = Just True,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.IS_AIR_CONDITIONED_VEHICLE,
                            descriptorName = Just "isAirConditionedVehicle",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = show <$> isAirConditioned
                  }
              ]
        }
    ]

mkOdometerTagGroupV2 :: Maybe Centesimal -> Maybe [Spec.TagGroup]
mkOdometerTagGroupV2 startOdometerReading' =
  startOdometerReading' <&> \startOdometerReading ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.RIDE_ODOMETER_DETAILS,
                  descriptorName = Just "Ride Odometer Details",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.START_ODOMETER_READING,
                            descriptorName = Just "Start Odometer Reading",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show startOdometerReading
                  }
              ]
        }
    ]

mkTollConfidenceTagGroupV2 :: Maybe Confidence -> Maybe [Spec.TagGroup]
mkTollConfidenceTagGroupV2 tollConfidence' =
  tollConfidence' <&> \tollConfidence ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.TOLL_CONFIDENCE_INFO,
                  descriptorName = Just "Toll Confidence Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.TOLL_CONFIDENCE,
                            descriptorName = Just "Toll Confidence (Sure/Unsure/Neutral)",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show tollConfidence
                  }
              ]
        }
    ]

mkVehicleAgeTagGroupV2 :: Maybe Months -> Maybe [Spec.TagGroup]
mkVehicleAgeTagGroupV2 vehicleAge' =
  vehicleAge' <&> \vehicleAge ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.VEHICLE_AGE_INFO,
                  descriptorName = Just "Vehicle Age Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.VEHICLE_AGE,
                            descriptorName = Just "Vehicle Age",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show vehicleAge
                  }
              ]
        }
    ]

buildAddressFromText :: MonadFlow m => Text -> m OS.Address
buildAddressFromText fullAddress = do
  let splitedAddress = T.splitOn ", " fullAddress
      totalAddressComponents = List.length splitedAddress
  logDebug $ "Search Address:-" <> fullAddress
  let area_code_ = Nothing
      building_ = splitedAddress !? (totalAddressComponents - 6)
      city_ = splitedAddress !? (totalAddressComponents - 3)
      country_ = splitedAddress !? (totalAddressComponents - 1)
      door_ =
        if totalAddressComponents > 7
          then splitedAddress !? 0 <> Just ", " <> splitedAddress !? 1
          else splitedAddress !? 0
      locality_ = splitedAddress !? (totalAddressComponents - 4)
      state_ = splitedAddress !? (totalAddressComponents - 2)
      street_ = splitedAddress !? (totalAddressComponents - 5)
      building = replaceEmpty building_
      street = replaceEmpty street_
      locality = replaceEmpty locality_
      ward_ = Just $ T.intercalate ", " $ catMaybes [locality, street, building]
      ward = if ward_ == Just "" then city_ else ward_
  pure $ OS.Address {area_code = area_code_, building = building_, city = city_, country = country_, door = door_, locality = locality_, state = state_, street = street_, ward = ward}

(!?) :: [a] -> Int -> Maybe a
(!?) xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just $ xs List.!! i

replaceEmpty :: Maybe Text -> Maybe Text
replaceEmpty string = if string == Just "" then Nothing else string

mapServiceTierToCategory :: DVST.ServiceTierType -> VehicleCategory
mapServiceTierToCategory serviceTier =
  case serviceTier of
    DVST.SEDAN -> CAB
    DVST.HATCHBACK -> CAB
    DVST.TAXI -> CAB
    DVST.SUV -> CAB
    DVST.TAXI_PLUS -> CAB
    DVST.COMFY -> CAB
    DVST.ECO -> CAB
    DVST.PREMIUM -> CAB
    DVST.PREMIUM_SEDAN -> CAB
    DVST.BLACK -> CAB
    DVST.BLACK_XL -> CAB
    DVST.AUTO_RICKSHAW -> AUTO_RICKSHAW
    DVST.BIKE -> MOTORCYCLE
    DVST.AMBULANCE_TAXI -> AMBULANCE
    DVST.AMBULANCE_TAXI_OXY -> AMBULANCE
    DVST.AMBULANCE_AC -> AMBULANCE
    DVST.AMBULANCE_AC_OXY -> AMBULANCE
    DVST.AMBULANCE_VENTILATOR -> AMBULANCE
    DVST.SUV_PLUS -> CAB

mapRideStatus :: Maybe DRide.RideStatus -> Enums.FulfillmentState
mapRideStatus rideStatus =
  case rideStatus of
    Just DRide.UPCOMING -> Enums.SCHEDULED_RIDE_ASSIGNED
    Just DRide.NEW -> Enums.RIDE_ASSIGNED
    Just DRide.INPROGRESS -> Enums.RIDE_STARTED
    Just DRide.COMPLETED -> Enums.RIDE_ENDED
    Just DRide.CANCELLED -> Enums.RIDE_CANCELLED
    Nothing -> Enums.RIDE_ASSIGNED

tfCancellationFee :: Maybe Common.PriceAPIEntity -> Maybe Spec.Fee
tfCancellationFee Nothing = Nothing
tfCancellationFee (Just price) = do
  Just
    Spec.Fee
      { feeAmount = mkPrice,
        feePercentage = Nothing
      }
  where
    mkPrice =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just $ show price.currency,
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Nothing,
            priceValue = Just $ encodeToText price.amount
          }

tfFulfillmentState :: Enums.FulfillmentState -> Maybe Spec.FulfillmentState
tfFulfillmentState state =
  Just $
    Spec.FulfillmentState
      { fulfillmentStateDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just $ show state,
                descriptorName = Nothing,
                descriptorShortDesc = Nothing
              }
      }

tfQuotation :: DBooking.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup booking.fareParams,
        quotationPrice = tfQuotationPrice $ HighPrecMoney $ toRational booking.estimatedFare,
        quotationTtl = Nothing
      }

tfQuotationSU :: DFParams.FareParameters -> HighPrecMoney -> Maybe Spec.Quotation
tfQuotationSU fareParams estimatedFare =
  Just
    Spec.Quotation
      { quotationBreakup = mkQuotationBreakup fareParams,
        quotationPrice = tfQuotationPrice estimatedFare,
        quotationTtl = Nothing
      }

tfQuotationPrice :: HighPrecMoney -> Maybe Spec.Price
tfQuotationPrice estimatedFare =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText estimatedFare,
        priceValue = Just $ encodeToText estimatedFare
      }

mkQuotationBreakup :: DFParams.FareParameters -> Maybe [Spec.QuotationBreakupInner]
mkQuotationBreakup fareParams =
  let fareParameters = mkFareParamsBreakups mkPrice mkQuotationBreakupInner fareParams
   in Just $ filter (filterRequiredBreakups $ DFParams.getFareParametersType fareParams) fareParameters -- TODO: Remove after roll out
  where
    mkPrice money =
      Just
        Spec.Price
          { priceComputedValue = Nothing,
            priceCurrency = Just "INR",
            priceMaximumValue = Nothing,
            priceMinimumValue = Nothing,
            priceOfferedValue = Just $ encodeToText money,
            priceValue = Just $ encodeToText money
          }

    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }

    filterRequiredBreakups :: DFParams.FareParametersType -> Spec.QuotationBreakupInner -> Bool
    filterRequiredBreakups fParamsType breakup = do
      case fParamsType of
        DFParams.Progressive ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_CHARGES)
        DFParams.Slab ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PLATFORM_FEE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CGST)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.FIXED_GOVERNMENT_RATE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_CHARGES)
        DFParams.Rental ->
          breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TIME_BASED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
            || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
        DFParams.InterCity -> True
        DFParams.Ambulance -> True

type MerchantShortId = Text

tfItems :: DBooking.Booking -> MerchantShortId -> Maybe Meters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> Maybe [Spec.Item]
tfItems booking shortId estimatedDistance mbFarePolicy mbPaymentId =
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor booking,
          itemFulfillmentIds = Just [booking.quoteId],
          itemId = Just $ maybe (Common.mkItemId shortId booking.vehicleServiceTier) getId (booking.estimateId),
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId mbPaymentId,
          itemPrice = tfItemPrice $ booking.estimatedFare,
          itemTags = mkRateCardTag estimatedDistance Nothing mbFarePolicy
        }
    ]

tfItemsSoftUpdate :: DBooking.Booking -> MerchantShortId -> Maybe HighPrecMeters -> Maybe FarePolicyD.FarePolicy -> Maybe Text -> DBUR.BookingUpdateRequest -> Text -> Maybe [Spec.Item]
tfItemsSoftUpdate booking shortId estimatedDistance mbFarePolicy mbPaymentId updatedBooking rideId = do
  let estimatedDistance' = maybe Nothing (\dist -> Just $ highPrecMetersToMeters dist) estimatedDistance
  Just
    [ Spec.Item
        { itemDescriptor = tfItemDescriptor booking,
          itemFulfillmentIds = Just [rideId],
          itemId = Just $ Common.mkItemId shortId booking.vehicleServiceTier,
          itemLocationIds = Nothing,
          itemPaymentIds = tfPaymentId mbPaymentId,
          itemPrice = tfItemPrice updatedBooking.estimatedFare,
          itemTags = mkRateCardTag estimatedDistance' Nothing mbFarePolicy
        }
    ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]

tfItemPrice :: HighPrecMoney -> Maybe Spec.Price
tfItemPrice estimatedFare =
  Just
    Spec.Price
      { priceComputedValue = Nothing,
        priceCurrency = Just "INR",
        priceMaximumValue = Nothing,
        priceMinimumValue = Nothing,
        priceOfferedValue = Just $ encodeToText estimatedFare, -- TODO : Remove this and make non mandatory on BAP side
        priceValue = Just $ encodeToText estimatedFare
      }

tfItemDescriptor :: DBooking.Booking -> Maybe Spec.Descriptor
tfItemDescriptor booking =
  Just
    Spec.Descriptor
      { descriptorCode = Just "RIDE",
        descriptorShortDesc = Just $ show booking.vehicleServiceTier,
        descriptorName = Just $ show booking.vehicleServiceTier
      }

convertEstimateToPricing :: Maybe Text -> (DEst.Estimate, DVST.VehicleServiceTier, Maybe NearestDriverInfo) -> Pricing
convertEstimateToPricing specialLocationName (DEst.Estimate {..}, serviceTier, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = maxFare,
      pricingMinFare = minFare,
      fulfillmentType = show Enums.DELIVERY,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.allowedVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      vehicleServiceTierSeatingCapacity = serviceTier.seatingCapacity,
      vehicleServiceTierAirConditioned = serviceTier.airConditionedThreshold,
      isAirConditioned = serviceTier.isAirConditioned,
      ..
    }

convertQuoteToPricing :: Maybe Text -> (DQuote.Quote, DVST.VehicleServiceTier, Maybe NearestDriverInfo) -> Pricing
convertQuoteToPricing specialLocationName (DQuote.Quote {..}, serviceTier, mbDriverLocations) =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      estimatedDistance = distance,
      fareParams = Just fareParams,
      fulfillmentType = mapToFulfillmentType tripCategory,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.allowedVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = mbDriverLocations <&> (.distanceToNearestDriver),
      vehicleServiceTierSeatingCapacity = serviceTier.seatingCapacity,
      vehicleServiceTierAirConditioned = serviceTier.airConditionedThreshold,
      isAirConditioned = serviceTier.isAirConditioned,
      ..
    }
  where
    mapToFulfillmentType (DTC.OneWay DTC.OneWayRideOtp) = show Enums.RIDE_OTP
    mapToFulfillmentType (DTC.RideShare DTC.RideOtp) = show Enums.RIDE_OTP
    mapToFulfillmentType (DTC.Rental _) = show Enums.RENTAL
    mapToFulfillmentType (DTC.InterCity _ _) = show Enums.INTER_CITY
    mapToFulfillmentType (DTC.Ambulance _) = show Enums.AMBULANCE_FLOW
    mapToFulfillmentType _ = show Enums.RIDE_OTP -- backward compatibility

convertBookingToPricing :: DVST.VehicleServiceTier -> DBooking.Booking -> Pricing
convertBookingToPricing serviceTier DBooking.Booking {..} =
  Pricing
    { pricingId = id.getId,
      pricingMaxFare = estimatedFare,
      pricingMinFare = estimatedFare,
      tripCategory = tripCategory,
      fareParams = Just fareParams,
      farePolicy = Nothing,
      fulfillmentType = show Enums.DELIVERY,
      serviceTierName = serviceTier.name,
      serviceTierDescription = serviceTier.shortDescription,
      vehicleVariant = fromMaybe (castServiceTierToVariant vehicleServiceTier) (listToMaybe serviceTier.allowedVehicleVariant), -- ideally this should not be empty
      distanceToNearestDriver = Nothing,
      isCustomerPrefferedSearchRoute = Nothing,
      isBlockedRoute = Nothing,
      specialLocationName = Nothing,
      ..
    }

mkGeneralInfoTagGroup :: DTC.TransporterConfig -> Pricing -> Bool -> Maybe Spec.TagGroup
mkGeneralInfoTagGroup transporterConfig pricing isValueAddNP
  | isNothing pricing.specialLocationTag && isNothing pricing.distanceToNearestDriver = Nothing
  | otherwise =
    Just $
      Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.INFO,
                  descriptorName = Just "Information",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            specialLocationTagSingleton pricing.specialLocationTag
              <> specialLocationNameTag pricing.specialLocationName
              <> distanceToNearestDriverTagSingleton pricing.distanceToNearestDriver
              <> isCustomerPrefferedSearchRouteSingleton pricing.isCustomerPrefferedSearchRoute
              <> isBlockedRouteSingleton pricing.isBlockedRoute
              <> tollNamesSingleton pricing.tollNames
              <> durationToNearestDriverTagSingleton
        }
  where
    specialLocationTagSingleton specialLocationTag
      | isNothing specialLocationTag = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just True,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SPECIAL_LOCATION_TAG,
                      descriptorName = Just "Special Location Tag",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationTag
            }
    specialLocationNameTag specialLocationName
      | isNothing specialLocationName = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.SPECIAL_LOCATION_NAME,
                      descriptorName = Just "Special Location Name",
                      descriptorShortDesc = Nothing
                    },
              tagValue = specialLocationName
            }
    distanceToNearestDriverTagSingleton distanceToNearestDriver
      | isNothing distanceToNearestDriver = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DISTANCE_TO_NEAREST_DRIVER_METER,
                      descriptorName = Just "Distance To Nearest Driver Meter",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show . double2Int . realToFrac <$> distanceToNearestDriver
            }
    isCustomerPrefferedSearchRouteSingleton isCustomerPrefferedSearchRoute
      | isNothing isCustomerPrefferedSearchRoute || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_CUSTOMER_PREFFERED_SEARCH_ROUTE,
                      descriptorName = Just "Is Customer Preffered Search Route",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> isCustomerPrefferedSearchRoute
            }
    isBlockedRouteSingleton isBlockedRoute
      | isNothing isBlockedRoute || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.IS_BLOCKED_SEARCH_ROUTE,
                      descriptorName = Just "Is Blocked Search Route",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> isBlockedRoute
            }
    tollNamesSingleton tollNames
      | isNothing tollNames || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.TOLL_NAMES,
                      descriptorName = Just "Toll Names",
                      descriptorShortDesc = Nothing
                    },
              tagValue = show <$> tollNames
            }
    durationToNearestDriverTagSingleton
      | isNothing (pricing.distanceToNearestDriver) || not isValueAddNP = Nothing
      | otherwise =
        Just . List.singleton $
          Spec.Tag
            { tagDisplay = Just False,
              tagDescriptor =
                Just
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DURATION_TO_NEAREST_DRIVER_MINUTES,
                      descriptorName = Just $ show pricing.vehicleVariant,
                      descriptorShortDesc = Nothing
                    },
              tagValue = getDurationToNearestDriver (DTC.avgSpeedOfVehicle transporterConfig)
            }
      where
        getDurationToNearestDriver :: Maybe DTC.AvgSpeedOfVechilePerKm -> Maybe Text
        getDurationToNearestDriver avgSpeedOfVehicle = do
          avgSpeed <- avgSpeedOfVehicle
          let variantSpeed = case pricing.vehicleVariant of
                Variant.SEDAN -> avgSpeed.sedan.getKilometers
                Variant.SUV -> avgSpeed.suv.getKilometers
                Variant.HATCHBACK -> avgSpeed.hatchback.getKilometers
                Variant.AUTO_RICKSHAW -> avgSpeed.autorickshaw.getKilometers
                Variant.BIKE -> avgSpeed.bike.getKilometers
                Variant.TAXI -> avgSpeed.taxi.getKilometers
                Variant.TAXI_PLUS -> avgSpeed.ambulance.getKilometers
                Variant.PREMIUM_SEDAN -> avgSpeed.premiumsedan.getKilometers
                Variant.BLACK -> avgSpeed.black.getKilometers
                Variant.BLACK_XL -> avgSpeed.blackxl.getKilometers
                Variant.AMBULANCE_TAXI -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_TAXI_OXY -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_AC -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_AC_OXY -> avgSpeed.ambulance.getKilometers
                Variant.AMBULANCE_VENTILATOR -> avgSpeed.ambulance.getKilometers
                Variant.SUV_PLUS -> avgSpeed.suvplus.getKilometers

          getDuration pricing.distanceToNearestDriver variantSpeed

        getDuration :: Maybe Meters -> Int -> Maybe Text
        getDuration distance avgSpeed
          | avgSpeed <= 0 = Nothing
          -- lets return 60 seconds in case distance is 0
          | distance == Just 0 = Just "60"
          | otherwise = do
            distance' <- distance
            let distanceInMeters = realToFrac @_ @Double distance'
                avgSpeedInMetersPerSec = realToFrac @_ @Double (avgSpeed * 5) / 18
                estimatedTimeTakenInSeconds :: Int = ceiling $ (distanceInMeters / avgSpeedInMetersPerSec)
            Just $ show estimatedTimeTakenInSeconds

mkRateCardTag :: Maybe Meters -> Maybe HighPrecMoney -> Maybe FarePolicyD.FarePolicy -> Maybe [Spec.TagGroup]
mkRateCardTag estimatedDistance tollCharges farePolicy = do
  let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id mkRateCardBreakupItem estimatedDistance tollCharges) farePolicy
      farePolicyBreakupsTags = buildRateCardTags <$> farePolicyBreakups
  Just
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.FARE_POLICY,
                  descriptorName = Just "Fare Policy",
                  descriptorShortDesc = Nothing
                },
          tagGroupList = Just farePolicyBreakupsTags
        }
    ]

mkRateCardBreakupItem :: Text -> Text -> RateCardBreakupItem
mkRateCardBreakupItem = RateCardBreakupItem

buildRateCardTags :: RateCardBreakupItem -> Spec.Tag
buildRateCardTags RateCardBreakupItem {..} =
  Spec.Tag
    { tagDisplay = Just False,
      tagDescriptor =
        Just
          Spec.Descriptor
            { descriptorCode = Just title,
              descriptorName = Just title,
              descriptorShortDesc = Nothing
            },
      tagValue = Just value
    }

tfCancellationTerms :: Maybe Common.PriceAPIEntity -> Maybe Enums.FulfillmentState -> [Spec.CancellationTerm]
tfCancellationTerms cancellationFee state =
  List.singleton
    Spec.CancellationTerm
      { cancellationTermCancellationFee = tfCancellationFee cancellationFee,
        cancellationTermFulfillmentState = mkFulfillmentState <$> state,
        cancellationTermReasonRequired = Just False -- TODO : Make true if reason parsing is added
      }

tfPayments :: DBooking.Booking -> DM.Merchant -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments booking transporter bppConfig = do
  let mPrice = Just $ Common.mkPrice (Just booking.currency) booking.estimatedFare
      updatedPaymentTags = def{Beckn.paymentTags = [(Beckn.BUYER_FINDER_FEES_PERCENTAGE, Just $ fromMaybe "0" bppConfig.buyerFinderFee), (Beckn.SETTLEMENT_WINDOW, Just $ fromMaybe "PT1D" bppConfig.settlementWindow), (Beckn.DELAY_INTEREST, Just "0"), (Beckn.SETTLEMENT_BASIS, Just "INVOICE_RECIEPT"), (Beckn.MANDATORY_ARBITRATION, Just "TRUE"), (Beckn.COURT_JURISDICTION, Just $ show transporter.city), (Beckn.STATIC_TERMS, Just $ maybe "https://api.example-bap.com/booking/terms" KP.showBaseUrl bppConfig.staticTermsUrl), (Beckn.SETTLEMENT_TYPE, bppConfig.settlementType)]}
      mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . List.singleton $ mkPayment' (show bppConfig.collectedBy) Enum.NOT_PAID mPrice booking.paymentId mkParams (Just updatedPaymentTags)

tfProvider :: DBC.BecknConfig -> Maybe Spec.Provider
tfProvider becknConfig =
  return $
    Spec.Provider
      { providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just $ becknConfig.subscriberId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing
      }

mkFulfillmentV2SoftUpdate ::
  (MonadFlow m, EncFlow m r) =>
  Maybe SP.Person ->
  Maybe DDriverStats.DriverStats ->
  DRide.Ride ->
  DBooking.Booking ->
  Maybe DVeh.Vehicle ->
  Maybe Text ->
  Maybe [Spec.TagGroup] ->
  Maybe [Spec.TagGroup] ->
  Bool ->
  Bool ->
  Maybe Payment.AccountId ->
  Maybe Text ->
  IsValueAddNP ->
  DLoc.Location ->
  m Spec.Fulfillment
mkFulfillmentV2SoftUpdate mbDriver mbDriverStats ride booking mbVehicle mbImage mbTags mbPersonTags isDriverBirthDay isFreeRide driverAccountId mbEvent isValueAddNP newDestination = do
  mbDInfo <- driverInfo
  let rideOtp = fromMaybe ride.otp ride.endOtp
  pure $
    Spec.Fulfillment
      { fulfillmentId = Just ride.id.getId,
        fulfillmentStops = mkStops' booking.fromLocation (Just newDestination) (Just rideOtp),
        fulfillmentType = Just $ mkFulfillmentType booking.tripCategory,
        fulfillmentAgent =
          Just $
            Spec.Agent
              { agentContact =
                  mbDInfo >>= \dInfo ->
                    Just $
                      Spec.Contact
                        { contactPhone = Just dInfo.mobileNumber
                        },
                agentPerson =
                  Just $
                    Spec.Person
                      { personId = Nothing,
                        personImage =
                          mbImage <&> \mbImage' ->
                            Spec.Image
                              { imageHeight = Nothing,
                                imageSizeType = Nothing,
                                imageUrl = Just mbImage',
                                imageWidth = Nothing
                              },
                        personName = mbDInfo >>= Just . (.name),
                        personTags = mbDInfo >>= (.tags) & (mbPersonTags <>)
                      }
              },
        fulfillmentVehicle =
          mbVehicle >>= \vehicle -> do
            let (category, variant) = castVariant vehicle.variant
            Just $
              Spec.Vehicle
                { vehicleColor = Just vehicle.color,
                  vehicleModel = Just vehicle.model,
                  vehicleRegistration = Just vehicle.registrationNo,
                  vehicleCategory = Just category,
                  vehicleVariant = Just variant,
                  vehicleMake = Nothing,
                  vehicleCapacity = vehicle.capacity
                },
        fulfillmentCustomer = Nothing,
        fulfillmentState =
          mbEvent
            >> ( Just $
                   Spec.FulfillmentState
                     { fulfillmentStateDescriptor =
                         Just $
                           Spec.Descriptor
                             { descriptorCode = mbEvent,
                               descriptorName = Nothing,
                               descriptorShortDesc = Nothing
                             }
                     }
               ),
        fulfillmentTags = mbTags
      }
  where
    driverInfo = forM (liftM2 (,) mbDriver mbDriverStats) $ \(driver, driverStats) -> do
      dPhoneNum <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present in OnUpdateBuildReq.")
      dName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
      let dTags = mkDriverDetailsTags driver driverStats isDriverBirthDay isFreeRide driverAccountId
      pure $
        DriverInfo
          { mobileNumber = dPhoneNum,
            name = dName,
            tags = if isValueAddNP then dTags else Nothing
          }

buildLocation :: MonadFlow m => Spec.Stop -> m DL.Location
buildLocation stop = do
  location <- stop.stopLocation & fromMaybeM (InvalidRequest "Location not present")
  guid <- generateGUID
  now <- getCurrentTime
  gps <- parseLatLong =<< (location.locationGps & fromMaybeM (InvalidRequest "Location GPS not present"))
  address <- parseAddress location >>= fromMaybeM (InvalidRequest "Location Address not present")
  return $
    DL.Location
      { DL.id = guid,
        createdAt = now,
        updatedAt = now,
        lat = gps.lat,
        lon = gps.lon,
        address
      }

castPaymentCollector :: MonadFlow m => Text -> m DMPM.PaymentCollector
castPaymentCollector "BAP" = return DMPM.BAP
castPaymentCollector "BPP" = return DMPM.BPP
castPaymentCollector _ = throwM $ InvalidRequest "Unknown Payment Collector"

castPaymentType :: MonadFlow m => Text -> m DMPM.PaymentType
castPaymentType "ON_ORDER" = return DMPM.ON_FULFILLMENT
castPaymentType "ON_FULFILLMENT" = return DMPM.POSTPAID
castPaymentType _ = throwM $ InvalidRequest "Unknown Payment Type"

mkForwardBatchTagGroupV2 :: Maybe Maps.LatLong -> Maybe [Spec.TagGroup]
mkForwardBatchTagGroupV2 previousRideDropLocation' =
  previousRideDropLocation' <&> \previousRideDropLocation ->
    [ Spec.TagGroup
        { tagGroupDisplay = Just False,
          tagGroupDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tags.FORWARD_BATCHING_REQUEST_INFO,
                  descriptorName = Just "Forward Batching Request Info",
                  descriptorShortDesc = Nothing
                },
          tagGroupList =
            Just
              [ Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PREVIOUS_RIDE_DROP_LOCATION_LAT,
                            descriptorName = Just "Current Location Lat",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show previousRideDropLocation.lat
                  },
                Spec.Tag
                  { tagDisplay = Just False,
                    tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PREVIOUS_RIDE_DROP_LOCATION_LON,
                            descriptorName = Just "Current Location Lon",
                            descriptorShortDesc = Nothing
                          },
                    tagValue = Just $ show previousRideDropLocation.lon
                  }
              ]
        }
    ]

getShouldFavouriteDriver :: Spec.Rating -> Maybe Bool
getShouldFavouriteDriver req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.SHOULD_FAVOURITE_DRIVER tagGroups
   in readMaybe . T.unpack =<< tagValue

getRiderPhoneNumber :: Spec.Rating -> Maybe Text
getRiderPhoneNumber req = do
  let tagGroups = req.ratingTag
      tagValue = Utils.getTagV2 Tags.RATING_TAGS Tags.RIDER_PHONE_NUMBER tagGroups
   in tagValue

mkFulfillmentState :: Enums.FulfillmentState -> Spec.FulfillmentState
mkFulfillmentState stateCode =
  Spec.FulfillmentState
    { fulfillmentStateDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show stateCode,
              descriptorShortDesc = Nothing,
              descriptorName = Nothing
            }
    }
