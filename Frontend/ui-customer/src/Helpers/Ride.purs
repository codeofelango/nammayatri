{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Ride where

import Prelude
import Types.App (FlowBT, ScreenType(..))
import Control.Monad.Except (runExcept)
import JBridge
import Presto.Core.Types.Language.Flow (getLogFields)
import ModifyScreenState (modifyScreenState)
import Control.Monad.Except.Trans (lift)
import Services.Backend as Remote
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Data.Either (Either(..))
import Services.API
import Data.Array (any, null, head, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, maybe', maybe)
import Screens.HomeScreen.ScreenData (dummyRideBooking, initData) as HSD
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getFareProductType)
import Helpers.TipConfig (setTipViewData)
import Data.Lens ((^.))
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Accessor
import Screens.Types (Stage(..), SearchResultType(..), PopupType(..), FlowStatusData(..))
import Engineering.Helpers.Commons (liftFlow, convertUTCtoISC)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams)
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Helpers.Utils (getCurrentDate, getCityNameFromCode)
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromBooking)
import Data.String (split, Pattern(..))
import Foreign.Generic (decodeJSON)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Common.Types.App as Common
import Helpers.SpecialZoneAndHotSpots (getSpecialTag)
import Screens.Types (FareProductType(..)) as FPT

checkRideStatus :: Boolean -> FlowBT String Unit --TODO:: Need to refactor this function
checkRideStatus rideAssigned = do
  logField_ <- lift $ lift $ getLogFields
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "true"
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) then do
        (GlobalState state') <- getState
        let state = state'.homeScreen
            multipleScheduled = length listResp.list > 1
            (RideBookingRes resp) = (fromMaybe HSD.dummyRideBooking (head listResp.list))
            status = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._status
            bookingStatus = resp.status
            fareProductType = getFareProductType ((resp.bookingDetails) ^. _fareProductType)
            rideStatus = if status == "NEW" || (bookingStatus == "CONFIRMED" && fareProductType == FPT.ONE_WAY_SPECIAL_ZONE) then RideAccepted else if status == "INPROGRESS" then RideStarted else HomeScreen
            otpCode = ((resp.bookingDetails) ^. _contents ^. _otpCode)
            rideScheduledAt = if bookingStatus == "CONFIRMED" then fromMaybe "" resp.rideScheduledTime else ""
            dropLocation = if (fareProductType == FPT.RENTAL) then _stopLocation else _toLocation
            stopLocationDetails = (resp.bookingDetails ^._contents^._stopLocation)
            newState = 
              state
                { data
                    { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) (fareProductType == FPT.ONE_WAY_SPECIAL_ZONE)
                    , finalAmount = fromMaybe 0 $ (fromMaybe dummyRideAPIEntity (head resp.rideList) )^. _computedPrice
                    , sourceAddress = getAddressFromBooking resp.fromLocation
                    , destinationAddress = getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.dropLocation))
                    , fareProductType = fareProductType
                    , vehicleVariant = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._vehicleVariant
                    , startedAtUTC = fromMaybe "" resp.rideStartTime
                    , rentalsInfo = (if rideScheduledAt == "" then Nothing else (Just{
                        rideScheduledAtUTC : rideScheduledAt
                      , bookingId : resp.id
                      , multipleScheduled : multipleScheduled
                      , fareProductType : fareProductType
                      , nearestRideScheduledAtUTC : ""
                      , vehicleVariant : fromMaybe "" resp.vehicleServiceTierType
                      }))},
                  props
                    { currentStage = rideStatus
                    , rideRequestFlow = true
                    , bookingId = resp.id
                    , isPopUp = NoPopUp
                    , stopLoc = maybe (Nothing) (\loc -> Just {
                        lat : loc^._lat ,
                        lng : loc^._lon,
                        stopLocAddress : decodeAddress (Booking loc)
                      }) stopLocationDetails
                    , zoneType = getSpecialTag resp.specialLocationTag
                    , showAcWorkingPopup = rideStatus == RideStarted
                  }
                }
        setValueToLocalStore IS_SOS_ACTIVE $ show $ Just Common.Pending == resp.sosStatus
        if rideStatus == HomeScreen then do 
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{rentalsInfo = (if rideScheduledAt == "" then Nothing else (Just{
                        rideScheduledAtUTC : rideScheduledAt
                      , bookingId : resp.id
                      , multipleScheduled : multipleScheduled
                      , fareProductType : fareProductType
                      , nearestRideScheduledAtUTC : ""
                      , vehicleVariant : fromMaybe "" resp.vehicleServiceTierType
                      }))}})
          updateLocalStage HomeScreen
        else do
          when (not rideAssigned) $ do
            lift $ lift $ liftFlow $ logEvent logField_ "ny_active_ride_with_idle_state"
            void $ pure $ logEventWithTwoParams logField_ "ny_active_ride_with_idle_state" "status" status "bookingId" resp.id
          modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
          updateLocalStage rideStatus
          maybe' (\_ -> pure unit) updateFlowStatusData (getFlowStatusData "LazyCheck")
          let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
              (RideBookingDetails contents) = bookingDetails.contents
              otpCode = contents.otpCode
              rideListItem = head resp.rideList
          case rideListItem of
            Nothing -> do
              case otpCode of
                Just otp' -> do
                  setValueToLocalStore TRACKING_ENABLED "True"
                  modifyScreenState $ HomeScreenStateType (\homeScreen → 
                    homeScreen
                    { props
                      { isSpecialZone = true
                      , isInApp = true
                      }
                    , data
                      { driverInfoCardState
                        { otp = otp' }
                      }
                    })
                Nothing -> pure unit
            Just (RideAPIEntity _) ->
              if isJust otpCode then do
                setValueToLocalStore TRACKING_ENABLED "True"
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true,isInApp = true }}) else
                pure unit
      else if ((getValueToLocalStore RATING_SKIPPED) == "false") then do
        updateLocalStage HomeScreen
        rideBookingListResponse <- lift $ lift $ Remote.rideBookingListWithStatus "1" "0" "COMPLETED" Nothing
        case rideBookingListResponse of
          Right (RideBookingListRes listResp) -> do
            let (RideBookingRes resp) = fromMaybe HSD.dummyRideBooking $ head listResp.list
                (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                (RideBookingDetails contents) = bookingDetails.contents
                (RideAPIEntity currRideListItem) = fromMaybe dummyRideAPIEntity $ head resp.rideList
                differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 currRideListItem.chargeableRideDistance)
                lastRideDate = (case currRideListItem.rideStartTime of
                                Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                Nothing        -> "")
                currentDate =  getCurrentDate ""
            setValueToLocalStore IS_SOS_ACTIVE $ show false
            if(lastRideDate /= currentDate) then do
              setValueToLocalStore FLOW_WITHOUT_OFFERS "true"
              setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "4"
              setValueToLocalStore TEST_POLLING_INTERVAL "8000.0"
              setValueToLocalStore TEST_POLLING_COUNT "22"
              setValueToLocalStore CONFIRM_QUOTES_POLLING_COUNT "100"
              pure unit
              else pure unit
            when (isNothing currRideListItem.rideRating) $ do
              when (length listResp.list > 0) $ do
                let nightSafetyFlow = showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime
                    fareProductType = getFareProductType ((resp.bookingDetails) ^. _fareProductType)
                    (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                    (RideBookingDetails contents) = bookingDetails.contents
                    (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                    props { currentStage = RideCompleted
                          , estimatedDistance = contents.estimatedDistance
                          , zoneType = getSpecialTag resp.specialLocationTag
                          , nightSafetyFlow = nightSafetyFlow
                          , showOfferedAssistancePopUp = resp.hasDisability == Just true
                          }
                  , data { rideRatingState
                          { driverName = currRideListItem.driverName
                          , rideId = currRideListItem.id
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , source = decodeAddress (Booking resp.fromLocation)
                          , destination = (decodeAddress (Booking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^._toLocation))))
                          , vehicleNumber = (currRideListItem.vehicleNumber)
                          , status = (currRideListItem.status)
                          , shortRideId = currRideListItem.shortRideId
                          , rideEndTimeUTC = ""
                          , offeredFare = resp.estimatedTotalFare
                          , distanceDifference = differenceOfDistance
                          , bookingId = resp.id
                          , feedback = ""
                          , rideStartTime = case currRideListItem.rideStartTime of
                                              Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                              Nothing        -> ""
                          , rideEndTime   = case currRideListItem.rideEndTime of
                                              Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                              Nothing        -> ""
                          , rideStartDate = case currRideListItem.rideStartTime of
                                              Just startTime ->( (fromMaybe "" (head (split (Pattern ",") (convertUTCtoISC startTime "llll")) )) <> ", " <>  (convertUTCtoISC startTime "Do MMM") )
                                              Nothing        -> ""
                          , dateDDMMYY =  case currRideListItem.rideStartTime of
                                            Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                            Nothing        -> ""
                          }
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , driverInfoCardState {
                            price = resp.estimatedTotalFare,
                            rideId = currRideListItem.id,
                            providerType = maybe Common.ONUS (\valueAdd -> if valueAdd then Common.ONUS else Common.OFFUS) resp.isValueAddNP,
                            rentalData 
                                { finalDuration = (fromMaybe 0 resp.duration) / (60)
                                , finalDistance = (fromMaybe 0 ride.chargeableRideDistance)/1000
                                , baseDuration = (fromMaybe 0 resp.estimatedDuration) / (60*60)
                                , baseDistance = (fromMaybe 0 resp.estimatedDistance) / 1000
                                , rideStartedAt = case currRideListItem.rideStartTime of
                                                    Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                                    Nothing        -> ""
                                , rideEndedAt = case currRideListItem.rideEndTime of
                                              Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                              Nothing        -> ""
                                }
                          }
                          , ratingViewState { rideBookingRes = (RideBookingRes resp), issueFacedView = nightSafetyFlow}
                          , vehicleVariant = currRideListItem.vehicleVariant
                          , fareProductType = fareProductType
                          }
                })
                updateLocalStage RideCompleted
              when (length listResp.list == 0) $ do 
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
                updateLocalStage HomeScreen
          Left err -> do 
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
            updateLocalStage HomeScreen
      else do
        modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
        updateLocalStage HomeScreen
    Left err -> do 
      modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
      updateLocalStage HomeScreen
  if not (any isLocalStageOn [RideAccepted, RideStarted]) then removeChatService "" else pure unit
  where 
    updateFlowStatusData :: FlowStatusData -> FlowBT String Unit
    updateFlowStatusData (FlowStatusData flowStatusData) = 
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ city = getCityNameFromCode flowStatusData.source.city
                                                                              , locateOnMapProps{ sourceLocationName = flowStatusData.source.address
                                                                                                , sourceGeoJson = flowStatusData.sourceGeoJson
                                                                                                , sourceGates = flowStatusData.sourceGates } }})
        

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  let state = HomeScreenData.initData.data
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  modifyScreenState $ HomeScreenStateType (\homeScreen -> 
    homeScreen{
      props{sendMessageActive = false, chatcallbackInitiated = false, unReadMessages = false, openChatScreen = false, showChatNotification = false, canSendSuggestion = true, isChatNotificationDismissed = false, isNotificationExpanded = false, removeNotification = true, enableChatWidget = false},
      data{messages = [], messagesSize = "-1", chatSuggestionsList = [], messageToBeSent = "", lastMessage = state.lastMessage, waitTimeInfo = false, lastSentMessage = state.lastSentMessage, lastReceivedMessage = state.lastReceivedMessage}})

getFlowStatusData :: String -> Maybe FlowStatusData
getFlowStatusData dummy =
  case runExcept (decodeJSON (getValueToLocalStore FLOW_STATUS_DATA) :: _ FlowStatusData) of
    Right res -> Just res
    Left err -> Nothing

showNightSafetyFlow :: Maybe Boolean -> Maybe String -> Maybe String -> Boolean
showNightSafetyFlow hasNightIssue rideStartTime rideEndTime = not (fromMaybe true hasNightIssue) && (isNightRide rideStartTime || isNightRide rideEndTime)

isNightRide :: Maybe String -> Boolean
isNightRide = maybe false (\time -> withinTimeRange "21:00:00" "06:00:00" $ convertUTCtoISC time "HH:mm:ss")