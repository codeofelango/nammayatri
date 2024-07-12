module Screens.EarningsScreen.Weekly.Controller where

import Constants.Configs (dummyDistance)
import Data.Array as DA
import Data.Int (toNumber)
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Screens.EarningsScreen.ScreenData
import Services.API (RidesSummary(..))
import Debug
import Storage

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToDaily State

data Action
  = NextClick
  | BackClick
  | ChangeTab
  | ToggleInfoView
  | RemovePopup
  | ShowTips
  | ShowAdjustments
  | RideSummaryResponseAction (Array RidesSummary) String (Array String)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State

eval (RideSummaryResponseAction ridesSummaryList todaysDate datesList) state = do
  let earningList = mapSummaryListWithWeeklyEarnings ridesSummaryList
      earningListWithMissingDates = getEarningListWithMissingDates earningList datesList
      dayOfWeek = HU.getDayOfWeek (EHC.getDayName todaysDate)
      noOfDaysToNearestSunday = 6 - dayOfWeek
      datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> EHC.getFutureDate todaysDate x) (1 DA... noOfDaysToNearestSunday) else []
      earningListWithFuture = getEarningListWithMissingDates [] datesUptoMearestSunday
      currentWeekEarnings = earningListWithMissingDates <> earningListWithFuture
      currentWeekData = getWeeklyEarningsPercentage currentWeekEarnings
      currWeekMaxEarning = DA.foldl getMax 0 currentWeekData
  -- let
  --   earningList = mapSummaryListWithWeeklyEarnings ridesSummaryList
  --   earningListWithMissingDates = getEarningListWithMissingDates earningList datesList
  --   dataToFetchAndUpdateFromCache = getEarningsToCache earningListWithMissingDates
  --   dayOfWeek = getDayOfWeek (getDayName todaysDate)
  --   earningsToCache = DA.reverse (DA.take (21 + dayOfWeek) (DA.reverse dataToFetchAndUpdateFromCache))
  --   earningstoCacheWithDriverId = ST.CachedEarningsForDriver { id: getValueToLocalStore DRIVER_ID, earningsData: earningsToCache }
  -- _ <- pure $ saveObject "RIDE_SUMMARY_DATA" earningstoCacheWithDriverId
  -- let
  --   _ = fetchWeekyEarningData RIDE_SUMMARY_DATA
  --   noOfDaysToNearestSunday = 6 - dayOfWeek
  --   datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> getFutureDate todaysDate x) (1 .. noOfDaysToNearestSunday) else []
  --   allWeeksData = getAllWeeksData earningsToCache earningListWithMissingDates datesUptoMearestSunday
  --   currentWeekData = getWeeklyEarningsPercentage (getEarningDataOfWeek state.props.weekIndex allWeeksData)
  --   currWeekMaxEarning = foldl getMax 0 currentWeekData
  continue state { props { currWeekData = currentWeekData, currentWeekMaxEarning = currWeekMaxEarning }} --{ data { weeklyEarningData = allWeeksData }, props { currWeekData = currentWeekData, totalEarningsData = getTotalCurrentWeekData currentWeekData, currentWeekMaxEarning = currWeekMaxEarning } }

eval NextClick state = exit $ NextScreen state

eval ChangeTab state = exit $ GoToDaily state

eval ToggleInfoView state = continue state { props { rideDistanceInfoPopUp = not state.props.rideDistanceInfoPopUp } }

eval RemovePopup state = continue state { props { rideDistanceInfoPopUp = not state.props.rideDistanceInfoPopUp } }

eval ShowAdjustments state = continue state { data { prevAdjustmentRotation = state.data.adjustmentRotation, adjustmentRotation = if state.data.adjustmentRotation == 0.0 then 180.0 else 0.0 } }

eval _ state = update state


mapSummaryListWithWeeklyEarnings :: Array RidesSummary -> Array WeeklyEarning
mapSummaryListWithWeeklyEarnings ridesSummaryList =
  map
    ( \(RidesSummary rideSummary) ->
        { earnings: rideSummary.earnings
        , earningsWithCurrency: rideSummary.earningsWithCurrency
        , rideDistance: rideSummary.rideDistance
        , rideDistanceWithUnit: rideSummary.rideDistanceWithUnit
        , rideDate: rideSummary.rideDate
        , noOfRides: rideSummary.noOfRides
        , percentLength: 0.0
        }
    )
    ridesSummaryList

getEarningListWithMissingDates :: Array WeeklyEarning -> Array String -> Array WeeklyEarning
getEarningListWithMissingDates earningLst dates = do
  let
    x = DA.catMaybes $ map (\d -> (getEarningForDate earningLst d)) dates
  DA.sortBy (comparing _.rideDate) (earningLst <> x)

getEarningForDate :: Array WeeklyEarning -> String -> Maybe WeeklyEarning
getEarningForDate earningLst date =
  let
    foundDate = DA.find (\e -> e.rideDate == date) earningLst
  in
    maybe (Just { earnings: 0, earningsWithCurrency: HU.dummyPriceForCity (getValueToLocalStore DRIVER_LOCATION), rideDistance: 0, rideDate: date, rideDistanceWithUnit: dummyDistance, noOfRides: 0, percentLength: 0.0 }) (\_ -> Nothing) foundDate

getWeeklyEarningsPercentage :: Array WeeklyEarning -> Array WeeklyEarning
getWeeklyEarningsPercentage weeklyEarningData = map (\x -> x { percentLength =((toNumber x.earnings) * 100.0) / (toNumber maxValue) }) weeklyEarningData
  where
  maxValue = DA.foldl getMax 0 weeklyEarningData

getMax :: Int -> WeeklyEarning -> Int
getMax num1 obj1 = max obj1.earnings num1