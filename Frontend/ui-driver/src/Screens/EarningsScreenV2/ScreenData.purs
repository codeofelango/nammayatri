module Screens.EarningsScreen.ScreenData where

import Common.Types.App (Price(..), Currency(..), Distance(..), DistanceUnit(..))
import Prelude
import Screens.EarningsScreen.Common.Types
import Halogen.VDom.DOM.Prop (PropValue)
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe
import PrestoDOM.List as List
import Styles.Colors as Color

data HistoryScreen = Payout | Ride

type State
  = { data :: Data
    , props :: Props
    }

type Data
  = { action :: Boolean
    , tipsRotation :: Number
    , adjustmentRotation :: Number
    , prevAdjustmentRotation :: Number
    , rideListItem :: Maybe List.ListItem
    , payoutListItem :: Maybe List.ListItem
    }

type Props
  = { actionProp :: String
    , showInfoView :: Boolean
    , rideDistanceInfoPopUp :: Boolean
    , selectedBarIndex :: Int
    , currentWeekMaxEarning :: Int
    , currWeekData :: Array WeeklyEarning
    , fromDate:: String
    , toDate:: String
    }

initialState :: State
initialState =
  { data:
      { action: false
      , tipsRotation: 270.0
      , adjustmentRotation: 0.0
      , prevAdjustmentRotation: 0.0
      , rideListItem : Nothing
      , payoutListItem : Nothing
      }
  , props:
      { actionProp: ""
      , showInfoView: true
      , rideDistanceInfoPopUp: false
      , selectedBarIndex: 0
      , currentWeekMaxEarning: 1500
      , currWeekData: dummyEarnings
      , fromDate: ""
      , toDate: ""
      }
  }

type WeeklyEarning = { 
    earnings :: Int
  , earningsWithCurrency :: Price
  , rideDistance :: Int
  , rideDistanceWithUnit :: Distance
  , rideDate :: String
  , noOfRides :: Int
  , percentLength :: Number
  }

dummyEarnings =
  [ { earnings: 1500
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 100.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 50.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 30
    , percentLength: 20.0
    }
  , { earnings: 100
    , earningsWithCurrency : dummyEarningsWithCurrency
    , rideDistance: 100
    , rideDistanceWithUnit: dummyRideDistanceWithUnit
    , rideDate: "12"
    , noOfRides: 100
    , percentLength: 50.0
    }
  ]

dummyWeeklyEarnings =
  { earnings: 100
  , earningsWithCurrency : dummyEarningsWithCurrency
  , rideDistance: 100
  , rideDistanceWithUnit: dummyRideDistanceWithUnit
  , rideDate: "12"
  , noOfRides: -1
  , percentLength: 50.0
  }

dummyEarningsWithCurrency :: Price
dummyEarningsWithCurrency = {amount: 100.0, currency: INR}

dummyRideDistanceWithUnit :: Distance
dummyRideDistanceWithUnit = Distance {value: 100.0, unit: Kilometer}

type ListProps = {
  serviceTier :: PropValue
, rideDate :: PropValue
, rideTime :: PropValue
, rideFare :: PropValue
, vehicleImage :: PropValue
, tagVisibilityPenality :: PropValue
, tagTextPenality :: PropValue
, tagVisibilityCancellation :: PropValue
, tagTextCancellation :: PropValue
, tagVisibilityTips :: PropValue
, tagTextTips :: PropValue
, vehicleImageVisibility :: PropValue
, rideFareColor :: PropValue
}

dummyProps :: Array ListProps
dummyProps = [{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "visible"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "visible"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "visible"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "visible"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.black
}]

dummyPayoutProps :: Array ListProps
dummyPayoutProps = [{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},
{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "gone"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
},{
  serviceTier : toPropValue $ "SUV",
  vehicleImageVisibility : toPropValue "gone"
, rideDate : toPropValue $ "Today"
, rideTime : toPropValue $ "5.65"
, rideFare : toPropValue $ "$200"
, vehicleImage : toPropValue $ "ny_ic_suv_ac_side"
, tagVisibilityPenality : toPropValue $ "gone"
, tagTextPenality : toPropValue $ "Early Penality"
, tagVisibilityCancellation : toPropValue $ "gone"
, tagTextCancellation : toPropValue $ "Passenger Cancelled"
, tagVisibilityTips : toPropValue $ "visible"
, tagTextTips : toPropValue $ "+ $5 tip earned"
, rideFareColor : toPropValue $ Color.green700
}]