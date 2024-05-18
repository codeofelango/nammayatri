module Components.RideCompletedCard.Controller where

import Prelude

import Components.Banner as Banner
import Components.PrimaryButton.Controller as PB
import Components.SelectListModal.Controller as SL
import Data.Int (toNumber)
import Helpers.Utils (parseFloat)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd)
import Prim.TypeError as String
import Common.Styles.Colors as Color
import Font.Style (Style (..))
import Components.PopUpModal as PopUpModal
import Halogen.VDom.DOM.Prop (Prop)
import Common.Types.App (LazyCheck(..))
import Font.Style as FontStyle
import Styles.Types (FontStyle)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Data.Maybe
import Styles.Types
import Common.Types.App (RentalBookingConfig)

data Action = Support
            | RideDetails
            | SelectButton Int
            | IssueReportIndex Int
            | RateClick Int
            | IssueReportPopUpAC SL.Action
            | SkipButtonActionController PB.Action
            | ContactSupportPopUpAC PopUpModal.Action
            | UpiQrRendered String
            | BannerAction Banner.Action 
            | HelpAndSupportAC
            | GoToSOS
            | NoAction

type RentalRideTextConfig = {
  rideTime :: String,
  rideDistance :: String,
  rideStart :: String,
  rideStartedAt :: String,
  rideEnd :: String,
  rideEndedAt :: String,
  odometerReading :: String
}

type Config = {
  isDriver :: Boolean,
  topCard :: TopCard,
  customerIssueCard :: CustomerIssueCard,
  customerBottomCard :: CustomerBottomCard,
  driverBottomCard :: DriverBottomCard,
  contactSupportPopUpConfig :: PopUpModal.Config,
  badgeCard :: BadgeCard,
  showContactSupportPopUp :: Boolean,
  driverUpiQrCard :: DriverUpiQrCard,
  noVpaCard :: NoVpaCard,
  primaryButtonConfig :: PB.Config,
  accessibility :: Accessiblity,
  theme :: Theme,
  isPrimaryButtonSticky :: Boolean,
  bannerConfig :: Banner.Config,
  viewsByOrder :: Array RideCompletedElements,
  enableContactSupport :: Boolean,
  isFreeRide :: Boolean,
  needHelpText :: String,
  lottieQRAnim :: LottieQRAnim,
  showSafetyCenter :: Boolean,
  safetyTitle :: String,
  rentalRideConfig :: RentalRideConfig,
  rentalRideTextConfig :: RentalRideTextConfig,
  capacity :: Maybe Int,
  serviceTierAndAC :: String,
  toll :: Toll,
  rentalRowDetails :: RentalRowConfig,
  rentalBookingData :: RentalBookingConfig,
  showRentalRideDetails :: Boolean
}

data Theme = DARK | LIGHT

derive instance genericTheme :: Generic Theme _
instance decodeTheme :: Decode Theme where decode = defaultEnumDecode
instance encodeTheme :: Encode Theme where encode = defaultEnumEncode
instance eqTheme :: Eq Theme where eq = genericEq

data RideCompletedElements = BANNER | QR_VIEW | NO_VPA_VIEW | BADGE_CARD | DRIVER_BOTTOM_VIEW | RENTAL_RIDE_VIEW

derive instance genericRideCompletedElements :: Generic RideCompletedElements _
instance eqRideCompletedElements :: Eq RideCompletedElements where eq = genericEq

data RentalRowView = RideTime | RideDistance | RideStartedAt | RideEndedAt | EstimatedFare | ExtraTimePrice | TotalFare

derive instance genericRentalRowView :: Generic RentalRowView _
instance eqRentalRowView :: Eq RentalRowView where eq = genericEq

type RentalTextConfig = {
  title :: String,
  estimatedValue :: String,
  actualValue :: String,
  color :: String
}

config :: Config 
config = {
  isDriver : true,
  isFreeRide : false,
  capacity : Nothing,
  serviceTierAndAC : "",
  showSafetyCenter : false,
  topCard : {
    title : "",
    titleColor : Color.grey900,
    finalAmount : 0,
    initialAmount : 0,
    fareUpdatedVisiblity : false,
    gradient : [Color.black900, Color.black900, Color.pickledBlue, Color.black900],
    topPill : {
      text : "",
      background : Color.black900,
      textColor : Color.white900,
      visible : false,
      icon : Nothing
    },
    infoPill : {
      image : "",
      imageVis : GONE,
      text :  "",
      color : Color.black600,
      background : Color.transparent,
      stroke : "1," <> Color.black700,
      cornerRadius : 8.0, 
      padding :  Padding 16 12 16 12,
      margin : Margin 15 16 15 0,
      alpha : 1.0,
      fontStyle : Tags,
      visible : VISIBLE
    },
    bottomText : "",
    horizontalLineColor : Color.white900
  },
  customerIssueCard : {
    issueFaced : false, 
    reportIssueView : false,
    selectedYesNoButton : 0,
    reportIssuePopUpConfig : SL.config,
    title : "",
    subTitle : "",
    option1Text : "",
    option2Text : "",
    yesText : "",
    noText : "",
    isNightRide : false,
    showCallSupport : false,
    wasOfferedAssistanceCardView : false
  },
  customerBottomCard : {
    visible : false,
    title : "",
    subTitle : "",
    driverImage : "",
    selectedRating : 0
  },
  driverBottomCard : {
    visible : false,
    savedMoney : []
  },
  badgeCard : {
    visible : false,
    background : Color.white900,
    text1 : "",
    text1Color : Color.darkCharcoal,
    text2 : "",
    text2Color : Color.darkCharcoal,
    image : "",
    imageWidth : V 0, 
    imageHeight : V 0,
    stroke : "1," <> Color.grey900
  },
  driverUpiQrCard : {
    text : "",
    id : "",
    vpa : "",
    vpaIcon : "",
    collectCashText : ""
  },
  noVpaCard : {
    title : "",
    collectCashText : ""
  },
  contactSupportPopUpConfig : PopUpModal.config,
  showContactSupportPopUp : false,
  primaryButtonConfig : PB.config,
  accessibility : ENABLE,
  theme : DARK,
  isPrimaryButtonSticky : false,
  bannerConfig : Banner.config,
  viewsByOrder : [],
  rentalRideConfig : {
    rideStartODOReading : "",
    rideEndODOReading : "",
    baseRideDuration : "",
    baseRideDistance : "",
    actualRideDuration : "",
    actualRideDistance : "",
    startRideOdometerImage: "",
    endRideOdometerImage: ""
  },
  rentalRideTextConfig : {
    rideTime : "Ride Time",
    rideDistance : "Ride Distance",
    rideStart : "Ride Start",
    rideStartedAt : "Ride Started At",
    rideEnd : "Ride End",
    rideEndedAt : "Ride Ended At",
    odometerReading : "Odometer Reading"
  },
  enableContactSupport : true,
  lottieQRAnim : {
    visible : false,
    url : ""
  },
  needHelpText : "",
  safetyTitle : "",
  toll : {
    actualAmount : 0.0,
    text : "",
    visibility : GONE,
    textColor : Color.black600,
    imageVisibility : GONE,
    image : ""
  },
  rentalRowDetails : dummyRentalRowConfig,
  rentalBookingData : dummyRentalBookingConfig,
  showRentalRideDetails : false
}

type CustomerIssueCard = {
  issueFaced :: Boolean, 
  reportIssueView :: Boolean,
  selectedYesNoButton :: Int,
  reportIssuePopUpConfig :: SL.Config,
  title :: String,
  subTitle :: String,
  option1Text :: String,
  option2Text :: String,
  yesText :: String,
  noText :: String,
  showCallSupport :: Boolean,
  isNightRide :: Boolean,
  wasOfferedAssistanceCardView :: Boolean
}

type TopCard = {
  title :: String,
  titleColor :: String,
  finalAmount :: Int,
  initialAmount :: Int,
  fareUpdatedVisiblity :: Boolean,
  gradient :: Array String,
  topPill :: TopPill,
  infoPill :: InfoPill, 
  bottomText :: String,
  horizontalLineColor :: String
}

type InfoPill = {
  image :: String ,
  imageVis :: Visibility, 
  text :: String ,
  color :: String ,
  background :: String ,
  stroke :: String, 
  cornerRadius :: Number, 
  padding ::  Padding,
  margin :: Margin,
  alpha :: Number,
  fontStyle :: Style,
  visible :: Visibility
}

type CustomerBottomCard = {
  visible :: Boolean,
  title :: String,
  subTitle :: String,
  driverImage :: String, 
  selectedRating :: Int
}

type DriverBottomCard = {
  visible :: Boolean,
  savedMoney :: Array SavedMoney
}

type SavedMoney = {
  amount :: Int
, reason :: String 
}

type BadgeCard = {
  visible :: Boolean,
  background :: String,
  text1 :: String,
  text1Color :: String,
  text2 :: String,
  text2Color :: String,
  image :: String,
  imageWidth :: Length, 
  imageHeight :: Length,
  stroke :: String
}

type DriverUpiQrCard = {
  text :: String,
  id :: String,
  vpa :: String,
  vpaIcon :: String,
  collectCashText :: String
}

type NoVpaCard = {
  title :: String,
  collectCashText :: String
}

type TopPill = {
  visible :: Boolean,
  background :: String,
  text :: String,
  textColor :: String,
  icon :: Maybe String
}

type LottieQRAnim = {
  visible :: Boolean,
  url :: String
}

type InfoCardConfig = {
  height :: Length,
  width :: Length,
  margin :: Margin,
  image :: InfoCardImageConfig , 
  heading :: InfocardTextConfig,
  subHeading1 :: InfocardTextConfig,
  subHeading2 :: InfocardTextConfig,
  id :: String
}

type InfoCardImageConfig = {
  width :: Length,
  height :: Length,
  visibility :: Visibility,
  renderImage :: String
}

type InfocardTextConfig = {
  text :: String,
  color :: String,
  fontStyle :: forall properties. (Array (Prop properties)),
  visibility :: Visibility
}

type RentalRideConfig = {
  rideStartODOReading :: String,
  rideEndODOReading :: String,
  baseRideDuration :: String,
  baseRideDistance :: String,
  actualRideDuration :: String,
  actualRideDistance :: String,
  startRideOdometerImage:: String,
  endRideOdometerImage:: String
}

type Toll = {
  actualAmount :: Number
, text :: String
, visibility :: Visibility
, textColor :: Color
, imageVisibility :: Visibility
, image :: String
}

type RentalRowConfig = {
    rideTime :: String
  , rideDistance :: String
  , rideStartedAt :: String
  , rideEndedAt :: String
  , estimatedFare :: String
  , extraTimePrice :: String
  , totalFare :: String
  , rideDetailsTitle :: String
  , fareUpdateTitle :: String
}

dummyRentalRowConfig :: RentalRowConfig
dummyRentalRowConfig = {
    rideTime : ""
  , rideDistance : ""
  , rideStartedAt : ""
  , rideEndedAt : ""
  , estimatedFare : ""
  , extraTimePrice : ""
  , totalFare : ""
  , rideDetailsTitle : ""
  , fareUpdateTitle : ""
}

dummyRentalBookingConfig :: RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : "" 
  , rideEndedAt : ""
  }
