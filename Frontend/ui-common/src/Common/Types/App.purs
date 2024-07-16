{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Common.Types.App where

import Prelude (class Eq, class Show, ($),(<>),show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Eq.Generic (genericEq)
import Foreign (Foreign)
import Presto.Core.Types.API (standardEncode,class StandardEncode)
import Foreign.Generic (class Decode, class Encode)
import Control.Monad.Free (Free)
import Control.Monad.Except.Trans (ExceptT)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Control.Transformers.Back.Trans (BackT)
import Data.Maybe (Maybe(..))
import DecodeUtil (parseJSON)

type FlowBT e st a = BackT (ExceptT e (Free (FlowWrapper st))) a

data AmbulanceVariant = Taxi | Taxi_Oxy | AC | AC_Oxy | Ventilator
data VehicalTypes = Sedan | Hatchback | SUV | Auto | Bike | Ambulance AmbulanceVariant 
data LazyCheck = LanguageStyle | EndPoint | BaseUrl | TypoGraphy | WithoutOffers | FunctionCall | Config | Language

newtype Place = Place {
  id :: String
, address :: String
, name :: String
, lat :: String
, lng :: String
}

derive instance genericPlace :: Generic Place _
derive instance newtypePlace :: Newtype Place _
instance encodePlace :: Encode Place where encode = defaultEncode
instance decodePlace :: Decode Place where decode = defaultDecode


derive instance genericVehicalTypes :: Generic VehicalTypes _
instance decodeVehicalTypes :: Decode VehicalTypes where decode = defaultDecode
instance encodeVehicalTypes :: Encode VehicalTypes where encode = defaultEncode
instance eqVehicalTypes :: Eq VehicalTypes where eq = genericEq

derive instance genericAmbulanceVariant :: Generic AmbulanceVariant _
instance decodeAmbulanceVariant :: Decode AmbulanceVariant where decode = defaultEnumDecode
instance encodeAmbulanceVariant :: Encode AmbulanceVariant where encode = defaultEnumEncode
instance eqAmbulanceVariant :: Eq AmbulanceVariant where eq = genericEq

data NotificationType = REGISTRATION_APPROVED | SEARCH_CALLBACK | CONFIRM_CALLBACK
  | TRACKING_CALLBACK | SEARCH_REQUEST | CONFIRM_REQUEST | UPCOMING_CASE

derive instance genericNotificationType :: Generic NotificationType _
instance decodeNotificationType :: Decode NotificationType where decode = defaultEnumDecode
instance encodeNotificationType :: Encode NotificationType where encode = defaultEnumEncode

newtype NotificationData = NotificationData {
  entity_type :: Maybe String
  , entity_ids :: Maybe String
  , notification_type :: Maybe String
  }

derive instance genericNotificationData :: Generic NotificationData _
derive instance newtypeNotificationData :: Newtype NotificationData _
instance encodeNotificationData :: Encode NotificationData where encode = defaultEncode
instance decodeNotificationData :: Decode NotificationData where decode = defaultDecode

newtype SignatureAuthData = SignatureAuthData {
  signature :: String
  , authData :: String
  }

derive instance genericSignatureAuthData :: Generic SignatureAuthData _
derive instance newtypeSignatureAuthData :: Newtype SignatureAuthData _
instance encodeSignatureAuthData :: Encode SignatureAuthData where encode = defaultEncode
instance decodeSignatureAuthData :: Decode SignatureAuthData where decode = defaultDecode

type Event = {
    type :: String
  , data :: String
}

newtype GlobalPayload = GlobalPayload
  { betaAssets :: Maybe Boolean
  , payload :: Payload
  , requestId :: Maybe String
  , sdkName :: Maybe String
  , sdkVersion :: Maybe String
  , service :: Maybe String
  , service_based :: Maybe Boolean
  }

derive instance newGlobalPayload :: Newtype GlobalPayload _
derive instance genericGlobalPayload :: Generic GlobalPayload _
instance decodeGlobalPayload :: Decode GlobalPayload where decode = defaultDecode
instance encodeGlobalPayload :: Encode GlobalPayload where encode = defaultEncode

newtype Payload = Payload
  { service :: Maybe String
  , environment :: Maybe String
  , notificationData :: Maybe NotificationData
  , signatureAuthData :: Maybe SignatureAuthData
  , search_type :: Maybe String
  , source :: Maybe LocationData
  , destination :: Maybe LocationData
  , payment_method :: Maybe String
  , show_splash :: Maybe Boolean
  , view_param :: Maybe String
  , deeplinkOptions :: Maybe DeeplinkOptions
  , deepLinkJSON :: Maybe QueryParam
  }

newtype QueryParam = QueryParam {
  option :: Maybe String,
  bookingId :: Maybe String
}

newtype DeeplinkOptions = DeeplinkOptions {
    parent_view :: Maybe Boolean
  , show_title :: Maybe Boolean
}

newtype LocationData = LocationData {
    lat :: Number
  , lon :: Number
  , name :: Maybe String
}

derive instance newPayload :: Newtype Payload _
derive instance genericPayload :: Generic Payload _
instance decodePayload :: Decode Payload where decode = defaultDecode
instance encodePayload :: Encode Payload where encode = defaultEncode

derive instance newLocationData :: Newtype LocationData _
derive instance genericLocationData :: Generic LocationData _
instance decodeLocationData :: Decode LocationData where decode = defaultDecode
instance encodeLocationData :: Encode LocationData where encode = defaultEncode

derive instance newDeeplinkOptions :: Newtype DeeplinkOptions _
derive instance genericDeeplinkOptions :: Generic DeeplinkOptions _
instance decodeDeeplinkOptions :: Decode DeeplinkOptions where decode = defaultDecode
instance encodeDeeplinkOptions :: Encode DeeplinkOptions where encode = defaultEncode

derive instance genericQueryParam :: Generic QueryParam _
derive instance newQueryParam :: Newtype QueryParam _
instance decodeQueryParam :: Decode QueryParam where decode body = defaultDecode $ parseJSON body
instance encodeQueryParam :: Encode QueryParam where encode = defaultEncode

type OptionButtonList = {
    reasonCode :: String,
    description :: String,
    textBoxRequired :: Boolean,
    subtext :: Maybe String
}

newtype Version = Version
  { 
    major :: Int,
    minor :: Int,
    maintenance :: Int
  }

type CheckBoxOptions = {
    text :: String,
    subText :: String,
    value :: String,
    isSelected :: Boolean
}


derive instance genericVersion :: Generic Version _
derive instance newtypeVersion :: Newtype Version _
instance standardEncodeVersion :: StandardEncode Version where standardEncode (Version body) = standardEncode body
instance showVersion :: Show Version where show = genericShow
instance decodeVersion :: Decode Version where decode = defaultDecode
instance encodeVersion  :: Encode Version where encode = defaultEncode
instance eqVersion :: Eq Version where eq = genericEq

newtype EventPayload = EventPayload {
    event :: String
  , payload :: Maybe InnerPayload
}

type InnerPayload = {
    action :: String
  , trip_amount :: Maybe Int
  , trip_id :: Maybe String
  , screen :: Maybe String
  , exit_app :: Boolean
  , ride_status :: Maybe String
}

derive instance genericEventPayload :: Generic EventPayload _
instance encodeEventPayload  :: Encode EventPayload where encode = defaultEncode

type LayoutBound = 
  { height :: Int
  , width :: Int
}
-- newtype LocationLatLong = LocationLatLong
--   { lat :: String
--   , long :: String
--   }

-- derive instance genericLocationLatLong :: Generic (LocationLatLong )  _
-- derive instance newtypeLocationLatLong :: Newtype (LocationLatLong ) _
-- instance decodeLocationLatLong :: Decode (LocationLatLong ) where decode = defaultDecode
-- instance encodeLocationLatLong :: Encode (LocationLatLong ) where encode = defaultEncode

-- derive instance genericLocationLatLong :: Generic LocationLatLong _
-- derive instance newtypeLocationLatLong :: Newtype LocationLatLong _
-- instance encodeLocationLatLong :: Encode LocationLatLong where encode = defaultEncode
-- instance decodeLocationLatLong :: Decode LocationLatLong where decode = defaultDecode

data RateCardType = DefaultRateCard | DriverAddition | FareUpdate | PaymentFareBreakup | WaitingCharges | TollOrParkingCharges | RentalRateCard
derive instance genericRateCardType :: Generic RateCardType _
instance eqRateCardType :: Eq RateCardType where eq = genericEq
instance decodeRateCardType :: Decode RateCardType where decode = defaultEnumDecode
instance encodeRateCardType :: Encode RateCardType where encode = defaultEnumEncode

type FareList = {
  key :: String,
  val :: String
}

type ClevertapEventParams = {
  key :: String ,
  value :: Foreign
}

type DateObj = {
  date :: Int
, month :: String
, year :: Int
}

type CalendarDate = {
    date :: Int
  , utcDate :: String
  , month :: String
  , year :: Int
}

type CalendarWeek = {
    startDate :: Int
  , utcStartDate :: String
  , endDate :: Int
  , utcEndDate :: String
  , startMonth :: String
  , endMonth :: String
}

type CountryCodeObj = {
  countryName :: String 
  , countryCode :: String 
  , countryShortCode :: String
}

type ChatComponent = {
    message :: String 
  , sentBy :: String 
  , timeStamp :: String
  , type :: String
  , delay :: Int
}

data MobileNumberValidatorResp = Invalid | Valid | MaxLengthExceeded | ValidPrefix

derive instance genericMobileNumberValidatorResp :: Generic MobileNumberValidatorResp _ 
instance eqMobileNumberValidatorResp :: Eq MobileNumberValidatorResp where eq = genericEq

data OTPChannel = WHATSAPP | SMS 

derive instance genericOTPChannel :: Generic OTPChannel _
instance showOTPChannel :: Show OTPChannel where show = genericShow
instance encodeOTPChannel  :: Encode OTPChannel where encode = defaultEncode
instance eqOTPChannel :: Eq OTPChannel where eq = genericEq

type FeedbackAnswer =  {
    questionId :: String,
    answer :: Array String
  }

type ShareImageConfig = {
    viewId :: String
  , code :: String
  , logoId :: String
  , isReferral :: Boolean
}
type YoutubeData = {
    videoTitle :: String
  , setVideoTitle :: Boolean
  , showMenuButton :: Boolean
  , showDuration :: Boolean
  , showSeekBar :: Boolean
  , videoId :: String
  , videoType :: String
  , videoHeight :: Int
  , showFullScreen :: Boolean
  , hideFullScreenButton :: Boolean
}

type FCMBundleUpdate = {
  title :: String,
  description :: String,
  image :: String
}

type CalendarModalDateObject = 
  { date :: Int
  , isInRange :: Boolean
  , isStart :: Boolean
  , isEnd :: Boolean
  , utcDate :: String
  , shortMonth :: String
  , year :: Int
  , intMonth :: Int
  }

type CalendarModalWeekObject = {
  week :: Array CalendarModalDateObject
}

type ModifiedCalendarObject = {
  selectedTimeSpan :: CalendarModalDateObject,
  weeks :: Array CalendarModalWeekObject,
  startDate :: Maybe CalendarModalDateObject,
  endDate :: Maybe CalendarModalDateObject
}

type PolylineAnimationConfig = {
    color :: String 
  , draw :: Int
  , fade :: Int
  , delay ::Int
}


data YoutubeVideoStatus = PLAY | PAUSE

derive instance genericYoutubeVideoStatus:: Generic YoutubeVideoStatus _
instance showYoutubeVideoStatus :: Show YoutubeVideoStatus where show = genericShow
instance eqYoutubeVideoStatus :: Eq YoutubeVideoStatus where eq = genericEq

type CarouselModal = {
  carouselData ::  Array CarouselData,
  gravity :: String 
}

type CarouselData = {
  imageConfig :: CarouselImageConfig,
  titleConfig :: CarouselTextConfig,
  youtubeConfig :: YoutubeData,
  contentType :: String,
  gravity :: Int, 
  descriptionConfig :: CarouselTextConfig,
  backgroundColor :: String
}

type CarouselImageConfig = {
  height :: Int,
  width :: Int,
  bgColor :: String,
  cornerRadius :: Number,
  image :: String,
  isUrl :: Boolean
}

type CarouselTextConfig = {
  textSize :: Int,
  textColor :: String,
  gravity :: String,
  margin :: MarginConfig,
  text :: String
}

type MarginConfig = {
  top :: Int ,
  right :: Int,
  bottom :: Int,
  left :: Int
}

type ReelModal = {
  reelData :: Array ReelItem,
  titleConfig :: ReelTextConfig,
  descriptionConfig :: ReelTextConfig,
  reelExtraConfig :: Maybe ReelExtraConfig
}

type ReelVideoThresholdConfig = {
  isThresholdEnabled :: Maybe Boolean,
  isStartThresholdEnabled :: Maybe Boolean,
  isEndThresholdEnabled :: Maybe Boolean,
  startThreshold :: Maybe Int,
  endThreshold :: Maybe Int,
  sendCallbackAfterEverySecondEnabled :: Maybe Boolean -- by default it is taken as false
}

type ReelExtraConfig = {
  bounceAnimationEnabled :: Maybe Boolean, -- bounce animation will not work if autoSwipeToNext is enabled
  bounceAnimationCount :: Maybe Int,
  bounceAnimationDuration :: Maybe Int,
  progressBarColor :: Maybe String,
  progressBarVisible :: Maybe Boolean,
  autoSwipeToNext :: Maybe Boolean,
  seekEnabled :: Maybe Boolean
}

type ReelButtonConfig = {
    prefixImage :: Maybe String,
    suffixImage :: Maybe String,
    actions :: Maybe (Array String),
    shareLink :: Maybe String,
    shareText :: Maybe String,
    shareMessageTitle :: Maybe String,
    buttonColor :: Maybe String,
    cornerRadius :: Maybe Int,
    text :: Maybe String,
    textSize :: Maybe Int,
    textColor :: Maybe String,
    prefixImageHeight :: Maybe Int,
    prefixImageWidth :: Maybe Int,
    suffixImageHeight :: Maybe Int,
    suffixImageWidth :: Maybe Int,
    activeIndex :: Maybe String,
    inActiveIndex :: Maybe String,
    activeIndexWidth :: Maybe Int,
    activeIndexHeight :: Maybe Int,
    inActiveIndexWidth :: Maybe Int,
    inActiveIndexHeight :: Maybe Int
}

type ReelItem = {
  title :: String,
  description :: Maybe String,
  id :: String,
  shareLink :: Maybe String,
  thumbnailImageUrl :: Maybe String,
  videoUrl :: String,
  carouselBigImageUrl :: Maybe String,
  carouselSmallImageUrl :: Maybe String,
  carouselTextString :: Maybe String,
  carouselTextColor :: Maybe String,
  bottomButtonConfig :: Maybe (Array (Array ReelButtonConfig)),
  sideButtonConfig :: Maybe (Array (Array ReelButtonConfig)),
  thresholdConfig :: Maybe ReelVideoThresholdConfig
}

type ReelTextConfig = {
  size :: Int,
  color :: String,
  maxLines :: Int
}

type CarouselHolderData = {
  id :: Int
, shouldPush :: Boolean
}

type CategoryListType = {
    categoryName :: String
  , categoryImageUrl :: String
  , categoryAction :: String
  , categoryId :: String
  }

type DisplayBase64ImageConig = {
    source :: String
  , id :: String
  , scaleType :: String
  , inSampleSize :: Int -- reduce image qulaity by this factor (highValue = low quality)
}

type CircleRippleConfig = {
  delay :: Int
, duration :: Int
, pause :: Int
, repeatMode :: Int
, count :: Int
, radius :: Number
, maxRadius :: Number
, strokeWidth :: Number
, maxStrokeWidth :: Number
, fromStrokeColor :: String
, toStrokeColor :: String
, prefix :: String
, center :: Paths
}

type GroundOverlayConfig = {
  id :: String
, height :: Int
, width :: Int
, imageUrl :: String
, fetchFromView :: Boolean
, viewId :: String
, center :: Paths
}

type MarkerLabelConfig = {
  id :: String
, title :: String
, actionImage :: String
, actionCallBack :: String
, position :: Paths
, markerImage :: String
}


type Paths = {
    lat :: Number
  , lng :: Number
}

data SosStatus = Pending | Resolved | NotResolved | MockPending | MockResolved

derive instance genericSosStatus :: Generic SosStatus _
instance standardEncodeSosStatus :: StandardEncode SosStatus where standardEncode _ = standardEncode {}
instance showSosStatus :: Show SosStatus where show = genericShow
instance decodeSosStatus :: Decode SosStatus where decode = defaultEnumDecode
instance encodeSosStatus  :: Encode SosStatus where encode = defaultEnumEncode
instance eqSosStatus :: Eq SosStatus where eq = genericEq

data ProviderType = ONUS | OFFUS

derive instance genericProviderType :: Generic ProviderType _
instance eqProviderType :: Eq ProviderType where eq = genericEq
instance showProviderType :: Show ProviderType where show = genericShow
instance encodeProviderType :: Encode ProviderType where encode = defaultEnumEncode
instance decodeProviderType :: Decode ProviderType where decode = defaultEnumDecode

type Price = {
    amount :: Number
  , currency :: String
}

type WaitingTimeInfo = {
  freeMinutes :: String,
  charge :: String
}

type RentalBookingConfig = {
    startTimeUTC :: String
  , baseDuration :: Int
  , baseDistance :: Int
  , finalDuration :: Int
  , finalDistance :: Int
  , startOdometer :: String
  , endOdometer :: String
  , nightCharge :: String
  , rideStartedAt :: String 
  , rideEndedAt :: String
  , extraDistanceFare :: String 
  , extraTimeFare :: String
}




data CustomerIssueTypes = TollCharge | NightSafety | Accessibility | NoIssue | MoreIssues
derive instance genericCustomerIssueTypes :: Generic CustomerIssueTypes _
instance eqCustomerIssueTypes :: Eq CustomerIssueTypes where eq = genericEq
instance priorityCustomerIssueTypes :: Priority CustomerIssueTypes where 
  priority = case _ of
    TollCharge -> 1
    NightSafety -> 0
    Accessibility -> 2
    NoIssue -> 10
    MoreIssues -> 20


class Priority a where
  priority :: a -> Int

data Confidence = Sure | Neutral | Unsure
derive instance genericConfidence :: Generic Confidence _
instance standardEncodeConfidence :: StandardEncode Confidence where standardEncode _ = standardEncode {}
instance eqConfidence :: Eq Confidence where eq = genericEq
instance showConfidence :: Show Confidence where show = genericShow
instance decodeConfidence :: Decode Confidence where decode = defaultEnumDecode
instance encodeConfidence  :: Encode Confidence where encode =  defaultEnumEncode

newtype EstimateFares = EstimateFares {
  priceWithCurrency :: Price,
  title :: String
}

derive instance genericEstimateFares :: Generic EstimateFares _
derive instance newtypeEstimateFares :: Newtype EstimateFares _
instance standardEncodeEstimateFares :: StandardEncode EstimateFares where standardEncode (EstimateFares body) = standardEncode body
instance showEstimateFares :: Show EstimateFares where show = genericShow
instance decodeEstimateFares :: Decode EstimateFares where decode = defaultDecode
instance encodeEstimateFares  :: Encode EstimateFares where encode = defaultEncode

type BreakupList = {
  fareList :: Array FareList,
  fareInfo :: Array String,
  driverAdditions :: Array FareList,
  nightChargeStart :: String,
  nightChargeEnd :: String,
  isNightShift :: Boolean,
  waitingTimeInfo :: WaitingTimeInfo
}

type RateCard =
  {
    baseFare :: Int,
    driverAdditions :: Array FareList,
    fareInfoDescription :: Array String,
    additionalFare :: Int,
    isNightShift :: Boolean,
    nightChargeTill :: String,
    nightChargeFrom :: String,
    currentRateCardType :: RateCardType,
    onFirstPage :: Boolean,
    createdTime :: String,
    extraFare :: Array FareList,
    waitingTimeInfo :: WaitingTimeInfo,
    serviceTierName :: Maybe String
  }
