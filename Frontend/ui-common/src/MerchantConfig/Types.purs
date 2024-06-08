{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Common.Types.Config where


type CommonAppConfig = (
    colors :: Colors
  , primaryButtonConfig :: PrimaryButtonConfig
  , fontConfig :: FontConfig
  , loaderConfig :: LoaderConfig
  , currency :: String
  , internationalNumberEnabled :: Boolean
  , navigationAppConfig :: NavigationConfig
  , genericHeaderConfig :: GenericHeaderConfig
  , showCorporateAddress :: Boolean
  , appData :: AppDatas
  , otpRegex :: String
  , termsLink :: String
  , termsVersion :: Number
  , privacyLink :: String
  , dashboard :: DashboardConfig
  , logFunctionCalls :: Boolean
  , bannerCarousel :: BannerCarousalConfig 
  , defaultLanguage :: String)
  
type Colors = {
  black800 :: String
, black900 :: String
, red :: String
}

type PrimaryButtonConfig = {
  isGradient :: Boolean
, gradient :: Array String
, loaderUrl :: String
}

type FontConfig = {
    default :: String
  , kannada :: String
  , type :: String
}

type LoaderConfig = {
  color :: String
}

type Miscellaneous = {
  otpRegex :: String
, termsLink :: String
, privacyLink :: String
}

type NavigationConfig = {
  android :: NavigationAppConfig
, ios :: NavigationAppConfig
}

type NavigationAppConfig = {
  query :: String
, packageName :: String
, walkQuery :: String
}

type GenericHeaderConfig = {
  backArrow :: String
}

type AppDatas = {
    link :: String
  , supportMail :: String
  , name :: String
  , website :: String
}

type CityConfig = {
  cityName :: String,
  mapImage :: String,
  cityCode :: String,
  showSubscriptions :: Boolean,
  cityLat :: Number,
  cityLong :: Number,
  supportNumber :: String,
  languageKey :: String,
  showDriverReferral :: Boolean,
  showCustomerReferral :: Boolean,
  uploadRCandDL :: Boolean,
  enableYatriCoins :: Boolean,
  registration :: RegistrationConfig,
  vehicleNSImg :: String,
  variantSubscriptionConfig :: VariantSubscriptionConfig
}

type DashboardConfig = {
    url :: String
  , enable :: Boolean
}

type RegistrationConfig = {
  supportWAN :: String,
  callSupport :: Boolean,
  whatsappSupport :: Boolean
}

type BannerCarousalConfig = {
  autoScrollDelay :: Number
, enableAutoScroll :: Boolean
}

type VariantSubscriptionConfig = {
  enableVariantBasedSubscription :: Boolean,
  variantList :: Array String
}