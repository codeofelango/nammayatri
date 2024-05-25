module RemoteConfig.Utils where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import Common.RemoteConfig (fetchRemoteConfigString, getCityBasedConfig, defaultRemoteConfig)
import Data.Maybe (Maybe(..))
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)
import Common.Types.App
import RemoteConfig.Types (SafetyVideoConfig)


safetyVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city

safetyBannerVideoConfigData :: String -> String -> Array SafetyVideoConfig
safetyBannerVideoConfigData city language = do
    let config = fetchRemoteConfigString ("safety_banner_videos_" <> language)
        value = decodeForeignObject (parseJSON config) $ defaultRemoteConfig []
    getCityBasedConfig value city