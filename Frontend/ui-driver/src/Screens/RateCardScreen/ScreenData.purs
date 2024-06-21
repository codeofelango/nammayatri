
module Screens.RateCardScreen.ScreenData where

import Prelude
import Screens.Types(RateCardScreenState)
import ConfigProvider (getAppConfig, appConfig)
import Screens.BookingOptionsScreen.ScreenData as BOP
import Screens.RegistrationScreen.ScreenData as RSD
import Resource.Constants as RC

initData :: RateCardScreenState
initData = { 
    data: { 
        ridePreferences : [],
        rateCard : BOP.dummyRateCard,
        cityConfig : RSD.dummyCityConfig
        -- config: getAppConfig appConfig
     }, 
    props: { 
        sliderVal : RC.defaultSliderDist,
        showRateCard : false,
        sliderDefVal : RC.defaultSliderDist,
        incrementUnit : 1,
        sliderMinValue :1,
        sliderMaxValue : 50,
        sliderLoading : false
     }
    }