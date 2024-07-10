{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.RiderPlatform.Management.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("merchant" :> (PostMerchantUpdate :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantConfigOperatingCityCreate :<|> PostMerchantSpecialLocationUpsert :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsert :<|> DeleteMerchantSpecialLocationGatesDelete))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMerchantUpdate merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> postMerchantServiceConfigSmsUpdate merchantId city :<|> getMerchantServiceUsageConfig merchantId city :<|> postMerchantServiceUsageConfigMapsUpdate merchantId city :<|> postMerchantServiceUsageConfigSmsUpdate merchantId city :<|> postMerchantConfigOperatingCityCreate merchantId city :<|> postMerchantSpecialLocationUpsert merchantId city :<|> deleteMerchantSpecialLocationDelete merchantId city :<|> postMerchantSpecialLocationGatesUpsert merchantId city :<|> deleteMerchantSpecialLocationGatesDelete merchantId city

type PostMerchantUpdate = (ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'MERCHANT_UPDATE :> API.Types.RiderPlatform.Management.Merchant.PostMerchantUpdate)

type PostMerchantServiceConfigMapsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'MAPS_SERVICE_CONFIG_UPDATE
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigMapsUpdate
  )

type PostMerchantServiceConfigSmsUpdate = (ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SMS_SERVICE_CONFIG_UPDATE :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceConfigSmsUpdate)

type GetMerchantServiceUsageConfig = (ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'SERVICE_USAGE_CONFIG :> API.Types.RiderPlatform.Management.Merchant.GetMerchantServiceUsageConfig)

type PostMerchantServiceUsageConfigMapsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'MAPS_SERVICE_USAGE_CONFIG_UPDATE
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigMapsUpdate
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'SMS_SERVICE_USAGE_CONFIG_UPDATE
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantServiceUsageConfigSmsUpdate
  )

type PostMerchantConfigOperatingCityCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'CREATE_MERCHANT_OPERATING_CITY
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantConfigOperatingCityCreate
  )

type PostMerchantSpecialLocationUpsert = (ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'UPSERT_SPECIAL_LOCATION :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationUpsert)

type DeleteMerchantSpecialLocationDelete = (ApiAuth 'APP_BACKEND_MANAGEMENT 'MERCHANT 'DELETE_SPECIAL_LOCATION :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationDelete)

type PostMerchantSpecialLocationGatesUpsert =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'UPSERT_SPECIAL_LOCATION_GATE
      :> API.Types.RiderPlatform.Management.Merchant.PostMerchantSpecialLocationGatesUpsert
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'MERCHANT
      'DELETE_SPECIAL_LOCATION_GATE
      :> API.Types.RiderPlatform.Management.Merchant.DeleteMerchantSpecialLocationGatesDelete
  )

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.FlowHandler Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Merchant.deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName
