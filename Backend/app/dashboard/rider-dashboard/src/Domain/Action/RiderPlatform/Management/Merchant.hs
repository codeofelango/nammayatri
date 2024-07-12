{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.RiderPlatform.Management.Merchant
  ( postMerchantUpdate,
    postMerchantServiceConfigMapsUpdate,
    postMerchantServiceConfigSmsUpdate,
    getMerchantServiceUsageConfig,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantConfigOperatingCityCreate,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
  )
where

import qualified API.Types.RiderPlatform.Management.Merchant
import qualified Dashboard.Common.Merchant
import qualified Dashboard.RiderPlatform.Merchant
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (state)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Geometry (getGeomFromKML)
import qualified Kernel.Utils.Validation
import qualified Lib.Types.SpecialLocation
import qualified RiderPlatformClient.RiderApp.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as SQM
import Tools.Auth.Api
import Tools.Auth.Merchant

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.RiderPlatform.Merchant.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantUpdateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantUpdate) req)

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceConfigMapsUpdateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigMapsUpdate) req)

postMerchantServiceConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigSmsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceConfigSmsUpdateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigSmsUpdate) req)

getMerchantServiceUsageConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow Dashboard.Common.Merchant.ServiceUsageConfigRes)
getMerchantServiceUsageConfig merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.getMerchantServiceUsageConfig)

postMerchantServiceUsageConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigMapsUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.Common.Merchant.validateMapsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceUsageConfigMapsUpdateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceUsageConfigMapsUpdate) req)

postMerchantServiceUsageConfigSmsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantServiceUsageConfigSmsUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.Common.Merchant.validateSmsServiceUsageConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceUsageConfigSmsUpdateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceUsageConfigSmsUpdate) req)

postMerchantConfigOperatingCityCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.CreateMerchantOperatingCityReq -> Environment.Flow Dashboard.Common.Merchant.CreateMerchantOperatingCityRes)
postMerchantConfigOperatingCityCreate merchantShortId opCity apiTokenInfo req@Dashboard.Common.Merchant.CreateMerchantOperatingCityReq {..} = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigOperatingCityCreateEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  -- update entry in dashboard
  merchant <- SQM.findByShortId merchantShortId >>= fromMaybeM (InvalidRequest $ "Merchant not found with shortId " <> show merchantShortId)
  geom <- getGeomFromKML req.file >>= fromMaybeM (InvalidRequest "Cannot convert KML to Geom")
  unless (req.city `elem` merchant.supportedOperatingCities) $
    SQM.updateSupportedOperatingCities merchantShortId (merchant.supportedOperatingCities <> [req.city])
  SharedLogic.Transaction.withTransactionStoring transaction $ RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigOperatingCityCreate) Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT {geom = T.pack geom, ..}

postMerchantSpecialLocationUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe ((Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)) -> Dashboard.Common.Merchant.UpsertSpecialLocationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationUpsert merchantShortId opCity apiTokenInfo specialLocationId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantSpecialLocationUpsertEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do error "Client call .merchantDSL.postMerchantSpecialLocationUpsert for separate helperApi couldn't be autogenerated. Logic yet to be decided" checkedMerchantId specialLocationId req)

deleteMerchantSpecialLocationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationDelete merchantShortId opCity apiTokenInfo specialLocationId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.DeleteMerchantSpecialLocationDeleteEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.deleteMerchantSpecialLocationDelete) specialLocationId)

postMerchantSpecialLocationGatesUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSpecialLocationGatesUpsert merchantShortId opCity apiTokenInfo specialLocationId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantSpecialLocationGatesUpsertEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do error "Client call .merchantDSL.postMerchantSpecialLocationGatesUpsert for separate helperApi couldn't be autogenerated. Logic yet to be decided" checkedMerchantId specialLocationId req)

deleteMerchantSpecialLocationGatesDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteMerchantSpecialLocationGatesDelete merchantShortId opCity apiTokenInfo specialLocationId gateName = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.DeleteMerchantSpecialLocationGatesDeleteEndpoint) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.merchantDSL.deleteMerchantSpecialLocationGatesDelete) specialLocationId gateName)
