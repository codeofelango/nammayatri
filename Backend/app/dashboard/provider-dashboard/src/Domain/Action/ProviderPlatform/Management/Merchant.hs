{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Merchant
  ( postMerchantUpdate,
    postMerchantServiceConfigMapsUpdate,
    getMerchantConfigCommon,
    postMerchantConfigCommonUpdate,
    getMerchantConfigDriverPool,
    postMerchantConfigDriverPoolUpdate,
    postMerchantConfigDriverPoolCreate,
    getMerchantConfigDriverIntelligentPool,
    postMerchantConfigDriverIntelligentPoolUpdate,
    getMerchantConfigOnboardingDocument,
    postMerchantConfigOnboardingDocumentUpdate,
    postMerchantConfigOnboardingDocumentCreate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate,
    postMerchantConfigFarePolicyPerExtraKmRateUpdate,
    postMerchantConfigFarePolicyUpdate,
    postMerchantConfigFarePolicyUpsert,
    postMerchantSchedulerTrigger,
    postMerchantUpdateOnboardingVehicleVariantMapping,
  )
where

import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import qualified Dashboard.ProviderPlatform.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Validation
import qualified Lib.Types.SpecialLocation
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes)
postMerchantUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateMerchantUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantUpdate) req)

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantServiceConfigMapsUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantServiceConfigMapsUpdate) req)

getMerchantConfigCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)
getMerchantConfigCommon merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.getMerchantConfigCommon)

postMerchantConfigCommonUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigCommonUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateMerchantCommonConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigCommonUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigCommonUpdate) req)

getMerchantConfigDriverPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.Meters) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes)
getMerchantConfigDriverPool merchantShortId opCity apiTokenInfo distanceUnit tripDistance tripDistanceValue = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.getMerchantConfigDriverPool) distanceUnit tripDistance tripDistanceValue

postMerchantConfigDriverPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Dashboard.Common.Variant) -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolUpdate merchantShortId opCity apiTokenInfo distanceUnit tripCategory tripDistanceValue vehicleVariant area tripDistance req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateDriverPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigDriverPoolUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverPoolUpdate) distanceUnit tripCategory tripDistanceValue vehicleVariant area tripDistance req)

postMerchantConfigDriverPoolCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Prelude.Maybe (Dashboard.Common.Variant) -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolCreate merchantShortId opCity apiTokenInfo distanceUnit tripCategory tripDistanceValue vehiclevariant area tripDistance req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateDriverPoolConfigCreateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigDriverPoolCreateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverPoolCreate) distanceUnit tripCategory tripDistanceValue vehiclevariant area tripDistance req)

getMerchantConfigDriverIntelligentPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)
getMerchantConfigDriverIntelligentPool merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.getMerchantConfigDriverIntelligentPool)

postMerchantConfigDriverIntelligentPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverIntelligentPoolUpdate merchantShortId opCity apiTokenInfo req = do
  Kernel.Utils.Validation.runRequestValidation Dashboard.ProviderPlatform.Merchant.validateDriverIntelligentPoolConfigUpdateReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigDriverIntelligentPoolUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigDriverIntelligentPoolUpdate) req)

getMerchantConfigOnboardingDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Merchant.DocumentType) -> Kernel.Prelude.Maybe (Dashboard.Common.Category) -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes)
getMerchantConfigOnboardingDocument merchantShortId opCity apiTokenInfo documentType vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.getMerchantConfigOnboardingDocument) documentType vehicleCategory

postMerchantConfigOnboardingDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentUpdate merchantShortId opCity apiTokenInfo category documentType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigOnboardingDocumentUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigOnboardingDocumentUpdate) category documentType req)

postMerchantConfigOnboardingDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentCreate merchantShortId opCity apiTokenInfo category documentType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigOnboardingDocumentCreateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigOnboardingDocumentCreate) category documentType req)

postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantShortId opCity apiTokenInfo farePolicyId distanceUnit startDistanceValue startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate) farePolicyId distanceUnit startDistanceValue startDistance req)

postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe (Kernel.Types.Common.DistanceUnit) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecDistance) -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantShortId opCity apiTokenInfo farePolicyId distanceUnit startDistanceValue startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate) farePolicyId distanceUnit startDistanceValue startDistance req)

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantShortId opCity apiTokenInfo farePolicyId startDistance req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyPerExtraKmRateUpdate) farePolicyId startDistance req)

postMerchantConfigFarePolicyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyUpdate merchantShortId opCity apiTokenInfo farePolicyId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigFarePolicyUpdateEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantConfigFarePolicyUpdate) farePolicyId req)

postMerchantConfigFarePolicyUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq -> Environment.Flow API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp)
postMerchantConfigFarePolicyUpsert merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantConfigFarePolicyUpsertEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantConfigFarePolicyUpsert)) req)

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantSchedulerTriggerEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.merchantDSL.postMerchantSchedulerTrigger) req)

postMerchantUpdateOnboardingVehicleVariantMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMerchantUpdateOnboardingVehicleVariantMapping merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MerchantAPI Dashboard.Common.Merchant.PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.merchantDSL.postMerchantUpdateOnboardingVehicleVariantMapping)) req)
