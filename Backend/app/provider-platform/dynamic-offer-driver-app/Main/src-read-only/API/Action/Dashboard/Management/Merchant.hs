{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Merchant
  ( API.Types.ProviderPlatform.Management.Merchant.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.Dashboard.Merchant as Domain.Action.Dashboard.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Merchant.API)
handler merchantId city = postMerchantUpdate merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city :<|> getMerchantConfigCommon merchantId city :<|> postMerchantConfigCommonUpdate merchantId city :<|> getMerchantConfigDriverPool merchantId city :<|> postMerchantConfigDriverPoolUpdate merchantId city :<|> postMerchantConfigDriverPoolCreate merchantId city :<|> getMerchantConfigDriverIntelligentPool merchantId city :<|> postMerchantConfigDriverIntelligentPoolUpdate merchantId city :<|> getMerchantConfigOnboardingDocument merchantId city :<|> postMerchantConfigOnboardingDocumentUpdate merchantId city :<|> postMerchantConfigOnboardingDocumentCreate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate merchantId city :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate merchantId city :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate merchantId city :<|> postMerchantConfigFarePolicyUpdate merchantId city :<|> postMerchantConfigFarePolicyUpsert merchantId city :<|> postMerchantSchedulerTrigger merchantId city :<|> postMerchantUpdateOnboardingVehicleVariantMapping merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes)
postMerchantUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantUpdate a3 a2 a1

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceConfigMapsUpdate a3 a2 a1

getMerchantConfigCommon :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)
getMerchantConfigCommon a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigCommon a2 a1

postMerchantConfigCommonUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigCommonUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigCommonUpdate a3 a2 a1

getMerchantConfigDriverPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes)
getMerchantConfigDriverPool a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigDriverPool a5 a4 a3 a2 a1

postMerchantConfigDriverPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Dashboard.Common.Variant -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolUpdate a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigDriverPoolUpdate a9 a8 a7 a6 a5 a4 a3 a2 a1

postMerchantConfigDriverPoolCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Dashboard.Common.Variant -> Lib.Types.SpecialLocation.Area -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverPoolCreate a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigDriverPoolCreate a9 a8 a7 a6 a5 a4 a3 a2 a1

getMerchantConfigDriverIntelligentPool :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)
getMerchantConfigDriverIntelligentPool a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigDriverIntelligentPool a2 a1

postMerchantConfigDriverIntelligentPoolUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigDriverIntelligentPoolUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigDriverIntelligentPoolUpdate a3 a2 a1

getMerchantConfigOnboardingDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Kernel.Prelude.Maybe Dashboard.Common.Category -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes)
getMerchantConfigOnboardingDocument a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.getMerchantConfigOnboardingDocument a4 a3 a2 a1

postMerchantConfigOnboardingDocumentUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentUpdate a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigOnboardingDocumentUpdate a5 a4 a3 a2 a1

postMerchantConfigOnboardingDocumentCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Category -> API.Types.ProviderPlatform.Management.Merchant.DocumentType -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigOnboardingDocumentCreate a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigOnboardingDocumentCreate a5 a4 a3 a2 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate a7 a6 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate a7 a6 a5 a4 a3 a2 a1

postMerchantConfigFarePolicyPerExtraKmRateUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyPerExtraKmRateUpdate a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFarePolicyPerExtraKmRateUpdate a5 a4 a3 a2 a1

postMerchantConfigFarePolicyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantConfigFarePolicyUpdate a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFarePolicyUpdate a4 a3 a2 a1

postMerchantConfigFarePolicyUpsert :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp)
postMerchantConfigFarePolicyUpsert a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantConfigFarePolicyUpsert a3 a2 a1

postMerchantSchedulerTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantSchedulerTrigger a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantSchedulerTrigger a3 a2 a1

postMerchantUpdateOnboardingVehicleVariantMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdateOnboardingVehicleVariantMapping a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantUpdateOnboardingVehicleVariantMapping a3 a2 a1
