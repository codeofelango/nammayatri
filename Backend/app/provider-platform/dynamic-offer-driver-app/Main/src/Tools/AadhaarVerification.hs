{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.AadhaarVerification
  ( module Reexport,
    generateAadhaarOtp,
    verifyAadhaarOtp,
  )
where

import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Kernel.External.AadhaarVerification as Reexport hiding
  ( generateAadhaarOtp,
    verifyAadhaarOtp,
  )
import qualified Kernel.External.AadhaarVerification as Verification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error
import Tools.Metrics

generateAadhaarOtp ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id MerchantOperatingCity ->
  AadhaarOtpReq ->
  m AadhaarVerificationResp
generateAadhaarOtp = runWithServiceConfig Verification.generateAadhaarOtp

verifyAadhaarOtp ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id MerchantOperatingCity ->
  AadhaarOtpVerifyReq ->
  m AadhaarOtpVerifyRes
verifyAadhaarOtp = runWithServiceConfig Verification.verifyAadhaarOtp

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (AadhaarVerificationServiceConfig -> req -> m resp) ->
  Id MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchatOperatingCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchatOperatingCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchatOperatingCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchatOperatingCityId (DMSC.AadhaarVerificationService merchantServiceUsageConfig.aadhaarVerificationService)
      >>= fromMaybeM (InternalError $ "No Aadhaar Verification service provider configured for the merchant, merchantId:" <> merchatOperatingCityId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.AadhaarVerificationServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
