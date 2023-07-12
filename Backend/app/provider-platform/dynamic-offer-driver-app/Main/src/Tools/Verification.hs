{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Verification
  ( module Reexport,
    verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
    -- getTask,
  )
where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Kernel.External.Verification as Reexport hiding
  ( extractDLImage,
    -- getTask,
    extractRCImage,
    validateImage,
    verifyDLAsync,
    verifyRCAsync,
  )
import qualified Kernel.External.Verification as Verification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error
import Tools.Metrics

verifyDLAsync ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync = runWithServiceConfig Verification.verifyDLAsync

verifyRCAsync ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  VerifyRCAsyncReq ->
  m VerifyRCAsyncResp
verifyRCAsync = runWithServiceConfig Verification.verifyRCAsync

validateImage ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage = runWithServiceConfig Verification.validateImage

extractRCImage ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage = runWithServiceConfig Verification.extractRCImage

extractDLImage ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage = runWithServiceConfig Verification.extractDLImage

-- getTask ::
--   ( EncFlow m r,
--     CacheFlow m r,
--     EsqDBFlow m r,
--     CoreMetrics m
--   ) =>
--   Id DM.Merchant ->
--   GetTaskReq ->
--   m GetTaskResp
-- getTask = runWithServiceConfig Verification.getTask

runWithServiceConfig ::
  (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) =>
  (VerificationServiceConfig -> req -> m resp) ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchantOperatingCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchantOperatingCityId
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCityId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantIdAndService merchantOperatingCityId (DMSC.VerificationService merchantServiceUsageConfig.verificationService)
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantId:" <> merchantOperatingCityId.getId)
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> func vsc req
    _ -> throwError $ InternalError "Unknown Service Config"
