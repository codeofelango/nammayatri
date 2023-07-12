{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantIdAndService,
    findOne,
    clearCache,
    cacheMerchantServiceConfig,
    upsertMerchantServiceConfig,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Merchant.MerchantServiceConfig
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService id serviceName =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeMerchantCityIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantIdAndService id serviceName

-- FIXME this is temprorary solution for backward compatibility
findOne :: (CacheFlow m r, EsqDBFlow m r) => ServiceName -> m (Maybe MerchantServiceConfig)
findOne serviceName = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeServiceNameKey serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheOneServiceConfig /=<< Queries.findOne serviceName

cacheOneServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheOneServiceConfig orgServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let serviceNameKey = makeServiceNameKey (getServiceName orgServiceConfig)
  Hedis.withCrossAppRedis $ Hedis.setExp serviceNameKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) orgServiceConfig) expTime

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig orgServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantCityIdAndServiceKey orgServiceConfig.merchantOperatingCityId (getServiceName orgServiceConfig)
  Hedis.withCrossAppRedis $ Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) orgServiceConfig) expTime

makeMerchantCityIdAndServiceKey :: Id MerchantOperatingCity -> ServiceName -> Text
makeMerchantCityIdAndServiceKey id serviceName = "driver-offer:CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName

makeServiceNameKey :: ServiceName -> Text
makeServiceNameKey serviceName = "driver-offer:CachedQueries:MerchantServiceConfig:ServiceName-" <> show serviceName

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> ServiceName -> m ()
clearCache merchantOperatingCityId serviceName = do
  Hedis.withCrossAppRedis $ Hedis.del (makeMerchantCityIdAndServiceKey merchantOperatingCityId serviceName)
  Hedis.withCrossAppRedis $ Hedis.del (makeServiceNameKey serviceName)

upsertMerchantServiceConfig :: MerchantServiceConfig -> Esq.SqlDB ()
upsertMerchantServiceConfig = Queries.upsertMerchantServiceConfig
