{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.CancellationAndNoShowChargeConfigs where

import qualified Domain.Types.CancellationAndNoShowChargeConfigs
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CancellationAndNoShowChargeConfigs as Queries

findByMerchantOpCityIdAndServiceTierType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.ServiceTierType.ServiceTierType -> m (Kernel.Prelude.Maybe Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs))
findByMerchantOpCityIdAndServiceTierType merchantOperatingCityId vehicleServiceTierType = do
  Hedis.safeGet ("driverOfferCachedQueries:CancellationAndNoShowChargeConfigs:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleServiceTierType-" <> show vehicleServiceTierType)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:CancellationAndNoShowChargeConfigs:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":VehicleServiceTierType-" <> show vehicleServiceTierType) dataToBeCached expTime
                )
                /=<< Queries.findByMerchantOpCityIdAndServiceTierType merchantOperatingCityId vehicleServiceTierType
        )
