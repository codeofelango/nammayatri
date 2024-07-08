{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationAndNoShowChargeConfigs where

import qualified Domain.Types.CancellationAndNoShowChargeConfigs
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationAndNoShowChargeConfigs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs] -> m ())
createMany = traverse_ create

findByMerchantOpCityIdAndServiceTierType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.ServiceTierType.ServiceTierType -> m (Maybe Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs))
findByMerchantOpCityIdAndServiceTierType merchantOperatingCityId vehicleServiceTierType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleServiceTierType $ Se.Eq vehicleServiceTierType
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs -> m (Maybe Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs -> m ())
updateByPrimaryKey (Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.freeCancellationTimeSeconds freeCancellationTimeSeconds,
      Se.Set Beam.maxCancellationCharge maxCancellationCharge,
      Se.Set Beam.maxWaitingTimeAtPickupSeconds maxWaitingTimeAtPickupSeconds,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minCancellationCharge minCancellationCharge,
      Se.Set Beam.perMetreCancellationCharge perMetreCancellationCharge,
      Se.Set Beam.perMinuteCancellationCharge perMinuteCancellationCharge,
      Se.Set Beam.percentageOfRideFareToBeCharged percentageOfRideFareToBeCharged,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CancellationAndNoShowChargeConfigs Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs where
  fromTType' (Beam.CancellationAndNoShowChargeConfigsT {..}) = do
    pure $
      Just
        Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs
          { currency = currency,
            freeCancellationTimeSeconds = freeCancellationTimeSeconds,
            id = Kernel.Types.Id.Id id,
            maxCancellationCharge = maxCancellationCharge,
            maxWaitingTimeAtPickupSeconds = maxWaitingTimeAtPickupSeconds,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minCancellationCharge = minCancellationCharge,
            perMetreCancellationCharge = perMetreCancellationCharge,
            perMinuteCancellationCharge = perMinuteCancellationCharge,
            percentageOfRideFareToBeCharged = percentageOfRideFareToBeCharged,
            vehicleServiceTierType = vehicleServiceTierType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CancellationAndNoShowChargeConfigs Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs where
  toTType' (Domain.Types.CancellationAndNoShowChargeConfigs.CancellationAndNoShowChargeConfigs {..}) = do
    Beam.CancellationAndNoShowChargeConfigsT
      { Beam.currency = currency,
        Beam.freeCancellationTimeSeconds = freeCancellationTimeSeconds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxCancellationCharge = maxCancellationCharge,
        Beam.maxWaitingTimeAtPickupSeconds = maxWaitingTimeAtPickupSeconds,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minCancellationCharge = minCancellationCharge,
        Beam.perMetreCancellationCharge = perMetreCancellationCharge,
        Beam.perMinuteCancellationCharge = perMinuteCancellationCharge,
        Beam.percentageOfRideFareToBeCharged = percentageOfRideFareToBeCharged,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
