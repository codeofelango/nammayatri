{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.FarePolicy.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.FarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.FarePolicy

findAllByMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Proxy ma ->
  m [FarePolicy]
findAllByMerchantId merchantId _ = do
  Esq.findAll @m @ma $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
    return farePolicy

findByMerchantIdAndVariant ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Variant ->
  Proxy ma ->
  m (Maybe FarePolicy)
findByMerchantIdAndVariant merchantId variant _ = do
  Esq.findOne @m @ma $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyMerchantId ==. val (toKey merchantId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: forall m ma. Transactionable ma m => Proxy ma -> Id FarePolicy -> m (Maybe FarePolicy)
findById _ = Esq.findById @m @ma

update :: FarePolicy -> SqlDB m ()
update farePolicy = do
  now <- getCurrentTime
  void $
    Esq.update $ \tbl -> do
      set
        tbl
        [ FarePolicyBaseDistanceFare =. val farePolicy.baseDistanceFare,
          FarePolicyBaseDistanceMeters =. val farePolicy.baseDistanceMeters,
          FarePolicyPerExtraKmFare =. val farePolicy.perExtraKmFare,
          FarePolicyDeadKmFare =. val farePolicy.deadKmFare,
          FarePolicyDriverMinExtraFee =. val farePolicy.driverExtraFee.minFee,
          FarePolicyDriverMaxExtraFee =. val farePolicy.driverExtraFee.maxFee,
          FarePolicyNightShiftStart =. val farePolicy.nightShiftStart,
          FarePolicyNightShiftEnd =. val farePolicy.nightShiftEnd,
          FarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)
