{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallStatusExtra where

import qualified Database.Beam as B
import Domain.Types.CallStatus
import Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Call.Types (CallService)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.CallStatus

-- Extra code goes here --

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findByCallSid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCT.callId $ Se.Eq callSid]

countCallsByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Ride -> m Int
countCallsByEntityId entityID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\ride -> (B.group_ (BeamCT.entityId ride), B.as_ @Int B.countAll_)) $
            B.filter_' (\(BeamCT.CallStatusT {..}) -> B.fromMaybe_ (B.val_ "") entityId B.==?. B.val_ (getId entityID)) $
              B.all_ (BeamCommon.callStatus BeamCommon.atlasDB)
  pure $ either (const 0) (maybe 0 snd) resp

findOneByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe CallStatus)
findOneByEntityId rideId = findAllWithOptionsKV [Se.Is BeamCT.entityId $ Se.Eq rideId] (Se.Desc BeamCT.createdAt) (Just 1) Nothing <&> listToMaybe

findByCallFromHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe DbHash -> m (Maybe CallStatus)
findByCallFromHash callFromNumberHash = findAllWithOptionsKV [Se.Is BeamCT.callFromNumberHash $ Se.Eq callFromNumberHash] (Se.Desc BeamCT.createdAt) (Just 1) Nothing <&> listToMaybe
