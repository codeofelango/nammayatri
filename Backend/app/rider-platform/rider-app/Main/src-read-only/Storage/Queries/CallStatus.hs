{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallStatus (module Storage.Queries.CallStatus, module ReExport) where

import qualified Domain.Types.CallStatus
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as Beam
import Storage.Queries.CallStatusExtra as ReExport

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CallStatus.CallStatus] -> m ())
createMany = traverse_ create

findByCallId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.CallStatus.CallStatus))
findByCallId callId = do findOneWithKV [Se.Is Beam.callId $ Se.Eq callId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m (Maybe Domain.Types.CallStatus.CallStatus))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride) -> m (Maybe Domain.Types.CallStatus.CallStatus))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId <$> rideId)]

updateCallAttempt ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride) -> m ())
updateCallAttempt callAttempt rideId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.callAttempt callAttempt, Se.Set Beam.updatedAt _now] [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId <$> rideId)]

updateCallError ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateCallError callError callService merchantId callId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.callError callError, Se.Set Beam.callService callService, Se.Set Beam.merchantId merchantId, Se.Set Beam.updatedAt _now] [Se.Is Beam.callId $ Se.Eq callId]

updateCallStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Call.Interface.Types.CallStatus -> Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallStatus conversationDuration recordingUrl status callAttempt id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.conversationDuration conversationDuration,
      Se.Set Beam.recordingUrl recordingUrl,
      Se.Set Beam.status status,
      Se.Set Beam.callAttempt callAttempt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCallStatusInformation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Call.Interface.Types.CallStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride) -> Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallStatusInformation status merchantId rideId callService dtmfNumberUsed id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.callService callService,
      Se.Set Beam.dtmfNumberUsed dtmfNumberUsed,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCallStatusSId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m ())
updateCallStatusSId callId id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.callId callId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCustomerIvrResponse :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.External.Call.Interface.Types.CallStatus -> m ())
updateCustomerIvrResponse callId customerIvrResponse status = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.callId callId, Se.Set Beam.customerIvrResponse customerIvrResponse, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.callId $ Se.Eq callId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus -> m (Maybe Domain.Types.CallStatus.CallStatus))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CallStatus.CallStatus -> m ())
updateByPrimaryKey (Domain.Types.CallStatus.CallStatus {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.callAttempt callAttempt,
      Se.Set Beam.callError callError,
      Se.Set Beam.callFromNumberEncrypted (callFromNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.callFromNumberHash (callFromNumber <&> (.hash)),
      Se.Set Beam.callId callId,
      Se.Set Beam.callService callService,
      Se.Set Beam.conversationDuration conversationDuration,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.customerIvrResponse customerIvrResponse,
      Se.Set Beam.dtmfNumberUsed dtmfNumberUsed,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.recordingUrl recordingUrl,
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
