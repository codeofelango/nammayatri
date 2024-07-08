{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.CheckExotelDoFallback where

import qualified Data.HashMap.Strict as HMS
import qualified Domain.Types.Booking as DBooking
import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.Ride as DRide
import Kernel.External.Notification.FCM.Types (FCMNotificationType (..))
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified SharedLogic.CallBAP as CallBAP
import qualified Storage.Queries.CallStatus as QCallStatus
import TransactionLogs.Types

checkExotelDoFallback ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Job 'CheckExotelStatusDoFallback ->
  m ExecutionResult
checkExotelDoFallback Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let ride = jobData.ride
  let booking = jobData.booking
  callStatus <- QCallStatus.findOneByEntityId (Just $ getId ride.id) >>= fromMaybeM CallStatusDoesNotExist
  handleCallStatus callStatus ride booking CALL_SERVICE_DOWN

handleCallStatus ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  DCallStatus.CallStatus ->
  DRide.Ride ->
  DBooking.Booking ->
  FCMNotificationType ->
  m ExecutionResult
handleCallStatus callStatus ride booking fcmNotificationType = do
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    Just DCallStatus.Failed -> do
      CallBAP.sendCallServiceDownUpdateToBAP booking ride fcmNotificationType
      return Complete
    Just DCallStatus.Attempted -> do
      -- unregistered number case
      CallBAP.sendCallServiceDownUpdateToBAP booking ride fcmNotificationType
      return Complete
    Nothing -> throwError (CallStatusFieldNotPresent "callAttempt")
