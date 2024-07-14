{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckExotelDoFallback where

import qualified Data.Text as T
import Domain.Types.CallStatus as DCallStatus hiding (rideId)
import Domain.Types.Person
import Domain.Types.Ride
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Call.Types as CallTypes
import Kernel.External.Notification.Interface.Types
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler hiding (ScheduledRideNotificationsToRiderJobData (..))
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as FCM
import qualified Tools.Notifications as TN

checkExotelDoFallback ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Job 'CheckExotelStatusDoFallback ->
  m ExecutionResult
checkExotelDoFallback Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let personId = jobData.personId
  let merchantOpCityId = jobData.merchantOperatingCityId
  merchantPN <- CPN.findByMerchantOpCityIdAndMessageKey merchantOpCityId "CALL_SERVICE_DOWN" >>= fromMaybeM (MerchantPNNotFound merchantOpCityId.getId "CALL_SERVICE_DOWN")
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let entityData = TN.NotifReq {title = merchantPN.title, message = merchantPN.body}
  callStatus <- QCallStatus.findOneByRideId (Just rideId.getId) >>= fromMaybeM CallStatusDoesNotExist
  handleCallStatus callStatus person rideId entityData merchantPN.fcmNotificationType

handleCallStatus ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  DCallStatus.CallStatus ->
  Person ->
  Id Ride ->
  TN.NotifReq ->
  Category ->
  m ExecutionResult
handleCallStatus callStatus person rideId entityData fcmNotificationType = do
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    ---------------------------------------------------------------------------------------------------
    Just DCallStatus.Failed
      | callStatus.callService == Just CallTypes.Knowlarity -> do
        FCM.notifyPersonOnEvents person entityData fcmNotificationType
        return Complete
    ---------------------------------------------------------------------------------------------------
    Just DCallStatus.Attempted -> do
      FCM.notifyPersonOnEvents person entityData fcmNotificationType
      return Complete
    ---------------------------------------------------------------------------------------------------
    Nothing -> do
      FCM.notifyPersonOnEvents person entityData fcmNotificationType
      return Complete
