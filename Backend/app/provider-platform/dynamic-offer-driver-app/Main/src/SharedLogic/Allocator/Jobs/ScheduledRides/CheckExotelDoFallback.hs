{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.ScheduledRides.CheckExotelDoFallback where

import Domain.Types.CallStatus as DCallStatus
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Kernel.External.Call.Types as CallTypes
import Kernel.External.Notification.FCM.Types (FCMNotificationType, FCMRecipientToken)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.CallStatus as QCallStatus
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
  let deviceToken = jobData.deviceToken
  let merchantOpCityId = jobData.merchantOperatingCityId
  merchantPN <- CPN.findByMerchantOpCityIdAndMessageKey merchantOpCityId "CALL_SERVICE_DOWN" >>= fromMaybeM (MerchantPNNotFound merchantOpCityId.getId "CALL_SERVICE_DOWN")
  let entityData = TN.NotifReq {entityId = personId.getId, title = merchantPN.title, message = merchantPN.body}
  callStatus <- QCallStatus.findOneByEntityId (Just rideId.getId) >>= fromMaybeM CallStatusDoesNotExist
  handleCallStatus callStatus personId deviceToken merchantOpCityId entityData merchantPN.fcmNotificationType

handleCallStatus ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  DCallStatus.CallStatus ->
  Id Person ->
  Maybe FCMRecipientToken ->
  Id DMOC.MerchantOperatingCity ->
  TN.NotifReq ->
  FCMNotificationType ->
  m ExecutionResult
handleCallStatus callStatus personId deviceToken merchantOperatingCityId entityData fcmNotificationType = do
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    ---------------------------------------------------------------------------------------------------
    Just DCallStatus.Failed
      | callStatus.callService == Just CallTypes.Knowlarity -> do
        FCM.notifyDriverOnEvents merchantOperatingCityId personId deviceToken entityData fcmNotificationType
        return Complete
    ---------------------------------------------------------------------------------------------------
    Just DCallStatus.Attempted -> do
      merchantPNUnRegisterdNum <- CPN.findByMerchantOpCityIdAndMessageKey merchantOperatingCityId "UNREGISTERED_NUMBER" >>= fromMaybeM (MerchantPNNotFound merchantOperatingCityId.getId "UNREGISTERED_NUMBER")
      let entityData' = TN.NotifReq {entityId = personId.getId, title = merchantPNUnRegisterdNum.title, message = merchantPNUnRegisterdNum.body}
      FCM.notifyDriverOnEvents merchantOperatingCityId personId deviceToken entityData' merchantPNUnRegisterdNum.fcmNotificationType
      return Complete
    ---------------------------------------------------------------------------------------------------
    Nothing -> do
      FCM.notifyDriverOnEvents merchantOperatingCityId personId deviceToken entityData fcmNotificationType
      return Complete
    _ -> return Complete
