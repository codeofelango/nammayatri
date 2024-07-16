{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckExotelDoFallback where

import qualified Data.HashMap.Strict as HM
import Domain.Types.CallStatus as DCallStatus hiding (rideId)
import qualified Domain.Types.Ride as Ride
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.JobScheduler hiding (ScheduledRideNotificationsToRiderJobData (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import Tools.Metrics (CoreMetrics)

checkExotelDoFallback ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CoreMetrics m
  ) =>
  Job 'CheckExotelStatusDoFallback ->
  m ExecutionResult
checkExotelDoFallback Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let bppRideId = jobData.bppRideId
  let driverOfferApiKey = jobData.driverOfferApiKey
  let driverOfferBaseUrl = jobData.driverOfferBaseUrl
  callStatus <- QCallStatus.findOneByRideId (Just rideId.getId) >>= fromMaybeM CallStatusDoesNotExist
  handleCallStatus callStatus bppRideId driverOfferApiKey driverOfferBaseUrl

handleCallStatus ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], MonadFlow m, CoreMetrics m) =>
  DCallStatus.CallStatus ->
  Id Ride.BPPRide ->
  Text ->
  BaseUrl ->
  m ExecutionResult
handleCallStatus callStatus bppRideId driverOfferApiKey driverOfferBaseUrl = do
  case callStatus.callAttempt of
    Just DCallStatus.Resolved ->
      return Complete
    Just DCallStatus.Failed -> do
      void $ CallBPPInternal.callCustomerFCM driverOfferApiKey driverOfferBaseUrl (getId bppRideId)
      return Complete
    Just DCallStatus.Attempted -> do
      -- unregistered number call status
      void $ CallBPPInternal.callCustomerFCM driverOfferApiKey driverOfferBaseUrl (getId bppRideId)
      return Complete
    Nothing -> throwError (CallStatusFieldNotPresent "callAttempt")
