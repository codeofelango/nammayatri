{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.SafetyIVR where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.RiderConfig
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import qualified Tools.Call as Call
import Tools.Error

sendSafetyIVR ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Job 'SafetyIVR ->
  m ExecutionResult
sendSafetyIVR Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      rideId = jobData.rideId
  ride <- B.runInReplica $ QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  if ride.safetyCheckStatus == Just True
    then do
      logDebug $ "User has marked the ride as safe." <> show rideId <> "skipping IVR calling as safety check status is : " <> show ride.safetyCheckStatus
      pure ()
    else do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      logDebug $ "Triggering IVR for ride : " <> show rideId
      triggerIVR person ride
  return Complete
  where
    triggerIVR ::
      ( EncFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBFlow m r,
        SchedulerFlow r
      ) =>
      DP.Person ->
      DRide.Ride ->
      m ()
    triggerIVR person ride = do
      merchantOperatingCityId <- maybe (QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId) >>= pure . (.merchantOperatingCityId)) pure ride.merchantOperatingCityId
      riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
      let maybeAppId = (HM.lookup SosAppletID . exotelMap) =<< riderConfig.exotelAppIdMapping
      logDebug $ "Applet ID for SOS call : " <> show maybeAppId
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
      let phoneNumber = countryCode <> mobileNumber
      callStatusId <- generateGUID
      let callReq =
            Call.InitiateCallReq
              { fromPhoneNum = phoneNumber,
                toPhoneNum = Nothing,
                attachments = Call.Attachments $ DCall.CallAttachments {callStatusId = callStatusId, rideId = ride.id},
                appletId = maybeAppId
              }
      exotelResponse <- Call.initiateCall person.merchantId person.merchantOperatingCityId callReq
      callStatus <- buildCallStatus callStatusId exotelResponse ride
      QCallStatus.create callStatus
    buildCallStatus callStatusId exotelResponse ride = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = Just ride.id,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = getId <$> ride.merchantId,
            callService = Just Call.Exotel,
            callAttempt = Nothing,
            callFromNumber = Nothing,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }
