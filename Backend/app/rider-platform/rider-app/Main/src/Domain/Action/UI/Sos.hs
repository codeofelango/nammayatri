{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Sos where

import API.Types.UI.Sos
import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import Data.Text as T
import qualified Domain.Action.UI.FollowRide as DFR
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Action.UI.Profile as DP
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Sos as DSos
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification as Notification
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPDEN
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Sos as QSos
import qualified Text.Read as Read
import Tools.Error
import Tools.Ticket as Ticket

data SOSVideoUploadReq = SOSVideoUploadReq
  { video :: FilePath,
    fileType :: S3.FileType,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SOSVideoUploadReq where
  fromMultipart form = do
    SOSVideoUploadReq
      <$> fmap fdPayload (lookupFile "video" form)
      <*> (lookupInput "fileType" form >>= (Read.readEither . T.unpack))
      <*> fmap fdFileCType (lookupFile "video" form)

instance ToMultipart Tmp SOSVideoUploadReq where
  toMultipart sosVideoUploadReq =
    MultipartData
      [Input "fileType" (show sosVideoUploadReq.fileType)]
      [FileData "video" (T.pack sosVideoUploadReq.video) "" (sosVideoUploadReq.video)]

newtype AddSosVideoRes = AddSosVideoRes
  { fileUrl :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getSosGetDetails :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DRide.Ride -> Flow SosDetailsRes
getSosGetDetails (mbPersonId, _) rideId_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbSosDetails <- CQSos.findByRideId rideId_
  case mbSosDetails of
    Nothing -> do
      mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId_
      case mockSos of
        Nothing -> return SosDetailsRes {sos = Nothing}
        Just mSos -> do
          now <- getCurrentTime
          return SosDetailsRes {sos = Just $ buildMockSos mSos now}
    Just sosDetails -> do
      unless (personId_ == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
      return SosDetailsRes {sos = Just sosDetails}
  where
    buildMockSos :: DSos.SosMockDrill -> UTCTime -> DSos.Sos
    buildMockSos mockSos now =
      DSos.Sos
        { flow = DSos.SafetyFlow,
          id = "mock-sos",
          personId = mockSos.personId,
          rideId = rideId_,
          status = mockSos.status,
          ticketId = Nothing,
          merchantId = Nothing,
          merchantOperatingCityId = Nothing,
          createdAt = now,
          updatedAt = now
        }

postSosCreate :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> SosReq -> Flow SosRes
postSosCreate (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  Redis.del $ CQSos.mockSosKey personId
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let trackLink = riderConfig.trackingShortUrlPattern <> ride.shortId.getShortId
  sosId <- createTicketForNewSos person ride riderConfig trackLink req
  message <-
    MessageBuilder.buildSOSAlertMessage person.merchantOperatingCityId $
      MessageBuilder.BuildSOSAlertMessageReq
        { userName = SLP.getName person,
          rideLink = trackLink
        }
  when (req.isRideEnded /= Just True) $ do
    emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
    when (shouldSendSms person) $ do
      void $ QPDEN.updateShareRideForAll personId True
      enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_TRIGGERED (Just message) True emergencyContacts.defaultEmergencyNumbers
  return $
    SosRes
      { sosId = sosId
      }
  where
    shouldSendSms person_ = person_.shareEmergencyContacts && req.flow /= DSos.Police
    notificationBody person_ = SLP.getName person_ <> " has initiated an SOS. Tap to follow and respond to the emergency situation"
    notificationTitle = "SOS Alert"

enableFollowRideInSos :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Flow ()
enableFollowRideInSos emergencyContacts = do
  mapM_
    ( \contact -> do
        case contact.contactPersonId of
          Nothing -> pure ()
          Just id -> do
            contactPersonEntity <- QP.findById id >>= fromMaybeM (PersonDoesNotExist id.getId)
            DFR.updateFollowDetails contactPersonEntity contact
    )
    emergencyContacts

createTicketForNewSos :: Person.Person -> DRide.Ride -> DRC.RiderConfig -> Text -> SosReq -> Flow (Id DSos.Sos)
createTicketForNewSos person ride riderConfig trackLink req = do
  sosRes <- CQSos.findByRideId ride.id
  case sosRes of
    Just sosDetails -> do
      void $ QSos.updateStatus DSos.Pending sosDetails.id
      void $ callUpdateTicket person sosDetails $ Just "SOS Re-Activated"
      CQSos.cacheSosIdByRideId ride.id $ sosDetails {DSos.status = DSos.Pending}
      return sosDetails.id
    Nothing -> do
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = buildRideInfo ride person phoneNumber
          kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
      ticketId <- do
        if riderConfig.enableSupportForSafety
          then do
            ticketResponse <- try @_ @SomeException (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink] rideInfo req.flow riderConfig.kaptureConfig.disposition kaptureQueue))
            case ticketResponse of
              Right ticketResponse' -> return (Just ticketResponse'.ticketId)
              Left _ -> return Nothing
          else return Nothing
      sosDetails <- buildSosDetails person req ticketId
      CQSos.cacheSosIdByRideId ride.id sosDetails
      void $ QSos.create sosDetails
      return sosDetails.id

postSosStatus :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> SosUpdateReq -> Flow APISuccess.APISuccess
postSosStatus (mbPersonId, _) sosId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  void $ QSos.updateStatus req.status sosId
  void $ callUpdateTicket person sosDetails req.comment
  pure APISuccess.Success

postSosMarkRideAsSafe :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Id DSos.Sos -> MarkAsSafeReq -> Flow APISuccess.APISuccess
postSosMarkRideAsSafe (mbPersonId, merchantId) sosId MarkAsSafeReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, merchantId)
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case isMock of
    Just True -> do
      mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
      case mockSos of
        Nothing -> pure ()
        Just _ -> do
          Redis.setExp (CQSos.mockSosKey personId) (DSos.SosMockDrill {personId, status = DSos.MockResolved}) 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers
      return APISuccess.Success
    _ -> do
      sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
      when (sosDetails.status == DSos.Resolved) $ throwError $ InvalidRequest "Sos already resolved."
      void $ callUpdateTicket person sosDetails $ Just "Mark Ride as Safe"
      void $ QSos.updateStatus DSos.Resolved sosId
      CQSos.cacheSosIdByRideId sosDetails.rideId $ sosDetails {DSos.status = DSos.Resolved}
      when (person.shareEmergencyContacts && isRideEnded /= Just True) $ do
        SPDEN.notifyEmergencyContacts person (notificationBody person) notificationTitle Notification.SOS_RESOLVED Nothing False emergencyContacts.defaultEmergencyNumbers
      pure APISuccess.Success
  where
    notificationBody person_ =
      SLP.getName person_
        <> if fromMaybe False isMock
          then " has marked ride as safe in test safety drill. This is a practice exercise, not a real emergency situation."
          else " has marked the ride as safe. Tap to view the ride details"
    notificationTitle = if fromMaybe False isMock then "Test Safety Drill Alert" else "Ride Safe"

postSosCreateMockSos :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> MockSosReq -> Flow APISuccess.APISuccess
postSosCreateMockSos (mbPersonId, _) MockSosReq {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  emergencyContacts <- DP.getDefaultEmergencyNumbers (personId, person.merchantId)
  case startDrill of
    Just True -> do
      SPDEN.notifyEmergencyContacts person (notificationBody person True) notificationTitle Notification.SOS_MOCK_DRILL_NOTIFY Nothing False emergencyContacts.defaultEmergencyNumbers
      when (fromMaybe False onRide) $ do
        void $ QPDEN.updateShareRideForAll personId True
        enableFollowRideInSos emergencyContacts.defaultEmergencyNumbers
    _ -> do
      when (not $ fromMaybe False person.hasCompletedMockSafetyDrill) $ QP.updateSafetyDrillStatus (Just True) personId
      when (fromMaybe False onRide) $ do
        let mockEntity = DSos.SosMockDrill {personId, status = DSos.MockPending}
        Redis.setExp (CQSos.mockSosKey personId) mockEntity 13400
      SPDEN.notifyEmergencyContacts person (notificationBody person False) notificationTitle Notification.SOS_MOCK_DRILL Nothing False emergencyContacts.defaultEmergencyNumbers
  pure APISuccess.Success
  where
    notificationBody person_ isStartDrill =
      SLP.getName person_
        <> if isStartDrill
          then " is going to start a test safety drill with you. Tap to follow the test ride. This is a practice exercise, and not a real ride."
          else " has initiated a test safety drill with you. This is a practice exercise, not a real emergency situation..."
    notificationTitle = "Test Safety Drill Alert"

addSosVideo :: Id DSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Flow AddSosVideoRes
addSosVideo sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  contentType <- validateContentType
  merchantConfig <- CQM.findById (person.merchantId) >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  fileSize <- L.runIO $ withFile video ReadMode hFileSize
  when (fileSize > fromIntegral riderConfig.videoFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile video
  filePath <- createFilePath "/sos-video/" ("sos-" <> getId sosId) Video contentType
  let fileUrl =
        merchantConfig.publicMediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos-video"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.putPublic (T.unpack filePath) mediaFile
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      ride <- QRide.findById sosDetails.rideId >>= fromMaybeM (RideDoesNotExist sosDetails.rideId.getId)
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = buildRideInfo ride person phoneNumber
          trackLink = riderConfig.trackingShortUrlPattern <> ride.shortId.getShortId
          kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
      when riderConfig.enableSupportForSafety $
        void $ try @_ @SomeException $ withShortRetry (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink, fileUrl] rideInfo DSos.SafetyFlow riderConfig.kaptureConfig.disposition kaptureQueue))
      createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType}
  where
    validateContentType = do
      case fileType of
        S3.Video | reqContentType == "video/mp4" -> pure "mp4"
        _ -> throwError $ FileFormatNotSupported reqContentType

createMediaEntry :: Common.AddLinkAsMedia -> Flow AddSosVideoRes
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  MFQuery.create fileEntity
  return $
    AddSosVideoRes
      { fileUrl = url
      }
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DMF.MediaFile
          { id,
            _type = S3.Video,
            url = fileUrl,
            createdAt = now
          }

buildRideInfo :: DRide.Ride -> Person.Person -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride person phoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      rideCity = show person.currentCity,
      customerName = Just $ SLP.getName person,
      customerPhoneNo = phoneNumber,
      driverName = Just ride.driverName,
      driverPhoneNo = Just ride.driverMobileNumber,
      vehicleNo = ride.vehicleNumber,
      vehicleCategory = Just $ show ride.vehicleVariant,
      vehicleServiceTier = show <$> ride.vehicleServiceTierType,
      status = show ride.status,
      rideCreatedAt = ride.createdAt,
      pickupLocation = castLocationAPIEntity ride.fromLocation,
      dropLocation = castLocationAPIEntity <$> ride.toLocation,
      fare = Nothing
    }
  where
    castLocationAPIEntity ent =
      Ticket.Location
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

callUpdateTicket :: Person.Person -> DSos.Sos -> Maybe Text -> Flow APISuccess.APISuccess
callUpdateTicket person sosDetails mbComment = do
  case sosDetails.ticketId of
    Just ticketId -> do
      fork "update ticket request" $
        void $ Ticket.updateTicket (person.merchantId) person.merchantOperatingCityId (Ticket.UpdateTicketReq (fromMaybe "" mbComment) ticketId Ticket.IN)
      pure APISuccess.Success
    Nothing -> pure APISuccess.Success

mkTicket :: Person.Person -> Maybe Text -> [Text] -> Ticket.RideInfo -> DSos.SosType -> Text -> Text -> Ticket.CreateTicketReq
mkTicket person phoneNumber mediaLinks info flow disposition queue = do
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      issueId = Nothing,
      issueDescription,
      mediaFiles = Just mediaLinks,
      name = Just $ SLP.getName person,
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Just info,
      disposition,
      queue,
      becknIssueId = Nothing
    }
  where
    issueDescription = case flow of
      DSos.Police -> "112 called"
      _ -> "SOS activated"

buildSosDetails :: (EncFlow m r) => Person.Person -> SosReq -> Maybe Text -> m DSos.Sos
buildSosDetails person req ticketId = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = person.id,
        status = DSos.Pending,
        flow = req.flow,
        rideId = req.rideId,
        ticketId = ticketId,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
