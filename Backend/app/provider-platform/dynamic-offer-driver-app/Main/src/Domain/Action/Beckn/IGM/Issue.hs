{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.IGM.Issue where

import API.UI.Issue
import Domain.Types.Booking
import Domain.Types.IGMConfig
import qualified Domain.Types.IGMIssue as DIGM
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified IGM.Enums as Spec
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as QIC
import qualified IssueManagement.Storage.Queries.Issue.IssueOption as QIO
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIR
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMOC
import Storage.Queries.Booking as QB
import qualified Storage.Queries.IGMConfig as QIGMConfig
import qualified Storage.Queries.IGMIssue as QIGM
import qualified Storage.Queries.Ride as QRide

data DIssue = DIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueSubCategory :: Maybe Text,
    issueTypeText :: Text,
    issueStatusText :: Text,
    bookingId :: Text,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    createdAt :: UTCTimeRFC3339,
    bapId :: Text
  }
  deriving (Show, Generic)

data ValidatedDIssue = ValidatedDIssue
  { issueId :: Text,
    issueCategory :: Text,
    issueSubCategory :: Maybe Text,
    issueType :: DIGM.IssueType,
    issueStatus :: DIGM.Status,
    booking :: Booking,
    issueRaisedBy :: Maybe Text,
    customerName :: Maybe Text,
    customerEmail :: Maybe Text,
    customerPhone :: Maybe Text,
    bapId :: Text,
    igmConfig :: IGMConfig,
    createdAt :: UTCTimeRFC3339,
    merchantOperatingCity :: MerchantOperatingCity,
    merchant :: Merchant,
    bppId :: Text
  }

data IssueRes = IssueRes
  { issueId :: Text,
    respondentAction :: Text,
    groName :: Text,
    groPhone :: Text,
    groEmail :: Text,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: UTCTimeRFC3339,
    merchant :: Merchant,
    merchantOperatingCity :: MerchantOperatingCity,
    issueStatus :: DIGM.Status,
    bapId :: Text,
    bppId :: Text
  }

validateRequest :: Id Merchant -> DIssue -> Flow ValidatedDIssue
validateRequest merchantId dIssue@DIssue {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  booking <- QB.findById (Id dIssue.bookingId) >>= fromMaybeM (BookingDoesNotExist dIssue.bookingId)
  merchantOperatingCity <- QMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  issueStatus <- mapStatusAndTypeToStatus issueStatusText issueTypeText
  issueType <- mapType issueTypeText
  igmConfig <- QIGMConfig.findByMerchantId merchantId >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchantId)
  let bppId = merchant.subscriberId.getShortId
  pure $ ValidatedDIssue {..}

handler :: ValidatedDIssue -> Flow IssueRes
handler ValidatedDIssue {..} = do
  now <- getCurrentTime
  case issueStatus of
    DIGM.OPEN -> openBecknIssue ValidatedDIssue {..}
    DIGM.ESCALATED -> escalateBecknIssue ValidatedDIssue {..} now
    DIGM.CLOSED -> closeBecknIssue ValidatedDIssue {..} now

openBecknIssue :: ValidatedDIssue -> Flow IssueRes
openBecknIssue dIssue@ValidatedDIssue {..} = do
  ride <- QRide.findOneByBookingId booking.id >>= fromMaybeM (RideDoesNotExist booking.id.getId)
  -- riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id") -- shrey00 : incorporate it back?
  let igmIssue =
        DIGM.IGMIssue
          { DIGM.createdAt = convertRFC3339ToUTC createdAt,
            DIGM.customerEmail = customerEmail,
            DIGM.customerName = customerName,
            DIGM.customerPhone = customerPhone,
            DIGM.id = Id issueId,
            DIGM.bookingId = booking.id,
            DIGM.issueRaisedByMerchant = bapId,
            DIGM.issueStatus = issueStatus,
            DIGM.issueType = issueType,
            DIGM.respondentAction = Nothing,
            DIGM.resolutionAction = Nothing,
            DIGM.updatedAt = convertRFC3339ToUTC createdAt,
            DIGM.merchantId = booking.providerId
          }
  category <- QIC.findByIGMIssueCategory issueCategory >>= fromMaybeM (InvalidRequest "Issue Category not found or unsupported")
  QIGM.create igmIssue
  mbOption <- QIO.findByIGMIssueSubCategory issueSubCategory
  let optionId = mbOption <&> (.id)
      description = maybe "No description provided" (.option) mbOption
  let issueReport = Common.IssueReportReq (Just $ cast ride.id) [] optionId category.id description Nothing (Just True)
  void $ Common.createIssueReport (cast ride.driverId, cast dIssue.merchant.id) Nothing issueReport driverIssueHandle Common.DRIVER (Just issueId)
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.PROCESSING,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = createdAt,
        updatedAt = createdAt,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

escalateBecknIssue :: ValidatedDIssue -> UTCTime -> Flow IssueRes
escalateBecknIssue ValidatedDIssue {..} now = do
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.ESCALATED,
            DIGM.updatedAt = now
          }
  QIGM.updateByPrimaryKey updatedIssue
  -- shrey00 : increase priority of capture issue : How?
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.PROCESSING,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = UTCTimeRFC3339 igmIssue.createdAt,
        updatedAt = UTCTimeRFC3339 now,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

closeBecknIssue :: ValidatedDIssue -> UTCTime -> Flow IssueRes
closeBecknIssue ValidatedDIssue {..} now = do
  igmIssue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  let updatedIssue =
        igmIssue
          { DIGM.issueStatus = DIGM.CLOSED,
            DIGM.updatedAt = now,
            DIGM.respondentAction = Just $ show Spec.RESOLVED
          }
  QIGM.updateByPrimaryKey updatedIssue
  issueReport <- QIR.findByBecknIssueId issueId >>= fromMaybeM (InvalidRequest "Issue Report not found")
  QIR.updateStatusAssignee issueReport.id (Just Common.CLOSED) issueReport.assignee
  void $ Common.updateTicketStatus issueReport TIT.CL (cast merchant.id) (cast merchantOperatingCity.id) driverIssueHandle "Closed by person"
  pure $
    IssueRes
      { issueId = issueId,
        respondentAction = show Spec.RESOLVED,
        groName = igmConfig.groName,
        groPhone = igmConfig.groPhone,
        groEmail = igmConfig.groEmail,
        createdAt = UTCTimeRFC3339 igmIssue.createdAt,
        updatedAt = UTCTimeRFC3339 now,
        merchantOperatingCity = merchantOperatingCity,
        ..
      }

mapType :: MonadFlow m => Text -> m DIGM.IssueType
mapType "ISSUE" = return DIGM.ISSUE
mapType "GRIEVANCE" = return DIGM.GRIEVANCE
mapType _ = throwError $ InvalidRequest "Invalid issue type"

mapStatusAndTypeToStatus :: MonadFlow m => Text -> Text -> m DIGM.Status
mapStatusAndTypeToStatus "OPEN" "ISSUE" = return DIGM.OPEN
mapStatusAndTypeToStatus "OPEN" "GRIEVANCE" = return DIGM.ESCALATED
mapStatusAndTypeToStatus "CLOSED" _ = return DIGM.CLOSED
mapStatusAndTypeToStatus _ _ = throwError $ InvalidRequest "Invalid issue status or type"

mapDomainStatusToSpecStatus :: DIGM.Status -> Maybe Text
mapDomainStatusToSpecStatus DIGM.CLOSED = Just $ show Spec.CLOSED
mapDomainStatusToSpecStatus _ = Just $ show Spec.OPEN
