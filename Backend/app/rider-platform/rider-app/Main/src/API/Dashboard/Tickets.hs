{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Tickets where

import qualified API.Types.UI.TicketService as ATB
import Data.Time
import qualified Domain.Action.UI.TicketService as DTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TicketBooking as DTB
import qualified Domain.Types.TicketBookingService as DTB
import qualified Domain.Types.TicketPlace as DTB
import qualified Domain.Types.TicketService as DTB
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data TicketBookingEndpoint
  = VerifyBookingDetails
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "TicketBookingEndpoint"

type VerifyBookingDetailsAPI =
  Capture "personServiceId" (Id DTB.TicketService)
    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBookingService)
    :> "verify"
    :> Post '[JSON] ATB.TicketServiceVerificationResp

type GetServicesAPI =
  Capture "ticketPlaceId" (Id DTB.TicketPlace)
    :> QueryParam "date" Day
    :> "services"
    :> Post '[JSON] [ATB.TicketServiceResp]

type GetTicketPlacesAPI =
  "places"
    :> Get '[JSON] [DTB.TicketPlace]

type UpdateSeatManagementAPI =
  "update"
    :> ReqBody '[JSON] ATB.TicketBookingUpdateSeatsReq
    :> Post '[JSON] APISuccess

type CancelTicketBookingServiceAPI =
  "bookings" :> "cancel"
    :> ReqBody '[JSON] ATB.TicketBookingCancelReq
    :> Post '[JSON] APISuccess

type CancelTicketServiceAPI =
  "service" :> "cancel"
    :> ReqBody '[JSON] ATB.TicketServiceCancelReq
    :> Post '[JSON] APISuccess

type GetTicketBookingDetailsAPI =
  "booking"
    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBooking)
    :> "details"
    :> Get '[JSON] ATB.TicketBookingDetails

type GetTicketBookingListAPI =
  "bookingList"
    :> Capture "ticketPlaceId" (Id DTB.TicketPlace)
    :> "details"
    :> QueryParam "ticketBookingShortId" Text
    :> QueryParam "ticketBookingServiceId" (Id DTB.TicketBookingService)
    :> QueryParam "status" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> QueryParam
         "limit"
         Int
    :> QueryParam
         "offset"
         Int
    :> Get '[JSON] DTB.TicketBookingListRes

type API =
  "tickets"
    :> VerifyBookingDetailsAPI
    :<|> GetServicesAPI
    :<|> UpdateSeatManagementAPI
    :<|> GetTicketPlacesAPI
    :<|> CancelTicketBookingServiceAPI
    :<|> CancelTicketServiceAPI
    :<|> GetTicketBookingDetailsAPI
    :<|> GetTicketBookingListAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  verifyBookingDetails merchantId
    :<|> getServices merchantId
    :<|> updateSeatManagement merchantId
    :<|> getTicketPlaces merchantId
    :<|> cancelTicketBookingService merchantId
    :<|> cancelTicketService merchantId
    :<|> getTicketBookingDetails merchantId
    :<|> getTicketBookingList merchantId

verifyBookingDetails :: ShortId DM.Merchant -> Id DTB.TicketService -> ShortId DTB.TicketBookingService -> FlowHandler ATB.TicketServiceVerificationResp
verifyBookingDetails merchantShortId personServiceId ticketBookingServiceShortId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketBookingsVerify (Nothing, m.id) personServiceId ticketBookingServiceShortId

getServices :: ShortId DM.Merchant -> Id DTB.TicketPlace -> Maybe Day -> FlowHandler [ATB.TicketServiceResp]
getServices merchantShortId ticketPlaceId date = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.getTicketPlacesServices (Nothing, m.id) ticketPlaceId date

updateSeatManagement :: ShortId DM.Merchant -> ATB.TicketBookingUpdateSeatsReq -> FlowHandler APISuccess
updateSeatManagement merchantShortId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketBookingsUpdateSeats (Nothing, m.id) req

getTicketPlaces :: ShortId DM.Merchant -> FlowHandler [DTB.TicketPlace]
getTicketPlaces merchantShortId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.getTicketPlaces (Nothing, m.id)

cancelTicketBookingService :: ShortId DM.Merchant -> ATB.TicketBookingCancelReq -> FlowHandler APISuccess
cancelTicketBookingService merchantShortId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketBookingCancel (Nothing, m.id) req

cancelTicketService :: ShortId DM.Merchant -> ATB.TicketServiceCancelReq -> FlowHandler APISuccess
cancelTicketService merchantShortId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketServiceCancel (Nothing, m.id) req

getTicketBookingDetails :: ShortId DM.Merchant -> ShortId DTB.TicketBooking -> FlowHandler ATB.TicketBookingDetails
getTicketBookingDetails merchantShortId ticketBookingShortId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.getTicketBookingsDetails (Nothing, m.id) ticketBookingShortId

getTicketBookingList :: ShortId DM.Merchant -> Id DTB.TicketPlace -> Maybe Text -> Maybe (Id DTB.TicketBookingService) -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Int -> Maybe Int -> FlowHandler DTB.TicketBookingListRes
getTicketBookingList merchantShortId ticketPlaceId mbTicketBookinShordId mbTicketBookingServiceId mbTicketBookingStatus mbFrom mbTo mbLimit mbOffset = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.getTicketBookingList (Nothing, m.id) mbLimit mbOffset ticketPlaceId mbTicketBookinShordId mbTicketBookingServiceId mbTicketBookingStatus mbFrom mbTo
