{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Status where

import qualified Beckn.Types.Core.Taxi.Status as Status
import Control.Lens ((%~))
import qualified Data.Text as T
import Domain.Types.Booking.Type (Booking)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantConfigNew
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

data DStatusReq = DStatusReq
  { booking :: Booking,
    merchant :: Merchant,
    merchantConfig :: MerchantConfigNew
  }

buildStatusReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DStatusReq ->
  m (BecknReq Status.StatusMessage)
buildStatusReq DStatusReq {..} = do
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchant.id.getId)
  context <-
    buildTaxiContext
      Context.STATUS
      messageId
      (Just booking.transactionId)
      merchant.bapId
      bapUrl
      (Just merchant.id.getId)
      (Just booking.providerUrl)
      merchantConfig.city
      merchantConfig.country
      False
  pure $
    BecknReq context $
      Status.StatusMessage
        { order_id = bppBookingId.getId
        }
