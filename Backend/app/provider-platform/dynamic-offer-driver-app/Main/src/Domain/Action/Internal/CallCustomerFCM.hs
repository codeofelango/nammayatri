{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.CallCustomerFCM where

import Domain.Types.Ride
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Notifications

callCustomerFCM :: Id Ride -> Maybe Text -> Flow APISuccess
callCustomerFCM rideId apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchantId <- maybe ((runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)) <&> (.providerId)) return ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  person <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  pingCustomer person
  _merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") $ ride.merchantId
  pure Success
  where
    pingCustomer customer = do
      case customer.deviceToken of
        Just token -> notifyDriver customer.merchantOperatingCityId FCM.CALL_SERVICE_DOWN "Try Direct Calling Option" "Customer is Trying to Reach You" customer (Just token)
        Nothing -> log INFO $ "Active customers with no token" <> show customer.id
