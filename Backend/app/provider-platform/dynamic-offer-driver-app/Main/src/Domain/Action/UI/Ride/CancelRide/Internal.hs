{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified SharedLogic.CallBAP as BP
import SharedLogic.Cancel
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Ride (updateOnRideStatusWithAdvancedRideCheck)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.CancellationAndNoShowChargeConfigs as CQCancellationAndNoShowConfigs
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

cancelRideImpl :: Id DRide.Ride -> DRide.RideEndedBy -> SBCR.BookingCancellationReason -> Flow ()
cancelRideImpl rideId rideEndedBy bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  let merchantId = booking.providerId
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- CTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  noShowCharges <- if transporterConfig.canAddCancellationFee then calculateNoShowCharges booking ride else return Nothing
  cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy noShowCharges
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  fork "cancelRide - Notify driver" $ do
    triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {merchantId = merchantId, driverId = driver.id, rideFare = Just booking.estimatedFare, currency = booking.currency, distanceUnit = booking.distanceUnit}
    Notify.notifyOnCancel ride.merchantOperatingCityId booking driver bookingCReason.source
  fork "cancelRide/ReAllocate - Notify BAP" $ do
    isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason
    unless isReallocated $ BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source noShowCharges

calculateNoShowCharges :: SRB.Booking -> DRide.Ride -> Flow (Maybe PriceAPIEntity)
calculateNoShowCharges booking ride = do
  mbCancellationAndNoShowConfigs <- CQCancellationAndNoShowConfigs.findByMerchantOpCityIdAndServiceTierType booking.merchantOperatingCityId booking.vehicleServiceTier
  case (ride.driverArrivalTime, mbCancellationAndNoShowConfigs) of
    (Just arrivalTime, Just cancellationAndNoShowConfigs) -> do
      now <- getCurrentTime
      let timeDiff = roundToIntegral $ diffUTCTime now arrivalTime
      if timeDiff > cancellationAndNoShowConfigs.maxWaitingTimeAtPickupSeconds
        then do
          let maxWaitingTimeAtPickupMinutes = toHighPrecMoney (cancellationAndNoShowConfigs.maxWaitingTimeAtPickupSeconds.getSeconds `div` 60)
          let cancellationFee = cancellationAndNoShowConfigs.maxCancellationCharge + (maxWaitingTimeAtPickupMinutes * cancellationAndNoShowConfigs.perMinuteCancellationCharge)
          return $ Just PriceAPIEntity {amount = cancellationFee, currency = cancellationAndNoShowConfigs.currency}
        else return Nothing
    _ -> do
      logError "calculateNoShowCharges: driverArrivalTime or cancellationAndNoShowConfigs is not present"
      return Nothing

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    LT.HasLocationService m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  Id DMerc.Merchant ->
  DRide.RideEndedBy ->
  Maybe PriceAPIEntity ->
  m ()
cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy cancellationFee = do
  let driverId = cast ride.driverId
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  void $ LF.rideDetails ride.id DRide.CANCELLED merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon
  void $ QRide.updateStatus ride.id DRide.CANCELLED
  void $ QRide.updateRideEndedBy ride.id rideEndedBy
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
  whenJust cancellationFee $ \fee -> do
    riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
    riderDetails <- QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
    QRiderDetails.updateCancellationDues (fee.amount + riderDetails.cancellationDues) riderId
