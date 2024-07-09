{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Ride where

import qualified Data.Text
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Ride as Beam
import Storage.Queries.Transformers.Ride
import qualified Storage.Queries.Transformers.Ride

instance FromTType' Beam.Ride Domain.Types.Ride.Ride where
  fromTType' (Beam.RideT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    fromLocation' <- Storage.Queries.Transformers.Ride.getFromLocation id bookingId merchantId merchantOperatingCityId
    merchantOperatingCityId' <- Storage.Queries.Transformers.Ride.getMerchantOperatingCityId bookingId merchantId merchantOperatingCityId
    toLocation' <- Storage.Queries.Transformers.Ride.getToLocation id bookingId merchantId merchantOperatingCityId
    trackingUrl' <- Kernel.Prelude.parseBaseUrl trackingUrl
    tripCategory' <- Storage.Queries.Transformers.Ride.getTripCategory bookingId tripCategory
    pure $
      Just
        Domain.Types.Ride.Ride
          { backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            bookingId = Kernel.Types.Id.Id bookingId,
            cancellationFeeIfCancelled = cancellationFeeIfCancelled,
            chargeableDistance = chargeableDistance,
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientId = Kernel.Types.Id.Id <$> clientId,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceCalculationFailed = distanceCalculationFailed,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverArrivalTime = driverArrivalTime,
            driverDeviatedFromRoute = driverDeviatedFromRoute,
            driverDeviatedToTollRoute = driverDeviatedToTollRoute,
            driverGoHomeRequestId = Kernel.Types.Id.Id <$> driverGoHomeRequestId,
            driverId = Kernel.Types.Id.Id driverId,
            enableFrequentLocationUpdates = enableFrequentLocationUpdates,
            enableOtpLessRide = enableOtpLessRide,
            endOdometerReading = Storage.Queries.Transformers.Ride.mkOdometerReading endOdometerReadingFileId endOdometerReadingValue,
            endOtp = endOtp,
            estimatedTollCharges = estimatedTollCharges,
            estimatedTollNames = estimatedTollNames,
            fare = fmap (Kernel.Types.Common.mkAmountWithDefault fareAmount) fare,
            fareParametersId = Kernel.Types.Id.Id <$> fareParametersId,
            fromLocation = fromLocation',
            id = Kernel.Types.Id.Id id,
            isAdvanceBooking = Kernel.Prelude.fromMaybe False isAdvanceBooking,
            isAirConditioned = isAirConditioned,
            isFreeRide = isFreeRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            numberOfDeviation = numberOfDeviation,
            numberOfOsrmSnapToRoadCalls = numberOfOsrmSnapToRoadCalls,
            numberOfSelfTuned = numberOfSelfTuned,
            numberOfSnapToRoadCalls = numberOfSnapToRoadCalls,
            onlinePayment = Kernel.Prelude.fromMaybe False onlinePayment,
            otp = otp,
            pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
            previousRideTripEndPos = Storage.Queries.Transformers.Ride.mkLatLong previousRideTripEndLat previousRideTripEndLon,
            rideEndedBy = rideEndedBy,
            safetyAlertTriggered = safetyAlertTriggered,
            shortId = Kernel.Types.Id.ShortId shortId,
            startOdometerReading = Storage.Queries.Transformers.Ride.mkOdometerReading startOdometerReadingFileId startOdometerReadingValue,
            status = status,
            toLocation = toLocation',
            tollCharges = tollCharges,
            tollConfidence = tollConfidence,
            tollNames = tollNames,
            trackingUrl = trackingUrl',
            traveledDistance = traveledDistance,
            tripCategory = tripCategory',
            tripEndPos = Storage.Queries.Transformers.Ride.mkLatLong tripEndLat tripEndLon,
            tripEndTime = tripEndTime,
            tripStartPos = Storage.Queries.Transformers.Ride.mkLatLong tripStartLat tripStartLon,
            tripStartTime = tripStartTime,
            uiDistanceCalculationWithAccuracy = uiDistanceCalculationWithAccuracy,
            uiDistanceCalculationWithoutAccuracy = uiDistanceCalculationWithoutAccuracy,
            updatedAt = updatedAt,
            vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
            vehicleServiceTierName = vehicleServiceTierName,
            vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.Ride Domain.Types.Ride.Ride where
  toTType' (Domain.Types.Ride.Ride {..}) = do
    Beam.RideT
      { Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.cancellationFeeIfCancelled = cancellationFeeIfCancelled,
        Beam.chargeableDistance = chargeableDistance,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientId = Kernel.Types.Id.getId <$> clientId,
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceCalculationFailed = distanceCalculationFailed,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverArrivalTime = driverArrivalTime,
        Beam.driverDeviatedFromRoute = driverDeviatedFromRoute,
        Beam.driverDeviatedToTollRoute = driverDeviatedToTollRoute,
        Beam.driverGoHomeRequestId = Kernel.Types.Id.getId <$> driverGoHomeRequestId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enableFrequentLocationUpdates = enableFrequentLocationUpdates,
        Beam.enableOtpLessRide = enableOtpLessRide,
        Beam.endOdometerReadingFileId = getEndOdometerReadingFileId endOdometerReading,
        Beam.endOdometerReadingValue = Kernel.Prelude.fmap Domain.Types.Ride.value endOdometerReading,
        Beam.endOtp = endOtp,
        Beam.estimatedTollCharges = estimatedTollCharges,
        Beam.estimatedTollNames = estimatedTollNames,
        Beam.fare = Kernel.Prelude.fmap roundToIntegral fare,
        Beam.fareAmount = fare,
        Beam.fareParametersId = Kernel.Types.Id.getId <$> fareParametersId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAdvanceBooking = Kernel.Prelude.Just isAdvanceBooking,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isFreeRide = isFreeRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.numberOfDeviation = numberOfDeviation,
        Beam.numberOfOsrmSnapToRoadCalls = numberOfOsrmSnapToRoadCalls,
        Beam.numberOfSelfTuned = numberOfSelfTuned,
        Beam.numberOfSnapToRoadCalls = numberOfSnapToRoadCalls,
        Beam.onlinePayment = Kernel.Prelude.Just onlinePayment,
        Beam.otp = otp,
        Beam.pickupDropOutsideOfThreshold = pickupDropOutsideOfThreshold,
        Beam.previousRideTripEndLat = Kernel.Prelude.fmap (.lat) previousRideTripEndPos,
        Beam.previousRideTripEndLon = Kernel.Prelude.fmap (.lon) previousRideTripEndPos,
        Beam.rideEndedBy = rideEndedBy,
        Beam.safetyAlertTriggered = safetyAlertTriggered,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.startOdometerReadingFileId = getStartOdometerReadingFileId startOdometerReading,
        Beam.startOdometerReadingValue = Kernel.Prelude.fmap Domain.Types.Ride.value startOdometerReading,
        Beam.status = status,
        Beam.tollCharges = tollCharges,
        Beam.tollConfidence = tollConfidence,
        Beam.tollNames = tollNames,
        Beam.trackingUrl = Kernel.Prelude.showBaseUrl trackingUrl,
        Beam.traveledDistance = traveledDistance,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.tripEndLat = Kernel.Prelude.fmap (.lat) tripEndPos,
        Beam.tripEndLon = Kernel.Prelude.fmap (.lon) tripEndPos,
        Beam.tripEndTime = tripEndTime,
        Beam.tripStartLat = Kernel.Prelude.fmap (.lat) tripStartPos,
        Beam.tripStartLon = Kernel.Prelude.fmap (.lon) tripStartPos,
        Beam.tripStartTime = tripStartTime,
        Beam.uiDistanceCalculationWithAccuracy = uiDistanceCalculationWithAccuracy,
        Beam.uiDistanceCalculationWithoutAccuracy = uiDistanceCalculationWithoutAccuracy,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierAirConditioned = vehicleServiceTierAirConditioned,
        Beam.vehicleServiceTierName = vehicleServiceTierName,
        Beam.vehicleServiceTierSeatingCapacity = vehicleServiceTierSeatingCapacity,
        Beam.vehicleVariant = vehicleVariant
      }
