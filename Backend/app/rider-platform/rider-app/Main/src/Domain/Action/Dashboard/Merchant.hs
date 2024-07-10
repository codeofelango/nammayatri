{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( postMerchantServiceConfigMapsUpdate,
    -- mapsServiceUsageConfigUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantUpdate,
    -- serviceUsageConfig,
    getMerchantServiceUsageConfig,
    -- smsServiceConfigUpdate,
    postMerchantServiceConfigSmsUpdate,
    -- smsServiceUsageConfigUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    -- createMerchantOperatingCity,
    postMerchantConfigOperatingCityCreate,
    -- upsertSpecialLocation,
    -- deleteSpecialLocation,
    -- upsertSpecialLocationGate,
    -- deleteSpecialLocationGate,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    buildMerchantServiceConfig,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified Data.Text as T
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.Geometry as QGEO
import Tools.Error

---------------------------------------------------------------------
postMerchantUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantUpdateReq -> Flow APISuccess
postMerchantUpdate merchantShortId city req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantOperatingCityId /= merchantOperatingCity.id) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  void $ CQM.update updMerchant
  whenJust req.exoPhones \exophones -> do
    CQExophone.deleteByMerchantOperatingCityId merchantOperatingCity.id
    forM_ exophones $ \exophoneReq -> do
      exophone <- buildExophone merchant.id merchantOperatingCity.id now exophoneReq
      CQExophone.create exophone

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantOperatingCityId == merchantOperatingCity.id) allExophones
    CQExophone.clearCache merchantOperatingCity.id oldExophones
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  pure Success
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId merchantOperatingCityId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
        merchantOperatingCityId,
        primaryPhone = req.primaryPhone,
        backupPhone = req.backupPhone,
        isPrimaryDown = False,
        callService = req.callService,
        updatedAt = now,
        createdAt = now
      }

---------------------------------------------------------------------
getMerchantServiceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.ServiceUsageConfigRes
getMerchantServiceUsageConfig merchantShortId city = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  config <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  pure $ mkServiceUsageConfigRes config

mkServiceUsageConfigRes :: DMSUC.MerchantServiceUsageConfig -> Common.ServiceUsageConfigRes
mkServiceUsageConfigRes DMSUC.MerchantServiceUsageConfig {..} =
  Common.ServiceUsageConfigRes
    { getEstimatedPickupDistances = Nothing,
      getPickupRoutes = Just getPickupRoutes,
      getTripRoutes = Just getTripRoutes,
      snapToRoadProvidersList = [],
      ..
    }

---------------------------------------------------------------------
buildMerchantServiceConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceConfig ->
  m DMSC.MerchantServiceConfig
buildMerchantServiceConfig merchantId merchantOperatingCityId serviceConfig = do
  now <- getCurrentTime
  pure
    DMSC.MerchantServiceConfig
      { merchantId,
        serviceConfig,
        merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }

postMerchantServiceConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigMapsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigMapsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigSmsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigSmsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigMapsUpdate merchantShortId city req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> postMerchantServiceUsageConfigMapsUpdate : " (show merchantOperatingCity.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigSmsUpdate merchantShortId city req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> postMerchantServiceUsageConfigSmsUpdate : " (show merchantOperatingCity.id)
  pure Success

postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId _city mbSpecialLocationId request = do
  existingSLWithGeom <- maybe (return Nothing) QSL.findByIdWithGeom mbSpecialLocationId
  let mbExistingSL = fst <$> existingSLWithGeom
      mbGeom = snd =<< existingSLWithGeom
  updatedSL <- mkSpecialLocation mbExistingSL mbGeom
  void $
    runTransaction $
      if isJust mbExistingSL then QSLG.updateSpecialLocation updatedSL else QSLG.create updatedSL
  return Success
  where
    mkSpecialLocation :: Maybe SL.SpecialLocation -> Maybe Text -> Flow SL.SpecialLocation
    mkSpecialLocation mbExistingSpLoc mbGeometry = do
      let geom = request.geom <|> mbGeometry
      id <- maybe generateGUID (return . (.id)) mbExistingSpLoc
      now <- getCurrentTime
      merchantOperatingCity <- maybe (return Nothing) (CQMOC.findByMerchantShortIdAndCity merchantShortId) request.city
      locationName <-
        fromMaybeM (InvalidRequest "Location Name cannot be empty for a new special location") $
          request.locationName <|> (mbExistingSpLoc <&> (.locationName))
      category <- fromMaybeM (InvalidRequest "Category is a required field for a new special location") $ request.category <|> (mbExistingSpLoc <&> (.category))
      return $
        SL.SpecialLocation
          { gates = [],
            createdAt = maybe now (.createdAt) mbExistingSpLoc,
            updatedAt = now,
            merchantOperatingCityId = (.id.getId) <$> merchantOperatingCity,
            ..
          }

deleteMerchantSpecialLocationDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Flow APISuccess
deleteMerchantSpecialLocationDelete _merchantShortid _city specialLocationId = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Special Location with given id not found")
  void $ runTransaction $ QSL.deleteById specialLocationId
  void $ runTransaction $ QGI.deleteAll specialLocationId
  pure Success

postMerchantSpecialLocationGatesUpsert :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Flow APISuccess
postMerchantSpecialLocationGatesUpsert _merchantShortId _city specialLocationId request = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  createOrUpdateGate existingGates request
  return Success
  where
    createOrUpdateGate :: [(D.GateInfo, Maybe Text)] -> Common.UpsertSpecialLocationGateReqT -> Flow ()
    createOrUpdateGate existingGates req = do
      let existingGatewithGeom = find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName req.name) existingGates
          existingGate = fst <$> existingGatewithGeom
          mbGeom = snd =<< existingGatewithGeom
      updatedGate <- mkGate req existingGate mbGeom
      void $
        runTransaction $
          if isNothing existingGate then QGIG.create updatedGate else QGIG.updateGate updatedGate

    mkGate :: Common.UpsertSpecialLocationGateReqT -> Maybe D.GateInfo -> Maybe Text -> Flow D.GateInfo
    mkGate reqT mbGate mbGeom = do
      id <- cast <$> maybe generateGUID (return . (.id)) mbGate
      now <- getCurrentTime
      latitude <- fromMaybeM (InvalidRequest "Latitude field cannot be empty for a new gate") $ reqT.latitude <|> (mbGate <&> (.point.lat))
      longitude <- fromMaybeM (InvalidRequest "Longitude field cannot be empty for a new gate") $ reqT.longitude <|> (mbGate <&> (.point.lon))
      address <- fromMaybeM (InvalidRequest "Address cannot be empty for a new gate") $ reqT.address <|> (mbGate >>= (.address))
      let canQueueUpOnGate = fromMaybe False $ reqT.canQueueUpOnGate <|> (mbGate <&> (.canQueueUpOnGate))
          defaultDriverExtra = reqT.defaultDriverExtra <|> (mbGate >>= (.defaultDriverExtra))
          geom = reqT.geom <|> mbGeom
      return $
        D.GateInfo
          { name = reqT.name,
            address = Just address,
            geom,
            createdAt = maybe now (.createdAt) mbGate,
            updatedAt = now,
            point = LatLong {lat = latitude, lon = longitude},
            ..
          }

deleteMerchantSpecialLocationGatesDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> Flow APISuccess
deleteMerchantSpecialLocationGatesDelete _merchantShortId _city specialLocationId gateName = do
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  let existingGate = fst <$> find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName gateName) existingGates
  case existingGate of
    Nothing -> throwError $ InvalidRequest "Could not find any gates with the specified name for the given specialLocationId"
    Just gate -> runTransaction $ QGI.deleteById gate.id
  return Success

normalizeName :: Text -> Text
normalizeName = T.strip . T.toLower

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  baseOperatingCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  let baseOperatingCityId = baseOperatingCity.id

  -- city
  newOperatingCity <- buildMerchantOperatingCity merchant.id

  -- merchant message
  merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId
  newMerchantMessages <- mapM (buildMerchantMessage newOperatingCity.id) merchantMessages

  -- merchant payment method
  merchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId baseOperatingCityId
  newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newOperatingCity.id) merchantPaymentMethods

  -- merchant service usage config
  merchantServiceUsageConfig <- CQMSUC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
  newMerchantServiceUsageConfig <- buildMerchantServiceUsageConfig newOperatingCity.id merchantServiceUsageConfig

  -- merchant service config
  -- merchantServiceConfigs <- CQMSC.findAllMerchantOpCityId baseOperatingCityId
  -- newMerchantServiceConfigs <- mapM (buildMerchantServiceConfig newOperatingCity.id) merchantServiceConfigs

  -- rider_config
  riderConfig <- CQRC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Transporter Config not found")
  newRiderConfig <- buildRiderConfig newOperatingCity.id riderConfig

  -- geometry
  geometry <- buildGeometry

  -- exophone
  exophones <- CQExophone.findAllByMerchantOperatingCityId baseOperatingCityId

  QGEO.create geometry
  CQMOC.create newOperatingCity
  mapM_ CQMM.create newMerchantMessages
  mapM_ CQMPM.create newMerchantPaymentMethods
  CQMSUC.create newMerchantServiceUsageConfig
  -- mapM_ CQMSC.create newMerchantServiceConfigs
  CQRC.create newRiderConfig
  whenJust (listToMaybe exophones) $ \exophone -> do
    exophone' <- buildNewExophone newOperatingCity.id exophone
    CQExophone.create exophone'

  when req.enableForMerchant $ do
    let newOrigin = updateGeoRestriction merchant.geofencingConfig.origin
        newDestination = updateGeoRestriction merchant.geofencingConfig.destination
    CQM.updateGeofencingConfig merchant.id newOrigin newDestination
    CQM.clearCache merchant

  pure $ Common.CreateMerchantOperatingCityRes newOperatingCity.id.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [(show req.city)]

    buildGeometry = do
      id <- generateGUID
      pure
        DGEO.Geometry
          { id,
            region = show req.city,
            state = req.state,
            city = req.city,
            geom = Just req.geom
          }

    buildMerchantOperatingCity merchantId = do
      id <- generateGUID
      now <- getCurrentTime
      pure
        DMOC.MerchantOperatingCity
          { id,
            merchantId,
            merchantShortId,
            lat = req.lat,
            long = req.long,
            city = req.city,
            state = req.state,
            country = req.country,
            distanceUnit = fromMaybe Meter req.distanceUnit,
            createdAt = now,
            updatedAt = now
          }

    buildNewExophone newCityId DExophone.Exophone {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DExophone.Exophone
          { id = newId,
            merchantOperatingCityId = newCityId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantMessage newCityId DMM.MerchantMessage {..} = do
      now <- getCurrentTime
      return $
        DMM.MerchantMessage
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantPaymentMethod newCityId DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantServiceUsageConfig newCityId DMSUC.MerchantServiceUsageConfig {..} = do
      now <- getCurrentTime
      return $
        DMSUC.MerchantServiceUsageConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    {- Do it once service config on basis on city id is implemented -}
    -- buildMerchantServiceConfig newCityId DMSC.MerchantServiceConfig {..} = do
    --   now <- getCurrentTime
    --   return $
    --     DMSC.MerchantServiceConfig
    --       { merchantOperatingCityId = Just newCityId,
    --         createdAt = now,
    --         updatedAt = now,
    --         ..
    --       }

    buildRiderConfig newCityId DRC.RiderConfig {..} = do
      now <- getCurrentTime
      return $
        DRC.RiderConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }
