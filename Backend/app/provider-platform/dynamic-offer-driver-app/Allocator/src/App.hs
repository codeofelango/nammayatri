{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App where

import Environment (Flow, HandlerCfg, HandlerEnv, buildHandlerEnv)
import qualified EulerHS.Runtime as R
import Kernel.Exit
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Types.Flow (runFlowR)
import Kernel.Utils.App (getPodName, handleLeft)
import Kernel.Utils.Common
import Kernel.Utils.Dhall
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers)
import qualified Storage.CachedQueries.Merchant as Storage
import qualified Storage.Queries.AllocatorJob as QAllJ

allocatorHandle :: R.FlowRuntime -> HandlerEnv -> SchedulerHandle AllocatorJobType
allocatorHandle flowRt env =
  SchedulerHandle
    { getTasksById = QAllJ.getTasksById,
      getReadyTasks = QAllJ.getReadyTasks,
      markAsComplete = QAllJ.markAsComplete,
      markAsFailed = QAllJ.markAsFailed,
      updateErrorCountAndFail = QAllJ.updateErrorCountAndFail,
      reSchedule = QAllJ.reSchedule,
      updateFailureCount = QAllJ.updateFailureCount,
      reScheduleOnError = QAllJ.reScheduleOnError,
      jobHandlers =
        emptyJobHandlerList
          & putJobHandlerInList (liftIO . runFlowR flowRt env . sendSearchRequestToDrivers)
    }

runDriverOfferAllocator ::
  (HandlerCfg -> HandlerCfg) ->
  IO ()
runDriverOfferAllocator configModifier = do
  handlerCfg <- configModifier <$> readDhallConfigDefault "driver-offer-allocator"
  handlerEnv <- buildHandlerEnv handlerCfg
  hostname <- getPodName
  let loggerRt = L.getEulerLoggerRuntime hostname handlerCfg.appCfg.loggerConfig

  R.withFlowRuntime (Just loggerRt) \flowRt -> do
    flowRt' <- runFlowR flowRt handlerEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded handlerCfg.appCfg.migrationPath handlerCfg.appCfg.autoMigrate handlerCfg.appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "

        logInfo "Setting up for signature auth..."
        allProviders <-
          try (Storage.loadAllProviders (Proxy @Flow))
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allSubscriberIds = map ((.subscriberId.getShortId) &&& (.uniqueKeyId)) allProviders
        flowRt' <- modFlowRtWithAuthManagers flowRt handlerEnv allSubscriberIds

        logInfo ("Runtime created. Starting server at port " <> show (handlerCfg.schedulerConfig.port))
        pure flowRt'
    runSchedulerService handlerCfg.schedulerConfig $ allocatorHandle flowRt' handlerEnv
