{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.Core where

import Control.Lens.Operators ((?~))
import Data.List (lookup)
import qualified Data.Text.Encoding as T
import Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Queries.BecknRequest as QBR
import Kernel.Types.Id
import Kernel.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Network.Wai.Internal as Wai
import Servant
import SharedLogic.CallBAP (buildBppUrl)

withCallback ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "httpClientOptions" ::: HttpClientOptions],
    HasShortDurationRetryCfg r c
  ) =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' withShortRetry

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ bppSubscriberId
  withBecknCallbackMig doWithCallback (Just authKey) action api context' cbUrl f

logBecknRequest :: AppEnv -> Application -> Application
logBecknRequest appEnv f req@Wai.Request {..} respF = do
  req' <- case lookup "Authorization" requestHeaders of
    Nothing -> return req
    Just header -> do
      body <- requestBody
      bodyMvar <- newMVar body
      let logEnv = appEnv.loggerEnv
          esqDBEnv = appEnv.esqDBEnv
      _ <- Esq.runTransactionIO logEnv esqDBEnv $ do
        QBR.logBecknRequest @IO (T.decodeUtf8 body) (T.decodeUtf8 header)
      return req {Wai.requestBody = mkRequestBody bodyMvar}
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe mempty
