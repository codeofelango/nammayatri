{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth where

import Data.Text as T
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common as CoreCommon
import qualified Kernel.Utils.Common as Utils
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Error

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (AdminTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyTokenAction ::
  forall m r.
  (HasEsqEnv m r, Redis.HedisFlow m r, HasField "authTokenCacheExpiry" r Seconds) =>
  VerificationAction VerifyToken m
verifyTokenAction = VerificationAction verifyPerson

-- | Verifies admin's token.
type AdminTokenAuth = HeaderAuth "token" AdminVerifyToken

data AdminVerifyToken

instance VerificationMethod AdminVerifyToken where
  type VerificationResult AdminVerifyToken = Person.Person
  verificationDescription =
    "Checks whether token is registered and belongs to a person with admin role."

verifyAdmin :: MonadFlow m => SP.Person -> m Person.Person
verifyAdmin user = do
  when (user.role /= SP.ADMIN) $
    throwError AccessDenied
  return user

verifyToken :: forall m r. (HasEsqEnv m r, MonadThrow m, Log m) => RegToken -> m SR.RegistrationToken
verifyToken regToken = do
  QR.findByToken regToken (Proxy @m)
    >>= Utils.fromMaybeM (InvalidToken regToken)
    >>= validateToken

validateAdmin :: forall m r. (HasEsqEnv m r, EncFlow m r) => RegToken -> m Person.Person
validateAdmin regToken = do
  SR.RegistrationToken {..} <- verifyToken regToken
  user <-
    QP.findById (Proxy @m) (Id entityId)
      >>= fromMaybeM (PersonNotFound entityId)
  verifyAdmin user

verifyPerson :: (HasEsqEnv m r, Redis.HedisFlow m r, HasField "authTokenCacheExpiry" r Seconds) => RegToken -> m (Id Person.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.get key
  case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let expiryTime = min sr.tokenExpiry authTokenCacheExpiry
      let personId = Id sr.entityId
      Redis.setExp key personId expiryTime
      return personId

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "providerPlatform:authTokenCacheKey:" <> regToken

validateAdminAction :: forall m r. (HasEsqEnv m r, EncFlow m r) => VerificationAction AdminVerifyToken m
validateAdminAction = VerificationAction validateAdmin

validateToken :: (HasEsqEnv m r, MonadThrow m, Log m) => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr

-- TODO Next logic is the same for rider-app, beckn-transport and driver-offer-bpp. Move it to Lib

type DashboardTokenAuth = HeaderAuth "token" DashboardVerifyToken

data DashboardVerifyToken = DashboardVerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardTokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

data Dashboard = Dashboard

instance VerificationMethod DashboardVerifyToken where
  type VerificationResult DashboardVerifyToken = Dashboard
  verificationDescription =
    "Checks whether dashboard token is registered."

verifyDashboardAction :: HasFlowEnv m r '["dashboardToken" ::: Text] => VerificationAction DashboardVerifyToken m
verifyDashboardAction = VerificationAction verifyDashboard

-- Do we need some expiry time for dashboard token?
verifyDashboard :: HasFlowEnv m r '["dashboardToken" ::: Text] => RegToken -> m Dashboard
verifyDashboard incomingToken = do
  dashboardToken <- asks (.dashboardToken)
  if incomingToken == dashboardToken
    then pure Dashboard
    else throwError (InvalidToken incomingToken)

clearDriverSession :: forall m r. (HasEsqEnv m r, Redis.HedisFlow m r) => Id Person.Person -> m ()
clearDriverSession personId = do
  regTokens <- QR.findAllByPersonId personId (Proxy @m)
  for_ regTokens $ \regToken -> do
    void $ Redis.del $ authTokenCacheKey regToken.token
