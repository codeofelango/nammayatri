{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.DriverOnboarding where

import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Status as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Auth (TokenAuth)

type API =
  "driver" :> "register"
    :> ( "dl"
           :> TokenAuth
           :> ReqBody '[JSON] DriverOnboarding.DriverDLReq
           :> Post '[JSON] DriverOnboarding.DriverDLRes
           :<|> "rc"
             :> TokenAuth
             :> ReqBody '[JSON] DriverOnboarding.DriverRCReq
             :> Post '[JSON] DriverOnboarding.DriverRCRes
           :<|> "status"
             :> TokenAuth
             :> Get '[JSON] DriverOnboarding.StatusRes
           :<|> "validateImage"
             :> TokenAuth
             :> ReqBody '[JSON] Image.ImageValidateRequest
             :> Post '[JSON] Image.ImageValidateResponse
           :<|> "validateImageFile"
             :> TokenAuth
             :> MultipartForm Tmp Image.ImageValidateFileRequest
             :> Post '[JSON] Image.ImageValidateResponse
           :<|> "generateAadhaarOtp"
             :> TokenAuth
             :> ReqBody '[JSON] AadhaarVerification.AadhaarOtpReq
             :> Post '[JSON] AadhaarVerification.AadhaarVerificationResp
           :<|> "verifyAadhaarOtp"
             :> TokenAuth
             :> ReqBody '[JSON] AV.VerifyAadhaarOtpReq
             :> Post '[JSON] AadhaarVerification.AadhaarOtpVerifyRes
       )
    :<|> "driver" :> "referral"
      :> TokenAuth
      :> ReqBody '[JSON] DriverOnboarding.ReferralReq
      :> Post '[JSON] DriverOnboarding.ReferralRes

handler :: FlowServer API
handler =
  ( verifyDL
      :<|> verifyRC
      :<|> statusHandler
      :<|> validateImage
      :<|> validateImageFile
      :<|> generateAadhaarOtp
      :<|> verifyAadhaarOtp
  )
    :<|> addReferral

verifyDL :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL (personId, merchantId, merchantOperatingCityId) = withFlowHandlerAPI . DriverOnboarding.verifyDL False Nothing (personId, merchantId, merchantOperatingCityId)

verifyRC :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC (personId, merchantId, merchantOperatingCityId) = withFlowHandlerAPI . DriverOnboarding.verifyRC False Nothing (personId, merchantId, merchantOperatingCityId)

statusHandler :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DriverOnboarding.StatusRes
statusHandler = withFlowHandlerAPI . DriverOnboarding.statusHandler

validateImage :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Image.ImageValidateRequest -> FlowHandler Image.ImageValidateResponse
validateImage (personId, merchantId, merchantOperatingCityId) = withFlowHandlerAPI . Image.validateImage False (personId, merchantId, merchantOperatingCityId)

validateImageFile :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Image.ImageValidateFileRequest -> FlowHandler Image.ImageValidateResponse
validateImageFile (personId, merchantId, merchantOperatingCityId) = withFlowHandlerAPI . Image.validateImageFile False (personId, merchantId, merchantOperatingCityId)

generateAadhaarOtp :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> AadhaarVerification.AadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp (personId, _, merchantOperatingCityId) = withFlowHandlerAPI . AV.generateAadhaarOtp False Nothing personId merchantOperatingCityId

verifyAadhaarOtp :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> AV.VerifyAadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp (personId, _, merchantOperatingCityId) = withFlowHandlerAPI . AV.verifyAadhaarOtp Nothing personId merchantOperatingCityId

addReferral :: (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DriverOnboarding.ReferralReq -> FlowHandler DriverOnboarding.ReferralRes
addReferral (personId, merchantId, merchantOperatingCityId) = withFlowHandlerAPI . DriverOnboarding.addReferral (personId, merchantId, merchantOperatingCityId)
