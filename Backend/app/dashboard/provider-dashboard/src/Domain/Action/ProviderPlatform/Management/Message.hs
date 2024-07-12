module Domain.Action.ProviderPlatform.Management.Message
  ( postMessageUploadFile,
    postMessageAddLink,
    postMessageAdd,
    postMessageSend,
    getMessageList,
    getMessageInfo,
    getMessageDeliveryInfo,
    getMessageReceiverList,
  )
where

import qualified API.Types.ProviderPlatform.Management.Message
import AWS.S3 (FileType (..))
import qualified Dashboard.Common
import qualified Dashboard.Common.Message
import qualified Data.Text as T
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postMessageUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.UploadFileRequest -> Environment.Flow API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageUploadFile merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (req.fileType `elem` [Audio, Image]) $
    throwError $ InvalidRequest "Only support Audio/Image media type. For Video/MediaLinks use AddLink API."
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MessageAPI Dashboard.Common.Message.PostMessageUploadFileEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.messageDSL.postMessageUploadFile)) req)

postMessageAddLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> Environment.Flow API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageAddLink merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (((req.fileType == VideoLink || req.fileType == PortraitVideoLink) && checkIfYoutubeLink req.url) || req.fileType == ImageLink) $
    throwError $ InvalidRequest "Only support youtube video links and image links. For Audio use uploadFile API."
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MessageAPI Dashboard.Common.Message.PostMessageAddLinkEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.postMessageAddLink) req)
  where
    -- youtube link can be https://youtu.be/shorts/nWbI-DfwRpw or https://www.youtube.com/shorts/nWbI-DfwRpw
    checkIfYoutubeLink link = T.isPrefixOf "https://" link && T.isInfixOf "youtu" link

postMessageAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> Environment.Flow API.Types.ProviderPlatform.Management.Message.AddMessageResponse)
postMessageAdd merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (length req.mediaFiles <= 1) $
    throwError $ InvalidRequest "Only support one media file per message. More than one media support will be added soon."
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MessageAPI Dashboard.Common.Message.PostMessageAddEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.postMessageAdd) req)

postMessageSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.SendMessageRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postMessageSend merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.MessageAPI Dashboard.Common.Message.PostMessageSendEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.messageDSL.postMessageSend)) req)

getMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Message.MessageListResponse)
getMessageList merchantShortId opCity apiTokenInfo limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.getMessageList) limit offset

getMessageInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Dashboard.Common.Message) -> Environment.Flow API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)
getMessageInfo merchantShortId opCity apiTokenInfo messageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.getMessageInfo) messageId

getMessageDeliveryInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Dashboard.Common.Message) -> Environment.Flow API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse)
getMessageDeliveryInfo merchantShortId opCity apiTokenInfo messageId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.getMessageDeliveryInfo) messageId

getMessageReceiverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> (Kernel.Types.Id.Id Dashboard.Common.Message) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus) -> Environment.Flow API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse)
getMessageReceiverList merchantShortId opCity apiTokenInfo messageId limit number offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.messageDSL.getMessageReceiverList) messageId limit number offset status
