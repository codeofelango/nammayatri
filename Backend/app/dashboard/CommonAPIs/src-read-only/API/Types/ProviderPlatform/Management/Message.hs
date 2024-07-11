{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Message where

import qualified AWS.S3
import qualified Dashboard.Common
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AddLinkAsMedia = AddLinkAsMedia {fileType :: AWS.S3.FileType, url :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data AddMessageRequest = AddMessageRequest
  { _type :: API.Types.ProviderPlatform.Management.Message.MessageType,
    alwaysTriggerOnOnboarding :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    description :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mediaFiles :: [Kernel.Types.Id.Id Dashboard.Common.File],
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    translations :: [API.Types.ProviderPlatform.Management.Message.MessageTranslation]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AddMessageResponse = AddMessageResponse {messageId :: Kernel.Types.Id.Id Dashboard.Common.Message} deriving (Generic, ToJSON, FromJSON, ToSchema)

data InputType = Include | Exclude | AllEnabled deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.Read)

data MediaFile = MediaFile {_type :: AWS.S3.FileType, link :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageDeliveryInfoResponse = MessageDeliveryInfoResponse
  { failed :: Kernel.Prelude.Int,
    liked :: Kernel.Prelude.Int,
    messageId :: Kernel.Types.Id.Id Dashboard.Common.Message,
    queued :: Kernel.Prelude.Int,
    seen :: Kernel.Prelude.Int,
    sending :: Kernel.Prelude.Int,
    success :: Kernel.Prelude.Int,
    viewed :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageDeliveryStatus = Failed | Success | Queued | Sending deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data MessageInfoResponse = MessageInfoResponse
  { _type :: API.Types.ProviderPlatform.Management.Message.MessageType,
    description :: Kernel.Prelude.Text,
    mediaFiles :: [API.Types.ProviderPlatform.Management.Message.MediaFile],
    messageId :: Kernel.Types.Id.Id Dashboard.Common.Message,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageListItem = MessageListItem {_type :: API.Types.ProviderPlatform.Management.Message.MessageType, messageId :: Kernel.Types.Id.Id Dashboard.Common.Message, title :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageListResponse = MessageListResponse {messages :: [API.Types.ProviderPlatform.Management.Message.MessageListItem], summary :: Dashboard.Common.Summary}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageReceiverListItem = MessageReceiverListItem
  { liked :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    receiverId :: Kernel.Types.Id.Id Dashboard.Common.Receiver,
    receiverName :: Kernel.Prelude.Text,
    receiverNumber :: Kernel.Prelude.Text,
    reply :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    seen :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    status :: API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageReceiverListResponse = MessageReceiverListResponse {receivers :: [API.Types.ProviderPlatform.Management.Message.MessageReceiverListItem], summary :: Dashboard.Common.Summary}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageTranslation = MessageTranslation
  { description :: Kernel.Prelude.Text,
    label :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language,
    shortDescription :: Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MessageType = Action Kernel.Prelude.Text | Read deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data SendMessageRequest = SendMessageRequest {_type :: API.Types.ProviderPlatform.Management.Message.InputType, csvFile :: Kernel.Prelude.Maybe Kernel.Prelude.FilePath, messageId :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UploadFileRequest = UploadFileRequest {file :: Kernel.Prelude.FilePath, fileType :: AWS.S3.FileType, reqContentType :: Kernel.Prelude.Text} deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype UploadFileResponse = UploadFileResponse {fileId :: Kernel.Types.Id.Id Dashboard.Common.File} deriving (Generic, ToJSON, FromJSON, ToSchema)

type API = ("message" :> (PostMessageUploadFile :<|> PostMessageAddLink :<|> PostMessageAdd :<|> PostMessageSend :<|> GetMessageList :<|> GetMessageInfo :<|> GetMessageDeliveryInfo :<|> GetMessageReceiverList))

type PostMessageUploadFile =
  ( "uploadFile" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp API.Types.ProviderPlatform.Management.Message.UploadFileRequest
      :> Post
           ('[JSON])
           API.Types.ProviderPlatform.Management.Message.UploadFileResponse
  )

type PostMessageAddLink =
  ( "addLink" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia
      :> Post
           ('[JSON])
           API.Types.ProviderPlatform.Management.Message.UploadFileResponse
  )

type PostMessageAdd = ("add" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Message.AddMessageRequest :> Post ('[JSON]) API.Types.ProviderPlatform.Management.Message.AddMessageResponse)

type PostMessageSend =
  ( "send" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp API.Types.ProviderPlatform.Management.Message.SendMessageRequest
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetMessageList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) API.Types.ProviderPlatform.Management.Message.MessageListResponse)

type GetMessageInfo = (Capture "messageId" ((Kernel.Types.Id.Id Dashboard.Common.Message)) :> "info" :> Get ('[JSON]) API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)

type GetMessageDeliveryInfo =
  ( Capture "messageId" ((Kernel.Types.Id.Id Dashboard.Common.Message)) :> "deliveryInfo"
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse
  )

type GetMessageReceiverList =
  ( Capture "messageId" ((Kernel.Types.Id.Id Dashboard.Common.Message)) :> "receiverList" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "number"
           Kernel.Prelude.Text
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse
  )

data MessageAPIs = MessageAPIs
  { postMessageUploadFile ::
      ( ( Data.ByteString.Lazy.ByteString,
          API.Types.ProviderPlatform.Management.Message.UploadFileRequest
        ) ->
        EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.UploadFileResponse
      ),
    postMessageAddLink :: (API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.UploadFileResponse),
    postMessageAdd :: (API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.AddMessageResponse),
    postMessageSend :: ((Data.ByteString.Lazy.ByteString, API.Types.ProviderPlatform.Management.Message.SendMessageRequest) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getMessageList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageListResponse),
    getMessageInfo :: ((Kernel.Types.Id.Id Dashboard.Common.Message) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageInfoResponse),
    getMessageDeliveryInfo :: ((Kernel.Types.Id.Id Dashboard.Common.Message) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse),
    getMessageReceiverList :: ((Kernel.Types.Id.Id Dashboard.Common.Message) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse)
  }

mkMessageAPIs :: (Client EulerHS.Types.EulerClient API -> MessageAPIs)
mkMessageAPIs messageClient = (MessageAPIs {..})
  where
    postMessageUploadFile :<|> postMessageAddLink :<|> postMessageAdd :<|> postMessageSend :<|> getMessageList :<|> getMessageInfo :<|> getMessageDeliveryInfo :<|> getMessageReceiverList = messageClient
