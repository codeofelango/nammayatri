{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CallStatus where

import Data.Aeson
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CallStatusE e = CallStatus
  { callAttempt :: Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus,
    callError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    callFromNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    callId :: Kernel.Prelude.Text,
    callService :: Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService,
    conversationDuration :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    dtmfNumberUsed :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recordingUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.External.Call.Interface.Types.CallStatus
  }
  deriving (Generic)

type CallStatus = CallStatusE 'AsEncrypted

type DecryptedCallStatus = CallStatusE 'AsUnencrypted

instance EncryptedItem CallStatus where
  type Unencrypted CallStatus = (DecryptedCallStatus, HashSalt)
  encryptItem (entity, salt) = do
    callFromNumber_ <- encryptItem $ (,salt) <$> callFromNumber entity
    pure
      CallStatus
        { callAttempt = callAttempt entity,
          callError = callError entity,
          callFromNumber = callFromNumber_,
          callId = callId entity,
          callService = callService entity,
          conversationDuration = conversationDuration entity,
          createdAt = createdAt entity,
          dtmfNumberUsed = dtmfNumberUsed entity,
          entityId = entityId entity,
          id = id entity,
          merchantId = merchantId entity,
          recordingUrl = recordingUrl entity,
          status = status entity
        }
  decryptItem entity = do
    callFromNumber_ <- fmap fst <$> decryptItem (callFromNumber entity)
    pure
      ( CallStatus
          { callAttempt = callAttempt entity,
            callError = callError entity,
            callFromNumber = callFromNumber_,
            callId = callId entity,
            callService = callService entity,
            conversationDuration = conversationDuration entity,
            createdAt = createdAt entity,
            dtmfNumberUsed = dtmfNumberUsed entity,
            entityId = entityId entity,
            id = id entity,
            merchantId = merchantId entity,
            recordingUrl = recordingUrl entity,
            status = status entity
          },
        ""
      )

instance EncryptedItem' CallStatus where
  type UnencryptedItem CallStatus = DecryptedCallStatus
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data CallAttemptStatus = Attempted | Resolved | Failed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CallAttemptStatus)
