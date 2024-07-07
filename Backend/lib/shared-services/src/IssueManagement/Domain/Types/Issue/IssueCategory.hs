module IssueManagement.Domain.Types.Issue.IssueCategory where

import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import Kernel.Types.Id

data IssueCategory = IssueCategory
  { id :: Id IssueCategory,
    category :: Text,
    logoUrl :: Text,
    priority :: Int,
    merchantId :: Id Merchant,
    igmCategory :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
