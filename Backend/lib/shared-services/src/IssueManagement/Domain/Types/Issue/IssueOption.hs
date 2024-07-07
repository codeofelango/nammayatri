module IssueManagement.Domain.Types.Issue.IssueOption where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory (IssueCategory)
import Kernel.Types.Id

data IssueOption = IssueOption
  { id :: Id IssueOption,
    issueCategoryId :: Maybe (Id IssueCategory),
    option :: Text,
    priority :: Int,
    issueMessageId :: Maybe Text,
    label :: Maybe Text,
    merchantId :: Id Merchant,
    igmSubCategory :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
