module IssueManagement.API.UI.Issue where

import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import Kernel.Types.Id
import Servant

type IssueAPI =
  Common.IssueCreateAPI
    :<|> "list"
      :> Common.IssueListAPI
    :<|> "upload"
      :> Common.IssueUploadAPI
    :<|> "media"
      :> Common.IssueFetchMediaAPI
    :<|> "category"
      :> Common.IssueCategoryAPI
    :<|> "option"
      :> Common.IssueOptionAPI
    :<|> Capture "issueId" (Id Domain.IssueReport)
      :> "info"
      :> Common.IssueInfoAPI
    :<|> Capture "issueId" (Id Domain.IssueReport)
      :> "option"
      :> Common.IssueUpdateAPI
    :<|> Capture "issueId" (Id Domain.IssueReport)
      :> "delete"
      :> Common.IssueDeleteAPI
    :<|> Capture "issueId" (Id Domain.IssueReport)
      :> "updateStatus"
      :> Common.IssueStatusUpdateAPI
    :<|> "igmStatus"
      :> Common.IgmStatusAPI
    :<|> Capture "issueId" (Id Domain.IssueReport)
      :> "resolveIgmIssue"
      :> Common.ResolveIgmIssueAPI
