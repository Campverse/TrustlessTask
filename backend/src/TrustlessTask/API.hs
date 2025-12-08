{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TrustlessTask.API where

import Servant
import TrustlessTask.API.Types
import Data.Text (Text)

type TrustlessTaskAPI =
  "api" :> "v1" :>
    (    "projects" :> ReqBody '[JSON] CreateProjectRequest :> Post '[JSON] ProjectResponse
    :<|> "projects" :> Get '[JSON] [ProjectResponse]
    :<|> "projects" :> Capture "projectId" Text :> Get '[JSON] ProjectResponse
    :<|> "projects" :> Capture "projectId" Text :> "milestone" :> Capture "milestoneId" Integer :> "complete" :> Post '[JSON] TxResponse
    :<|> "projects" :> Capture "projectId" Text :> "milestone" :> Capture "milestoneId" Integer :> "approve" :> Post '[JSON] TxResponse
    :<|> "projects" :> Capture "projectId" Text :> "cancel" :> Post '[JSON] TxResponse
    :<|> "users" :> Capture "address" Text :> "profile" :> Get '[JSON] UserProfile
    :<|> "disputes" :> ReqBody '[JSON] DisputeRequest :> Post '[JSON] DisputeResponse
    :<|> "disputes" :> Capture "disputeId" Text :> Get '[JSON] DisputeResponse
    :<|> "disputes" :> Capture "disputeId" Text :> "resolve" :> ReqBody '[JSON] Bool :> Post '[JSON] TxResponse
    )

trustlessTaskAPI :: Proxy TrustlessTaskAPI
trustlessTaskAPI = Proxy
