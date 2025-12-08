{-# LANGUAGE OverloadedStrings #-}

module TrustlessTask.API.Handlers where

import Servant
import TrustlessTask.API.Types
import TrustlessTask.Cardano.Client
import TrustlessTask.Database
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

-- | Create a new project
createProjectHandler :: CreateProjectRequest -> Handler ProjectResponse
createProjectHandler req = liftIO $ do
  -- Store project in database
  projectId <- storeProject req
  
  -- Build and submit transaction to Cardano
  txHash <- submitCreateProjectTx req
  
  -- Return project response
  return $ ProjectResponse
    { prId = projectId
    , prTitle = cprTitle req
    , prDescription = cprDescription req
    , prClientAddress = cprClientAddress req
    , prFreelancerAddress = cprFreelancerAddress req
    , prTotalAmount = cprTotalAmount req
    , prStatus = "Created"
    , prMilestones = map toMilestoneResponse (cprMilestones req)
    , prCreatedAt = undefined -- Set from DB
    , prTxHash = Just txHash
    }

-- | List all projects
listProjectsHandler :: Handler [ProjectResponse]
listProjectsHandler = liftIO fetchAllProjects

-- | Get project by ID
getProjectHandler :: Text -> Handler ProjectResponse
getProjectHandler projectId = liftIO $ fetchProjectById projectId

-- | Complete a milestone
completeMilestoneHandler :: Text -> Integer -> Handler TxResponse
completeMilestoneHandler projectId milestoneId = liftIO $ do
  txHash <- submitCompleteMilestoneTx projectId milestoneId
  return $ TxResponse txHash "Submitted"

-- | Approve a milestone
approveMilestoneHandler :: Text -> Integer -> Handler TxResponse
approveMilestoneHandler projectId milestoneId = liftIO $ do
  txHash <- submitApproveMilestoneTx projectId milestoneId
  return $ TxResponse txHash "Submitted"

-- | Cancel a project
cancelProjectHandler :: Text -> Handler TxResponse
cancelProjectHandler projectId = liftIO $ do
  txHash <- submitCancelProjectTx projectId
  return $ TxResponse txHash "Submitted"

-- | Get user profile
getUserProfileHandler :: Text -> Handler UserProfile
getUserProfileHandler address = liftIO $ fetchUserProfile address

-- | Create a dispute
createDisputeHandler :: DisputeRequest -> Handler DisputeResponse
createDisputeHandler req = liftIO $ do
  disputeId <- storeDispute req
  txHash <- submitRaiseDisputeTx req
  return $ DisputeResponse
    { dsId = disputeId
    , dsProjectId = drProjectId req
    , dsReason = drReason req
    , dsRaiserAddress = drRaiserAddress req
    , dsArbiterAddress = "" -- Set from project
    , dsResolved = False
    , dsOutcome = Nothing
    }

-- | Get dispute by ID
getDisputeHandler :: Text -> Handler DisputeResponse
getDisputeHandler disputeId = liftIO $ fetchDisputeById disputeId

-- | Resolve a dispute
resolveDisputeHandler :: Text -> Bool -> Handler TxResponse
resolveDisputeHandler disputeId outcome = liftIO $ do
  txHash <- submitResolveDisputeTx disputeId outcome
  return $ TxResponse txHash "Submitted"

-- Helper function
toMilestoneResponse :: MilestoneRequest -> MilestoneResponse
toMilestoneResponse mr = MilestoneResponse
  { msId = 0
  , msDescription = mrDescription mr
  , msAmount = mrAmount mr
  , msDeadline = mrDeadline mr
  , msCompleted = False
  , msApproved = False
  }
