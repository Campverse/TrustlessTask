{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TrustlessTask.API.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

-- | Project creation request
data CreateProjectRequest = CreateProjectRequest
  { cprTitle :: Text
  , cprDescription :: Text
  , cprClientAddress :: Text
  , cprFreelancerAddress :: Text
  , cprTotalAmount :: Integer
  , cprMilestones :: [MilestoneRequest]
  , cprArbiterAddress :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Milestone request
data MilestoneRequest = MilestoneRequest
  { mrDescription :: Text
  , mrAmount :: Integer
  , mrDeadline :: UTCTime
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Project response
data ProjectResponse = ProjectResponse
  { prId :: Text
  , prTitle :: Text
  , prDescription :: Text
  , prClientAddress :: Text
  , prFreelancerAddress :: Text
  , prTotalAmount :: Integer
  , prStatus :: Text
  , prMilestones :: [MilestoneResponse]
  , prCreatedAt :: UTCTime
  , prTxHash :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Milestone response
data MilestoneResponse = MilestoneResponse
  { msId :: Integer
  , msDescription :: Text
  , msAmount :: Integer
  , msDeadline :: UTCTime
  , msCompleted :: Bool
  , msApproved :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Transaction submission response
data TxResponse = TxResponse
  { txHash :: Text
  , txStatus :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | User profile
data UserProfile = UserProfile
  { upAddress :: Text
  , upCompletedProjects :: Integer
  , upTotalEarned :: Integer
  , upAverageRating :: Double
  , upDisputes :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Dispute request
data DisputeRequest = DisputeRequest
  { drProjectId :: Text
  , drReason :: Text
  , drRaiserAddress :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Dispute response
data DisputeResponse = DisputeResponse
  { dsId :: Text
  , dsProjectId :: Text
  , dsReason :: Text
  , dsRaiserAddress :: Text
  , dsArbiterAddress :: Text
  , dsResolved :: Bool
  , dsOutcome :: Maybe Bool
  } deriving (Show, Generic, ToJSON, FromJSON)
