{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TrustlessTask.Types where

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON

-- | Project status enumeration
data ProjectStatus
  = Created
  | InProgress
  | UnderReview
  | Disputed
  | Completed
  | Cancelled
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''ProjectStatus
PlutusTx.makeLift ''ProjectStatus

-- | Milestone definition
data Milestone = Milestone
  { milestoneId :: Integer
  , milestoneDescription :: BuiltinByteString
  , milestoneAmount :: Integer
  , milestoneDeadline :: POSIXTime
  , milestoneCompleted :: Bool
  , milestoneApproved :: Bool
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''Milestone
PlutusTx.makeLift ''Milestone

-- | Project datum stored in escrow UTXO
data ProjectDatum = ProjectDatum
  { projectId :: BuiltinByteString
  , client :: PubKeyHash
  , freelancer :: PubKeyHash
  , totalAmount :: Integer
  , milestones :: [Milestone]
  , projectStatus :: ProjectStatus
  , createdAt :: POSIXTime
  , arbiter :: Maybe PubKeyHash
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''ProjectDatum
PlutusTx.makeLift ''ProjectDatum

-- | Redeemer actions for escrow contract
data EscrowRedeemer
  = CreateProject
  | CompleteMilestone Integer
  | ApproveMilestone Integer
  | ReleaseFunds Integer
  | CancelProject
  | RaiseDispute
  | ResolveDispute Bool
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''EscrowRedeemer
PlutusTx.makeLift ''EscrowRedeemer

-- | Reputation datum
data ReputationDatum = ReputationDatum
  { userPkh :: PubKeyHash
  , completedProjects :: Integer
  , totalEarned :: Integer
  , averageRating :: Integer -- Scaled by 100 (e.g., 450 = 4.50)
  , disputes :: Integer
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''ReputationDatum
PlutusTx.makeLift ''ReputationDatum

-- | Dispute datum
data DisputeDatum = DisputeDatum
  { disputeProjectId :: BuiltinByteString
  , disputeRaiser :: PubKeyHash
  , disputeReason :: BuiltinByteString
  , disputeArbiter :: PubKeyHash
  , disputeResolved :: Bool
  , disputeOutcome :: Maybe Bool -- True = freelancer wins, False = client wins
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

PlutusTx.unstableMakeIsData ''DisputeDatum
PlutusTx.makeLift ''DisputeDatum
