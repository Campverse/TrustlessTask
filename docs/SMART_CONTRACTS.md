# Smart Contract Documentation

## Escrow Validator

### Purpose
Manages trustless escrow for freelance projects with milestone-based fund releases.

### Datum Structure
```haskell
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
```

### Redeemer Actions

#### CreateProject
- **Signer**: Client
- **Validation**:
  - Client signature present
  - Status is Created
  - Total milestone amounts equal total amount
- **Effect**: Locks funds in escrow

#### CompleteMilestone
- **Signer**: Freelancer
- **Parameters**: Milestone ID
- **Validation**:
  - Freelancer signature present
  - Project status is InProgress
  - Milestone not already completed
  - Before milestone deadline
- **Effect**: Marks milestone as completed

#### ApproveMilestone
- **Signer**: Client
- **Parameters**: Milestone ID
- **Validation**:
  - Client signature present
  - Milestone is completed
  - Milestone not already approved
- **Effect**: Marks milestone as approved

#### ReleaseFunds
- **Parameters**: Milestone ID
- **Validation**:
  - Milestone is completed and approved
  - Correct amount paid to freelancer
- **Effect**: Releases milestone funds

#### CancelProject
- **Signers**: Client (if no milestones completed) OR Client + Freelancer
- **Validation**:
  - Appropriate signatures present
  - Valid cancellation conditions
- **Effect**: Returns funds to client

#### RaiseDispute
- **Signer**: Client OR Freelancer
- **Validation**:
  - Either party signature present
  - Project not completed or cancelled
- **Effect**: Changes status to Disputed

#### ResolveDispute
- **Signer**: Arbiter
- **Parameters**: Outcome (Boolean)
- **Validation**:
  - Arbiter signature present
  - Project status is Disputed
- **Effect**: Distributes funds per outcome

## Dispute Validator

### Purpose
Manages dispute creation and resolution with arbiter involvement.

### Datum Structure
```haskell
data DisputeDatum = DisputeDatum
  { disputeProjectId :: BuiltinByteString
  , disputeRaiser :: PubKeyHash
  , disputeReason :: BuiltinByteString
  , disputeArbiter :: PubKeyHash
  , disputeResolved :: Bool
  , disputeOutcome :: Maybe Bool
  }
```

### Redeemer Actions

#### CreateDispute
- **Signer**: Dispute raiser
- **Validation**:
  - Raiser signature present
  - Dispute not already resolved
- **Effect**: Creates dispute record

#### ResolveDisputeAction
- **Signer**: Arbiter
- **Parameters**: Outcome
- **Validation**:
  - Arbiter signature present
  - Dispute not already resolved
- **Effect**: Records resolution outcome

## Reputation Validator

### Purpose
Tracks on-chain reputation metrics for users.

### Datum Structure
```haskell
data ReputationDatum = ReputationDatum
  { userPkh :: PubKeyHash
  , completedProjects :: Integer
  , totalEarned :: Integer
  , averageRating :: Integer
  , disputes :: Integer
  }
```

### Redeemer Actions

#### UpdateReputation
- **Signer**: User
- **Parameters**: Projects count, Rating
- **Validation**:
  - User signature present
  - Projects count not decreasing
  - Rating within valid range (0-500)
- **Effect**: Updates reputation metrics

#### RecordDispute
- **Signer**: User
- **Validation**:
  - User signature present
- **Effect**: Increments dispute count

## Testing Strategy

### Unit Tests
- Individual validator logic
- Edge cases for each redeemer
- Invalid input handling
- Signature verification

### Integration Tests
- Full transaction flows
- Multi-step processes
- State transitions
- Error scenarios

### Property Tests
- Invariant checking
- Randomized inputs
- State machine testing
- Concurrency scenarios

## Deployment

### Testnet Deployment
```bash
cd contracts
cabal build
cabal run trustless-task-contracts -- write-scripts --out-dir ./plutus-scripts
./scripts/deploy.sh
```

### Mainnet Considerations
- Audit by certified auditors
- Formal verification where possible
- Gradual rollout with limits
- Bug bounty program
- Emergency pause mechanism
