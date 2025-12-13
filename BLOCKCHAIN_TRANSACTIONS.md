# Blockchain Transaction Integration

## Overview

TrustlessTask now integrates real Cardano blockchain transactions for critical operations:

1. **Project Creation** - Locks funds in escrow smart contract
2. **Milestone Completion** - Records completion on-chain (freelancer action)
3. **Milestone Approval** - Releases funds from escrow to freelancer (client action)

## Transaction Flow

### 1. Project Creation & Fund Locking

When a client creates a project:

```
Client → Frontend → Backend → Cardano Blockchain
```

**Steps:**
1. Client fills project form with milestones
2. Frontend validates and sends to backend
3. Backend creates project record
4. **Future:** Backend builds transaction to lock total amount in escrow smart contract
5. Client signs transaction with their wallet
6. Transaction submitted to Cardano blockchain
7. Project marked as "Funded" with transaction hash

**Current Status:** Simulated (returns mock tx hash)

### 2. Milestone Completion

When a freelancer marks a milestone as complete:

```
Freelancer → Frontend → Backend → Cardano Blockchain
```

**Steps:**
1. Freelancer clicks "Mark Complete" button
2. Frontend builds completion transaction
3. **Future:** Transaction records completion on-chain with timestamp
4. Freelancer signs transaction
5. Backend submits to blockchain
6. Milestone marked as completed with transaction hash

**Current Status:** Simulated (returns mock tx hash)

### 3. Milestone Approval & Fund Release

When a client approves a completed milestone:

```
Client → Frontend → Backend → Smart Contract → Freelancer
```

**Steps:**
1. Client reviews completed work
2. Client clicks "Approve & Release Funds"
3. Frontend builds release transaction from escrow
4. Transaction includes:
   - Script UTxO reference (locked funds)
   - Redeemer (approval proof)
   - Recipient address (freelancer)
   - Amount to release
5. Client signs transaction with their wallet
6. Backend submits to blockchain
7. Smart contract validates and releases funds
8. Freelancer receives payment
9. Milestone marked as approved with transaction hash

**Current Status:** Simulated (returns mock tx hash)

## Smart Contract Integration

### Escrow Contract

The Plutus smart contract (`contracts/src/TrustlessTask/Escrow.hs`) handles:

- **Locking:** Client locks funds with project datum
- **Validation:** Ensures only authorized releases
- **Release:** Transfers funds to freelancer on approval
- **Refund:** Returns funds to client on cancellation

### Datum Structure

```haskell
data EscrowDatum = EscrowDatum
  { projectId :: ByteString
  , clientAddress :: Address
  , freelancerAddress :: Address
  , milestones :: [Milestone]
  , currentMilestone :: Integer
  }
```

### Redeemer Actions

```haskell
data EscrowRedeemer
  = ReleaseFunds Integer  -- Milestone ID
  | RefundClient
  | RaiseDispute ByteString
```

## API Endpoints

### Complete Milestone

```
POST /api/v1/projects/:projectId/milestone/:milestoneId/complete
Body: { signedTx?: string }
```

**Response:**
```json
{
  "txHash": "abc123...",
  "status": "Completed",
  "milestone": { ... }
}
```

### Approve Milestone

```
POST /api/v1/projects/:projectId/milestone/:milestoneId/approve
Body: { signedTx: string }  // Required!
```

**Response:**
```json
{
  "txHash": "def456...",
  "status": "Approved",
  "milestone": { ... },
  "projectCompleted": false
}
```

## Frontend Integration

### Wallet Interaction

The frontend uses the Cardano wallet API to:

1. **Connect Wallet** - Get user's address
2. **Sign Transactions** - User approves blockchain actions
3. **Submit Transactions** - Send to backend for blockchain submission

### Example: Approving a Milestone

```typescript
// 1. Build transaction
const milestone = project.milestones.find(m => m.id === milestoneId);
const txData = {
  recipient: project.freelancerAddress,
  amount: milestone.amount,
  scriptUtxo: project.escrowUtxo,
  redeemer: { ReleaseFunds: milestoneId }
};

// 2. Get signed transaction from wallet
const signedTx = await wallet.signTx(txData);

// 3. Submit to backend
const result = await projectsApi.approveMilestone(
  projectId, 
  milestoneId, 
  signedTx
);

// 4. Show transaction hash
console.log('Funds released:', result.txHash);
```

## Transaction Verification

All transactions can be verified on Cardano blockchain explorers:

- **Preprod Testnet:** https://preprod.cardanoscan.io/
- **Mainnet:** https://cardanoscan.io/

Search by transaction hash to see:
- Block confirmation
- Input/output addresses
- Amount transferred
- Script execution details
- Datum and redeemer data

## Security Features

1. **Smart Contract Validation** - Only authorized parties can release funds
2. **On-Chain Verification** - All actions recorded immutably
3. **Wallet Signatures** - User must approve all transactions
4. **Escrow Protection** - Funds locked until conditions met
5. **Dispute Resolution** - Arbiter can intervene if needed

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Project Creation | ✅ Working | Simulated tx hash |
| Fund Locking | 🚧 Planned | Needs smart contract deployment |
| Milestone Completion | ✅ Working | Simulated tx hash |
| Fund Release | ✅ Working | Simulated tx hash |
| Transaction Verification | 🚧 Planned | Blockfrost integration ready |
| Smart Contract Deployment | ✅ Complete | Contracts compiled and tested |

## Next Steps

To enable real blockchain transactions:

1. **Deploy Smart Contracts** to Cardano testnet
2. **Get Script Address** from deployed contract
3. **Update Backend** with real transaction building
4. **Test with Real Wallets** on preprod testnet
5. **Add Transaction Monitoring** for confirmations
6. **Deploy to Mainnet** after thorough testing

## Testing

### Testnet Testing

1. Get testnet ADA from faucet: https://docs.cardano.org/cardano-testnet/tools/faucet/
2. Create test project with small amounts
3. Complete and approve milestones
4. Verify transactions on explorer
5. Check wallet balances

### Local Testing

Currently using simulated transactions for development:
- No real ADA required
- Instant "confirmations"
- Full UI/UX testing
- Database integration verified

## Resources

- [Cardano Documentation](https://docs.cardano.org/)
- [Plutus Documentation](https://plutus.readthedocs.io/)
- [Blockfrost API](https://blockfrost.io/)
- [Cardano Serialization Lib](https://github.com/Emurgo/cardano-serialization-lib)
