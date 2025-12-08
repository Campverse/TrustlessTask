# Cardano Blockchain Integration Guide

## 🎯 Overview

This guide explains how to enable live Cardano blockchain transactions in TrustlessTask.

## 📋 Current Status

### ✅ What's Implemented
- Cardano wallet connection interface
- Blockfrost API integration
- Transaction building framework
- Script interaction methods
- Transaction status tracking

### ⏳ What's Needed for Live Transactions
1. Blockfrost API key (free tier available)
2. Cardano wallet browser extension
3. Test ADA from faucet
4. Deployed Plutus scripts

## 🚀 Quick Start: Enable Blockchain

### Step 1: Get Blockfrost API Key

1. Go to https://blockfrost.io/
2. Sign up for free account
3. Create new project (select Preprod testnet)
4. Copy your project ID

### Step 2: Configure Environment

**Frontend** (`frontend/.env`):
```env
VITE_BLOCKFROST_PROJECT_ID=your_project_id_here
VITE_BLOCKFROST_URL=https://cardano-preprod.blockfrost.io/api/v0
VITE_NETWORK=Preprod
```

**Backend** (`backend-api/.env`):
```env
BLOCKFROST_PROJECT_ID=your_project_id_here
CARDANO_NETWORK=preprod
```

### Step 3: Install Cardano Wallet

Choose one:
- **Nami**: https://namiwallet.io/
- **Eternl**: https://eternl.io/
- **Flint**: https://flint-wallet.com/

Install browser extension and create wallet.

### Step 4: Get Test ADA

1. Go to https://docs.cardano.org/cardano-testnet/tools/faucet/
2. Enter your testnet address
3. Receive 10,000 test ADA

### Step 5: Enable Blockchain Mode

**In `frontend/src/hooks/useWallet.ts`:**
```typescript
const USE_REAL_BLOCKCHAIN = true; // Change from false to true
```

### Step 6: Restart Application

```bash
# Stop current processes (Ctrl+C)

# Restart backend
cd backend-api
npm run dev

# Restart frontend (new terminal)
cd frontend
npm run dev
```

## 🔧 How It Works

### Wallet Connection Flow

```
User clicks "Connect Wallet"
    ↓
Frontend detects wallet extension
    ↓
Request wallet access
    ↓
User approves in wallet popup
    ↓
Get wallet address & balance
    ↓
Store connection state
```

### Transaction Flow

```
User creates project
    ↓
Frontend builds transaction
    ↓
Attach project datum
    ↓
User signs in wallet
    ↓
Submit to Cardano network
    ↓
Wait for confirmation
    ↓
Update UI with tx hash
```

## 📝 Code Examples

### Connect to Real Wallet

```typescript
import { getCardanoService } from './services/cardano';

const cardano = getCardanoService();

// Connect wallet
const { address, balance } = await cardano.connectWallet('nami');
console.log('Connected:', address);
console.log('Balance:', balance / 1_000_000, 'ADA');
```

### Lock Funds in Escrow

```typescript
const txHash = await cardano.lockFundsInEscrow({
  amount: 1000_000_000, // 1000 ADA
  scriptAddress: 'addr_test1w...',
  datum: {
    projectId: 'proj_123',
    client: 'addr_test1...',
    freelancer: 'addr_test1...',
    milestones: [...]
  }
});

console.log('Funds locked! Tx:', txHash);
```

### Release Funds

```typescript
const txHash = await cardano.releaseFundsFromEscrow({
  scriptUtxo: 'utxo_ref',
  redeemer: {
    action: 'ReleaseFunds',
    milestoneId: 1
  },
  recipient: 'addr_test1...'
});

console.log('Funds released! Tx:', txHash);
```

### Check Transaction Status

```typescript
const status = await cardano.getTransactionStatus(txHash);

if (status.confirmed) {
  console.log('Confirmed at block:', status.blockHeight);
} else {
  console.log('Pending...');
}
```

## 🔐 Security Considerations

### Private Keys
- ✅ Never stored in application
- ✅ Managed by wallet extension
- ✅ User signs each transaction

### Transaction Validation
- ✅ Amount limits enforced
- ✅ Recipient addresses verified
- ✅ User confirms in wallet popup

### Smart Contract Security
- ✅ Plutus validators enforce rules
- ✅ Funds locked until conditions met
- ✅ No centralized control

## 🧪 Testing on Testnet

### Test Scenario 1: Create Project

1. Connect wallet (Nami/Eternl/Flint)
2. Create new project with milestones
3. Sign transaction in wallet
4. Wait for confirmation (~20 seconds)
5. View transaction on CardanoScan

### Test Scenario 2: Complete Milestone

1. As freelancer, mark milestone complete
2. As client, approve milestone
3. Sign release transaction
4. Funds transferred to freelancer
5. Verify on blockchain explorer

### Test Scenario 3: Dispute

1. Raise dispute
2. Arbiter reviews
3. Arbiter resolves
4. Funds distributed per outcome

## 📊 Monitoring Transactions

### Blockchain Explorers

**Preprod Testnet:**
- https://preprod.cardanoscan.io/
- https://preprod.cexplorer.io/

**Mainnet:**
- https://cardanoscan.io/
- https://cexplorer.io/

### Check Transaction

```
https://preprod.cardanoscan.io/transaction/{txHash}
```

## 🚨 Troubleshooting

### Wallet Not Detected
```
Error: nami wallet not found
```
**Solution**: Install wallet extension and refresh page

### Insufficient Funds
```
Error: Not enough ADA
```
**Solution**: Get test ADA from faucet

### Transaction Failed
```
Error: Transaction submission failed
```
**Solution**: 
- Check network (preprod vs mainnet)
- Verify script address
- Ensure enough ADA for fees

### Blockfrost Rate Limit
```
Error: 429 Too Many Requests
```
**Solution**: 
- Wait a moment
- Upgrade Blockfrost plan
- Implement request caching

## 📈 Production Deployment

### Before Mainnet

1. **Audit Smart Contracts**
   - Professional security audit
   - Formal verification
   - Bug bounty program

2. **Test Thoroughly**
   - All user workflows
   - Edge cases
   - Concurrent transactions
   - Large amounts

3. **Set Up Monitoring**
   - Transaction tracking
   - Error logging
   - Performance metrics
   - Alert system

4. **Prepare Support**
   - User documentation
   - FAQ
   - Support channels
   - Emergency procedures

### Mainnet Configuration

```env
# Frontend
VITE_BLOCKFROST_PROJECT_ID=mainnet_project_id
VITE_BLOCKFROST_URL=https://cardano-mainnet.blockfrost.io/api/v0
VITE_NETWORK=Mainnet

# Backend
BLOCKFROST_PROJECT_ID=mainnet_project_id
CARDANO_NETWORK=mainnet
```

### Gradual Rollout

1. **Phase 1**: Limited beta (10 users)
2. **Phase 2**: Public beta (100 users)
3. **Phase 3**: Transaction limits ($1000)
4. **Phase 4**: Full launch

## 🎓 Learning Resources

### Cardano Development
- [Cardano Docs](https://docs.cardano.org/)
- [Plutus Pioneer Program](https://github.com/input-output-hk/plutus-pioneer-program)
- [Blockfrost Docs](https://docs.blockfrost.io/)

### Wallet Integration
- [CIP-30](https://cips.cardano.org/cips/cip30/) - Wallet API Standard
- [Nami Docs](https://docs.namiwallet.io/)
- [Eternl Docs](https://eternl.io/app/docs)

### Testing
- [Cardano Testnet](https://docs.cardano.org/cardano-testnet/)
- [Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/)
- [Explorer](https://preprod.cardanoscan.io/)

## ✅ Checklist

### Development
- [ ] Blockfrost API key obtained
- [ ] Wallet extension installed
- [ ] Test ADA received
- [ ] Environment configured
- [ ] Blockchain mode enabled
- [ ] Wallet connection tested
- [ ] Transaction tested

### Production
- [ ] Smart contracts audited
- [ ] Mainnet API key obtained
- [ ] Monitoring setup
- [ ] Support documentation
- [ ] Emergency procedures
- [ ] Insurance considered
- [ ] Legal compliance checked

## 🎉 Summary

You now have everything needed to enable live Cardano blockchain transactions:

1. **Cardano Service** - Wallet connection & transaction building
2. **Blockfrost Integration** - Blockchain API access
3. **Transaction Builder** - Smart contract interactions
4. **Status Tracking** - Monitor confirmations

**Next Step**: Get your Blockfrost API key and enable blockchain mode!

The application will then execute real transactions on Cardano testnet, with funds locked in Plutus smart contracts and released based on milestone completion.
