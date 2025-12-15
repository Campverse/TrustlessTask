# 🔗 Real Cardano Blockchain Transactions Guide

## ✅ What's Implemented

TrustlessTask now supports **real Cardano blockchain transactions** for:

1. **Milestone Approval & Fund Release** - Client approves work and releases payment to freelancer
2. **On-chain Metadata** - Transaction includes project and milestone details
3. **Blockfrost Integration** - Submits transactions to Cardano testnet/mainnet
4. **Wallet Signing** - Uses Nami, Lace, Eternl, or Flint to sign transactions

## 🚀 How It Works

### When Client Approves a Milestone:

1. **User clicks "Approve & Release Funds"**
2. **Frontend builds transaction** using Lucid library
3. **Wallet prompts for signature** (Nami/Lace/etc)
4. **Transaction submitted** to Cardano blockchain via Blockfrost
5. **Transaction hash returned** and stored in database
6. **Funds transferred** from client to freelancer on-chain

### Transaction Metadata:

Each transaction includes metadata (label 674):
```json
{
  "type": "milestone_payment",
  "projectId": "project_123",
  "milestoneId": 1,
  "projectTitle": "Build Website",
  "milestoneDescription": "Complete homepage design",
  "timestamp": "2024-12-15T16:00:00Z"
}
```

## 📋 Prerequisites

### 1. Cardano Wallet

Install one of these browser extensions:
- **Nami**: https://namiwallet.io/
- **Lace**: https://www.lace.io/
- **Eternl**: https://eternl.io/
- **Flint**: https://flint-wallet.com/

### 2. Testnet ADA

Get free testnet ADA from the faucet:

1. Visit: https://docs.cardano.org/cardano-testnet/tools/faucet/
2. Enter your wallet address
3. Request testnet ADA (free)
4. Wait ~20 seconds for confirmation

**Minimum Required**: 2 ADA per transaction (1 ADA + fees)

### 3. Blockfrost API Key

Get a free API key:

1. Visit: https://blockfrost.io
2. Sign up for free account
3. Create new project: **Cardano Preprod Testnet**
4. Copy your project ID (starts with `preprod...`)

### 4. Configure Environment

Create `frontend/.env`:

```env
VITE_BLOCKFROST_PROJECT_ID=preprodYourKeyHere
VITE_NETWORK=preprod
VITE_API_URL=http://localhost:8080/api/v1
```

Create `backend-api/.env`:

```env
BLOCKFROST_PROJECT_ID=preprodYourKeyHere
CARDANO_NETWORK=preprod
```

### 5. Restart Servers

```bash
# Terminal 1 - Backend
cd backend-api
npm run dev

# Terminal 2 - Frontend
cd frontend
npm run dev
```

## 🎯 Testing Real Transactions

### Step 1: Connect Wallet

1. Open http://localhost:3000
2. Click "Connect Wallet"
3. Select your wallet (Nami/Lace/etc)
4. Approve the connection
5. Verify your address displays

### Step 2: Create a Project

1. Click "Create Project"
2. Fill in project details:
   - Title: "Test Project"
   - Description: "Testing real blockchain transactions"
   - Freelancer Address: (use another wallet address or same for testing)
   - Milestone: 2,000,000 lovelace (2 ADA)
3. Click "Create Project"

### Step 3: Complete Milestone (Freelancer)

1. Go to project detail page
2. If you're the freelancer, click "Mark Complete"
3. This updates the database (no blockchain transaction)

### Step 4: Approve & Release Funds (Client)

1. As the client, click "Approve & Release Funds"
2. **Wallet will prompt for signature** - Review the transaction:
   - Amount: 2 ADA
   - Recipient: Freelancer address
   - Fee: ~0.17 ADA
3. Click "Confirm" in wallet
4. Wait for transaction to submit (~5 seconds)
5. Success! Transaction hash displayed

### Step 5: Verify on Blockchain

1. Copy the transaction hash
2. Visit: https://preprod.cardanoscan.io/
3. Paste transaction hash
4. View transaction details:
   - ✅ Amount transferred
   - ✅ Metadata included
   - ✅ Confirmed on blockchain

## 🔍 Transaction Flow

```
Client Wallet (2 ADA) 
    ↓
[Build Transaction with Lucid]
    ↓
[Sign with Wallet]
    ↓
[Submit to Blockfrost API]
    ↓
[Cardano Blockchain]
    ↓
Freelancer Wallet (+2 ADA)
```

## 💡 Key Features

### 1. Real Blockchain Transactions
- ✅ Actual ADA transferred on Cardano
- ✅ Immutable transaction records
- ✅ Verifiable on blockchain explorers

### 2. Metadata Tracking
- ✅ Project and milestone info on-chain
- ✅ Timestamp and description
- ✅ Searchable transaction history

### 3. Wallet Integration
- ✅ Multiple wallet support
- ✅ Secure signing process
- ✅ Balance checking

### 4. Error Handling
- ✅ Insufficient funds detection
- ✅ Invalid API key warnings
- ✅ Network error recovery
- ✅ Fallback to simulated mode

## 🛠️ Troubleshooting

### "Lucid library not available"

**Cause**: Running in production build mode

**Solution**: Use development mode
```bash
cd frontend
npm run dev
```

### "Wallet not connected"

**Cause**: Wallet extension not detected or not connected

**Solution**:
1. Install wallet extension
2. Create/unlock wallet
3. Click "Connect Wallet" in app
4. Approve connection

### "No funds available in wallet"

**Cause**: Wallet has 0 ADA

**Solution**:
1. Visit testnet faucet
2. Request testnet ADA
3. Wait for confirmation
4. Refresh and try again

### "Invalid project token"

**Cause**: Blockfrost API key is invalid or missing

**Solution**:
1. Get API key from https://blockfrost.io
2. Add to `frontend/.env`
3. Restart frontend server

### "Insufficient funds"

**Cause**: Not enough ADA for transaction + fees

**Solution**:
- Need at least 2 ADA per transaction
- Request more from faucet
- Check wallet balance

### Transaction Pending Forever

**Cause**: Network congestion or invalid transaction

**Solution**:
1. Check transaction on explorer
2. Wait 2-3 minutes
3. If still pending, try again
4. Check Blockfrost status page

## 📊 Transaction Costs

### Testnet (Free):
- **ADA**: Free from faucet
- **Fees**: ~0.17 ADA per transaction
- **API**: Free (Blockfrost)

### Mainnet (Real Money):
- **ADA**: Purchase from exchange
- **Fees**: ~0.17 ADA per transaction
- **API**: Free tier available (Blockfrost)

## 🔐 Security Notes

### ✅ Safe:
- Wallet never shares private keys
- Transactions signed locally
- User approves each transaction
- Metadata is public (by design)

### ⚠️ Important:
- Always verify transaction details before signing
- Check recipient address carefully
- Start with small amounts on testnet
- Keep wallet seed phrase secure

## 🌐 Network Configuration

### Testnet (Current):
```env
VITE_NETWORK=preprod
VITE_BLOCKFROST_PROJECT_ID=preprodYourKey
```
- Free testnet ADA
- Safe for testing
- No real money

### Mainnet (Production):
```env
VITE_NETWORK=mainnet
VITE_BLOCKFROST_PROJECT_ID=mainnetYourKey
```
- Real ADA required
- Real money transactions
- Use with caution

## 📈 Next Steps

### Current Implementation:
- ✅ Milestone payment transactions
- ✅ Metadata on-chain
- ✅ Blockfrost submission

### Future Enhancements:
- 🔄 Escrow smart contracts (Plutus)
- 🔄 Multi-signature approvals
- 🔄 Automated dispute resolution
- 🔄 Staking rewards for arbiters
- 🔄 NFT certificates for completed projects

## 🎉 Success Checklist

- [ ] Wallet installed and funded
- [ ] Blockfrost API key configured
- [ ] Development servers running
- [ ] Wallet connected to app
- [ ] Test project created
- [ ] Milestone completed
- [ ] Funds approved and released
- [ ] Transaction verified on explorer
- [ ] Freelancer received payment

## 📞 Support

**Issues?**
- Check browser console (F12) for detailed logs
- Review transaction on Cardano explorer
- Verify Blockfrost API key is valid
- Ensure wallet has sufficient funds

**Resources:**
- Blockfrost Docs: https://docs.blockfrost.io
- Cardano Docs: https://docs.cardano.org
- Lucid Docs: https://lucid.spacebudz.io

---

**Status**: ✅ Real blockchain transactions fully implemented
**Network**: Cardano Preprod Testnet
**Last Updated**: December 15, 2024
