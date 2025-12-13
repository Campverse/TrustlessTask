# Real Cardano Blockchain Transaction Setup

## Current Implementation Status

The application now attempts to build and sign real Cardano transactions when approving milestones. Here's what happens:

### Milestone Approval Flow

1. **User clicks "Approve & Release Funds"**
2. **Frontend builds transaction:**
   - Gets wallet UTxOs
   - Creates transaction to send ADA to freelancer
   - Adds metadata (projectId, milestoneId, timestamp)
3. **Wallet signs transaction:**
   - User sees wallet popup
   - User approves transaction
   - Wallet returns signed transaction
4. **Backend submits to blockchain:**
   - Receives signed transaction
   - Submits to Cardano network via Blockfrost
   - Returns transaction hash
5. **Transaction confirmed:**
   - Visible on Cardano explorer
   - Funds transferred on-chain
   - Milestone marked as approved

## Requirements for Real Transactions

### 1. Blockfrost API Key

Get a free API key from [Blockfrost.io](https://blockfrost.io/):

1. Sign up for account
2. Create new project (Preprod Testnet)
3. Copy your project ID

### 2. Update Environment Variables

**Backend** (`backend-api/.env`):
```env
BLOCKFROST_PROJECT_ID=preprodYourActualKeyHere
CARDANO_NETWORK=preprod
```

**Frontend** (`frontend/.env`):
```env
VITE_BLOCKFROST_PROJECT_ID=preprodYourActualKeyHere
VITE_NETWORK=preprod
```

### 3. Get Testnet ADA

Your wallet needs testnet ADA to pay transaction fees:

1. Go to [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/)
2. Enter your wallet address
3. Request testnet ADA (you'll receive ~1000 tADA)
4. Wait for confirmation (~20 seconds)

### 4. Install Transaction Library (Optional)

For more robust transaction building:

```bash
cd frontend
npm install @meshsdk/core
```

Or use the Cardano Serialization Library:

```bash
npm install @emurgo/cardano-serialization-lib-browser
```

## Current Limitations

### Transaction Building

The current implementation uses a simplified transaction builder. For production, you should:

1. **Use proper CBOR serialization** - Current version uses JSON
2. **Calculate fees correctly** - Need to query protocol parameters
3. **Handle UTxO selection** - Smart coin selection algorithm
4. **Add change output** - Return excess ADA to sender
5. **Set TTL (Time To Live)** - Transaction expiration

### Smart Contract Integration

For full escrow functionality:

1. **Deploy Plutus contracts** to testnet
2. **Lock funds in script** when creating project
3. **Spend from script** when approving milestones
4. **Include datum and redeemer** in transactions

## Testing Real Transactions

### Step 1: Setup

```bash
# 1. Get Blockfrost API key
# 2. Add to .env files
# 3. Get testnet ADA
# 4. Restart servers
```

### Step 2: Create Test Project

1. Connect wallet (must have testnet ADA)
2. Create project with small amount (e.g., 10 ADA)
3. Use different address for freelancer
4. Submit project

### Step 3: Complete & Approve Milestone

1. Switch to freelancer wallet (or use different browser)
2. Mark milestone as complete
3. Switch back to client wallet
4. Click "Approve & Release Funds"
5. **Wallet popup appears** - Review and approve
6. Transaction submitted to blockchain
7. Check transaction on explorer

### Step 4: Verify on Blockchain

Visit Cardano Explorer:
```
https://preprod.cardanoscan.io/transaction/YOUR_TX_HASH
```

You should see:
- ✅ Transaction confirmed
- ✅ Funds transferred
- ✅ Metadata attached
- ✅ Block number and timestamp

## Production Deployment

### Mainnet Checklist

Before deploying to mainnet:

- [ ] Thorough testing on preprod testnet
- [ ] Smart contracts audited
- [ ] Transaction fees calculated correctly
- [ ] Error handling for all edge cases
- [ ] User confirmation dialogs
- [ ] Transaction monitoring and retries
- [ ] Backup wallet recovery
- [ ] Legal compliance review
- [ ] Insurance/dispute resolution process

### Security Considerations

1. **Never store private keys** - Always use wallet extensions
2. **Validate all inputs** - Check addresses, amounts, etc.
3. **Use HTTPS only** - Protect API keys and transactions
4. **Rate limiting** - Prevent spam transactions
5. **Transaction limits** - Set maximum amounts
6. **Multi-sig for large amounts** - Require multiple approvals
7. **Audit trail** - Log all transactions
8. **Backup systems** - Handle Blockfrost downtime

## Troubleshooting

### "No UTxOs available"

**Problem:** Wallet has no funds or all UTxOs are locked

**Solution:**
- Check wallet balance
- Wait for pending transactions to confirm
- Get more testnet ADA from faucet

### "Transaction submission failed"

**Problem:** Invalid transaction or network issue

**Solution:**
- Check Blockfrost API key is valid
- Verify network setting (preprod vs mainnet)
- Check transaction format
- Try again after a few seconds

### "Wallet signature rejected"

**Problem:** User cancelled or wallet error

**Solution:**
- User must approve in wallet popup
- Check wallet is unlocked
- Try different wallet (Nami, Lace, Eternl)

### "Insufficient funds"

**Problem:** Not enough ADA for transaction + fees

**Solution:**
- Get more testnet ADA
- Reduce transaction amount
- Wait for pending transactions

## Advanced Features

### Escrow Smart Contract

To use real escrow (funds locked in script):

1. Deploy escrow contract to testnet
2. Get script address
3. Update project creation to lock funds
4. Update approval to spend from script
5. Include proper datum and redeemer

### Metadata Standards

Follow CIP-20 for transaction metadata:

```json
{
  "674": {
    "msg": ["TrustlessTask Milestone Approval"],
    "projectId": "uuid",
    "milestoneId": 1,
    "amount": 10000000,
    "timestamp": "2024-01-01T00:00:00Z"
  }
}
```

### Multi-Signature

For high-value projects:

1. Require multiple approvals
2. Use native scripts or Plutus
3. Implement voting mechanism
4. Add arbiter signature requirement

## Resources

- [Cardano Documentation](https://docs.cardano.org/)
- [Blockfrost API Docs](https://docs.blockfrost.io/)
- [Mesh SDK](https://meshjs.dev/)
- [Cardano Serialization Lib](https://github.com/Emurgo/cardano-serialization-lib)
- [CIP Standards](https://cips.cardano.org/)
- [Plutus Documentation](https://plutus.readthedocs.io/)

## Support

For issues with blockchain integration:

1. Check console logs (F12)
2. Verify Blockfrost API key
3. Test with small amounts first
4. Review transaction on explorer
5. Check wallet extension logs
