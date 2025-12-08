# 🔗 Real Cardano Wallet Connection Guide

## ✅ Status: ENABLED

Real Cardano blockchain connection is now **ACTIVE**! The application will connect to actual Cardano wallets.

## 📋 Prerequisites

### 1. Install a Cardano Wallet

Choose and install ONE of these browser extensions:

#### Nami Wallet (Recommended)
- **Website**: https://namiwallet.io/
- **Chrome**: https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo
- **Features**: Simple, user-friendly, popular

#### Eternl Wallet
- **Website**: https://eternl.io/
- **Chrome**: https://chrome.google.com/webstore/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka
- **Features**: Advanced features, multi-account

#### Flint Wallet
- **Website**: https://flint-wallet.com/
- **Chrome**: https://chrome.google.com/webstore/detail/flint-wallet/hnhobjmcibchnmglfbldbfabcgaknlkj
- **Features**: Lightweight, fast

### 2. Set Up Your Wallet

1. **Install Extension**: Click install from Chrome Web Store
2. **Create Wallet**: Follow setup wizard
3. **Save Recovery Phrase**: Write down 24 words (KEEP SAFE!)
4. **Set Password**: Choose strong password
5. **Switch to Testnet**: 
   - Open wallet settings
   - Select "Preprod Testnet" network

### 3. Get Test ADA

1. **Copy Your Address**: From wallet
2. **Visit Faucet**: https://docs.cardano.org/cardano-testnet/tools/faucet/
3. **Request Funds**: Paste address, complete captcha
4. **Wait**: Receive 10,000 test ADA (~1 minute)

## 🚀 Connect to TrustlessTask

### Step 1: Open Application

```
http://localhost:3001
```

### Step 2: Click "Connect Wallet"

Top right corner of the application.

### Step 3: Choose Your Wallet

Select the wallet you installed (Nami/Eternl/Flint).

### Step 4: Approve Connection

A popup will appear from your wallet:
- Review permissions
- Click "Approve" or "Connect"

### Step 5: Verify Connection

You should see:
- ✅ Your wallet address (shortened)
- ✅ Your ADA balance
- ✅ "Disconnect" button

## 🎯 What You Can Do Now

### Create Real Projects

1. Click "Create Project"
2. Fill in details
3. Add milestones
4. **Sign Transaction** in wallet popup
5. Wait for blockchain confirmation (~20 seconds)
6. View on Cardano explorer!

### Complete Milestones

1. As freelancer: Mark milestone complete
2. **Sign transaction** in wallet
3. As client: Approve milestone
4. **Sign transaction** in wallet
5. Funds released on-chain!

### View Transactions

Every action creates a real blockchain transaction:
- View on https://preprod.cardanoscan.io/
- Search by transaction hash
- See on-chain data

## 🔍 Transaction Flow

```
User Action → Build Transaction → Wallet Popup → User Signs → Submit to Blockchain → Confirmation
```

### Example: Create Project

1. **User**: Fills form, clicks "Create Project"
2. **App**: Builds transaction with project data
3. **Wallet**: Shows transaction details
4. **User**: Reviews and signs
5. **Blockchain**: Processes transaction
6. **App**: Shows success + transaction hash

## 💰 Understanding Fees

### Transaction Costs (Testnet)

- **Create Project**: ~0.2 ADA
- **Complete Milestone**: ~0.2 ADA
- **Approve Milestone**: ~0.3 ADA (includes script execution)
- **Cancel Project**: ~0.2 ADA

### Mainnet Costs

- Similar amounts in real ADA
- Fees go to network validators
- Typical: 0.15-0.5 ADA per transaction

## 🛡️ Security

### Your Private Keys

- ✅ **Never** leave your device
- ✅ Stored only in wallet extension
- ✅ TrustlessTask **never** sees them
- ✅ You sign every transaction

### Transaction Safety

- ✅ Review every transaction in wallet
- ✅ Check amounts before signing
- ✅ Verify recipient addresses
- ✅ Can reject any transaction

### Best Practices

1. **Never share** recovery phrase
2. **Always verify** transaction details
3. **Use testnet** for learning
4. **Start small** on mainnet
5. **Keep wallet** updated

## 🐛 Troubleshooting

### Wallet Not Detected

**Problem**: "Wallet not found" error

**Solutions**:
1. Install wallet extension
2. Refresh browser page
3. Unlock wallet
4. Check wallet is on Preprod network

### Connection Failed

**Problem**: Can't connect to wallet

**Solutions**:
1. Check wallet is unlocked
2. Try different browser
3. Disable other wallet extensions
4. Clear browser cache

### Transaction Failed

**Problem**: Transaction rejected

**Solutions**:
1. Check you have enough ADA
2. Verify network (Preprod vs Mainnet)
3. Wait a moment and retry
4. Check wallet balance

### No Test ADA

**Problem**: Faucet not working

**Solutions**:
1. Wait 24 hours between requests
2. Try different faucet
3. Ask in Cardano Discord
4. Check address is correct

## 📊 Monitoring Your Transactions

### Cardano Explorer

**Preprod Testnet**:
```
https://preprod.cardanoscan.io/transaction/{YOUR_TX_HASH}
```

**What You'll See**:
- Transaction status
- Block number
- Timestamp
- Inputs/Outputs
- Fees paid
- Metadata

### In Application

- Transaction hash displayed after submission
- Click to view on explorer
- Status updates automatically
- Confirmation count shown

## 🎓 Learning More

### Cardano Basics

- **What is ADA?**: Cardano's native cryptocurrency
- **What is a UTXO?**: Unspent transaction output
- **What is a wallet?**: Software managing your keys
- **What is testnet?**: Practice network with fake ADA

### Resources

- [Cardano Docs](https://docs.cardano.org/)
- [Cardano Forum](https://forum.cardano.org/)
- [Cardano Discord](https://discord.gg/cardano)
- [Cardano Reddit](https://reddit.com/r/cardano)

## ✅ Verification Checklist

Before using real wallet:

- [ ] Wallet extension installed
- [ ] Wallet created and backed up
- [ ] Switched to Preprod testnet
- [ ] Received test ADA from faucet
- [ ] Application running (localhost:3001)
- [ ] Wallet unlocked
- [ ] Ready to connect!

## 🎉 You're Ready!

Your TrustlessTask application is now connected to the **real Cardano blockchain**!

### What This Means

✅ **Real Transactions**: Every action goes on-chain
✅ **Real Wallet**: Using actual Cardano wallet
✅ **Real Blockchain**: Interacting with Cardano network
✅ **Real Security**: Cryptographic signatures
✅ **Real Decentralization**: No central authority

### Next Steps

1. **Connect your wallet** in the app
2. **Create a test project** with small amounts
3. **Complete the workflow** end-to-end
4. **View transactions** on explorer
5. **Experiment** with features

### Moving to Mainnet

When ready for production:

1. Get real ADA
2. Update configuration to mainnet
3. Deploy smart contracts
4. Start with small amounts
5. Gradually increase limits

---

**Need Help?** Check the troubleshooting section or open an issue on GitHub.

**Have Fun!** You're now using real blockchain technology! 🚀
