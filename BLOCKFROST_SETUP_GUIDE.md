# Blockfrost API Setup Guide

## What is Blockfrost?

Blockfrost is a free API service that provides access to the Cardano blockchain. It's required for TrustlessTask to submit real transactions to the blockchain.

## Step-by-Step Setup (5 minutes)

### Step 1: Create Blockfrost Account

1. **Visit Blockfrost website**:
   ```
   https://blockfrost.io
   ```

2. **Click "Sign Up" or "Get Started"**

3. **Create account**:
   - Enter your email
   - Create password
   - Verify email (check inbox/spam)

### Step 2: Create a Project

1. **Log in to Blockfrost dashboard**

2. **Click "Add Project" or "New Project"**

3. **Configure project**:
   - **Name**: TrustlessTask (or any name you like)
   - **Network**: Select **"Cardano Preprod"** (IMPORTANT!)
   - Click "Create Project"

### Step 3: Get Your API Key

1. **Find your project** in the dashboard

2. **Click on the project** to open details

3. **Copy the Project ID**:
   - It looks like: `preprodXXXXXXXXXXXXXXXXXXXXXXXXXXXX`
   - This is your API key!

### Step 4: Update Your .env File

1. **Open the file**: `frontend/.env`

2. **Find this line**:
   ```env
   VITE_BLOCKFROST_PROJECT_ID=preprodDemo123
   ```

3. **Replace with your actual key**:
   ```env
   VITE_BLOCKFROST_PROJECT_ID=preprodYourActualKeyHere
   ```

4. **Save the file**

### Step 5: Restart Frontend Server

1. **Stop the current server**:
   - Go to terminal running frontend
   - Press `Ctrl + C`

2. **Start it again**:
   ```bash
   cd frontend
   npm run dev
   ```

3. **Wait for it to start**:
   - Should show: `ready in XXXXms`
   - URL: `http://localhost:3000` or `http://localhost:3001`

### Step 6: Get Testnet ADA

Your wallet needs testnet ADA to pay transaction fees:

1. **Get your wallet address**:
   - Connect wallet in TrustlessTask
   - Copy your address from the navbar

2. **Visit Cardano Faucet**:
   ```
   https://docs.cardano.org/cardano-testnet/tools/faucet/
   ```

3. **Request testnet ADA**:
   - Paste your wallet address
   - Complete captcha
   - Click "Request"
   - Wait ~20 seconds

4. **Verify you received it**:
   - Check your wallet
   - Should show balance (e.g., 1000 tADA)

## Testing Real Transactions

Once setup is complete:

1. **Create a project** in TrustlessTask
2. **Complete a milestone** (as freelancer)
3. **Approve milestone** (as client)
4. **Watch for wallet popup** - Approve the transaction
5. **Transaction submitted** to Cardano blockchain!
6. **View on explorer**: https://preprod.cardanoscan.io

## Troubleshooting

### "Invalid project token" error

**Problem**: API key is wrong or not set

**Solution**:
- Double-check you copied the full key
- Make sure it starts with `preprod`
- No extra spaces before/after
- Restart frontend server after changing .env

### "No funds available" error

**Problem**: Wallet has no testnet ADA

**Solution**:
- Visit faucet and request testnet ADA
- Wait 20-30 seconds for confirmation
- Refresh wallet balance
- Try transaction again

### "Transaction failed" error

**Problem**: Various reasons

**Solutions**:
- Check wallet has enough ADA (need ~2 ADA for fees)
- Make sure wallet is unlocked
- Try different wallet (Nami, Lace, Eternl)
- Check Blockfrost status: https://status.blockfrost.io

### Frontend not picking up new API key

**Problem**: .env changes not loaded

**Solution**:
1. Stop frontend server (Ctrl+C)
2. Clear browser cache (Ctrl+Shift+Delete)
3. Start frontend again: `npm run dev`
4. Hard refresh browser (Ctrl+F5)

## Blockfrost Free Tier Limits

The free tier includes:

- ✅ **50,000 requests per day**
- ✅ **10 requests per second**
- ✅ **Unlimited projects**
- ✅ **All networks** (Preprod, Preview, Mainnet)

This is more than enough for development and testing!

## Security Notes

### ⚠️ Important

- **Never commit** your API key to Git
- **Never share** your API key publicly
- **Use .env files** (already in .gitignore)
- **Different keys** for dev/prod

### API Key Safety

Your `.env` file is already in `.gitignore`, so it won't be committed to Git. This keeps your API key safe.

## Upgrading to Mainnet

When ready for production:

1. **Create new Blockfrost project**:
   - Select "Cardano Mainnet" (not Preprod)
   - Get new API key

2. **Update .env**:
   ```env
   VITE_BLOCKFROST_PROJECT_ID=mainnetYourKeyHere
   VITE_NETWORK=mainnet
   ```

3. **Use real ADA**:
   - No faucet for mainnet
   - Real ADA costs real money
   - Test thoroughly on testnet first!

## Support

### Blockfrost Support

- **Documentation**: https://docs.blockfrost.io
- **Status Page**: https://status.blockfrost.io
- **Discord**: https://discord.gg/blockfrost

### TrustlessTask Support

- **Check console** (F12) for detailed error messages
- **Review logs** in terminal
- **Test on testnet** before mainnet

## Quick Reference

### Environment Variables

```env
# Required for blockchain transactions
VITE_BLOCKFROST_PROJECT_ID=preprodYourKeyHere

# Network (preprod for testing, mainnet for production)
VITE_NETWORK=preprod

# API URL (usually don't need to change)
VITE_API_URL=http://localhost:8080/api/v1
```

### Useful Links

- **Blockfrost**: https://blockfrost.io
- **Testnet Faucet**: https://docs.cardano.org/cardano-testnet/tools/faucet/
- **Preprod Explorer**: https://preprod.cardanoscan.io
- **Mainnet Explorer**: https://cardanoscan.io

## Next Steps

After setup:

1. ✅ Create test project
2. ✅ Complete milestone (sends 1 ADA to self)
3. ✅ Approve milestone (sends ADA to freelancer)
4. ✅ View transaction on explorer
5. ✅ Verify funds transferred

Congratulations! You're now making real Cardano blockchain transactions! 🎉
