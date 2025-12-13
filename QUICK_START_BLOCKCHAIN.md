# Quick Start: Enable Blockchain Transactions

## 🚀 5-Minute Setup

### 1️⃣ Get Blockfrost API Key (FREE)

```
👉 Visit: https://blockfrost.io
```

- Click "Sign Up"
- Verify email
- Create project → Select "Cardano Preprod"
- Copy your project ID

### 2️⃣ Update .env File

Open `frontend/.env` and replace this line:

```env
VITE_BLOCKFROST_PROJECT_ID=preprodDemo123
```

With your actual key:

```env
VITE_BLOCKFROST_PROJECT_ID=preprodYourActualKeyHere
```

### 3️⃣ Restart Frontend

```bash
# Stop current server (Ctrl+C)
cd frontend
npm run dev
```

### 4️⃣ Get Testnet ADA (FREE)

```
👉 Visit: https://docs.cardano.org/cardano-testnet/tools/faucet/
```

- Enter your wallet address
- Request testnet ADA
- Wait 20 seconds

### 5️⃣ Test It!

1. Create a project
2. Complete milestone → **Real blockchain transaction!**
3. Approve milestone → **Real ADA sent to freelancer!**
4. View on explorer: https://preprod.cardanoscan.io

## ✅ That's It!

You're now making real Cardano blockchain transactions!

## 📚 Need Help?

- **Detailed guide**: See `BLOCKFROST_SETUP_GUIDE.md`
- **Troubleshooting**: Check console (F12) for errors
- **Blockfrost docs**: https://docs.blockfrost.io

## 🎯 What Happens Next?

### When you complete a milestone:
- ✅ Transaction built with Lucid
- ✅ Wallet popup asks for approval
- ✅ Transaction submitted to Cardano blockchain
- ✅ Visible on block explorer
- ✅ Recorded permanently on-chain

### When you approve a milestone:
- ✅ Real ADA sent to freelancer
- ✅ Transaction fees paid from your wallet
- ✅ Funds transferred on blockchain
- ✅ Verifiable on explorer
- ✅ Immutable record created

## 💡 Pro Tips

- **Test on Preprod first** - It's free and safe
- **Get plenty of testnet ADA** - Faucet gives 1000 tADA
- **Check transactions on explorer** - Verify everything works
- **Keep API key secret** - Never commit to Git

## 🔒 Security

Your API key is safe:
- ✅ Stored in `.env` file
- ✅ Already in `.gitignore`
- ✅ Never committed to Git
- ✅ Only used locally

## 🌟 Ready for Production?

When ready for mainnet:
1. Create new Blockfrost project (Mainnet)
2. Update `.env` with mainnet key
3. Use real ADA (costs real money!)
4. Test thoroughly first!

---

**Questions?** Check `BLOCKFROST_SETUP_GUIDE.md` for detailed instructions!
