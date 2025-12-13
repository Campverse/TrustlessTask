# Nami vs Lace Wallet Guide

## Understanding the Issue

When you have **Lace wallet** installed and click "Connect Nami", the app uses Lace wallet instead because:

1. Lace extension includes Nami compatibility mode
2. Lace has its own separate wallet with different addresses
3. Your Nami wallet address is different from your Lace wallet address

## The Address Difference

- **Lace Wallet Address**: Starts with `addr1...` (what you're seeing now)
- **Nami Wallet Address**: Also starts with `addr1...` but is a completely different address

These are **two separate wallets** with different seed phrases and addresses.

## Solutions

### Option 1: Install Standalone Nami Extension (Recommended)

1. Go to https://namiwallet.io/
2. Install the Nami browser extension
3. Set up or restore your Nami wallet
4. Refresh TrustlessTask
5. Click "Connect Wallet" → "Nami"
6. Now it will use your actual Nami wallet address

### Option 2: Import Nami Seed Phrase into Lace

1. Open Lace wallet extension
2. Go to Settings → Wallet
3. Click "Add Wallet" or "Restore Wallet"
4. Enter your Nami wallet's 24-word seed phrase
5. Your Nami addresses will now be available in Lace
6. Refresh TrustlessTask and connect

### Option 3: Use Lace Wallet

If you want to use Lace wallet (the current connected wallet):
1. This is already working!
2. The address shown is your Lace wallet address
3. You can fund it and use it for transactions
4. Just know it's different from your Nami wallet

## How to Check Which Wallet is Connected

The app now shows the wallet name in the navbar:
- Look for the blue badge showing "LACE" or "NAMI"
- This tells you which wallet is actually connected

## Important Notes

⚠️ **Different Wallets = Different Addresses**
- Lace and Nami are separate wallets
- They have different seed phrases
- They have different addresses
- Funds in one wallet are NOT in the other

✅ **Both Wallets Work**
- You can use either Lace or Nami
- Both are legitimate Cardano wallets
- Both work with TrustlessTask

🔐 **Security**
- Never share your seed phrase
- Keep your seed phrases backed up separately
- Each wallet needs its own backup

## Current Status

Your app is currently connected to: **LACE WALLET**

Address: The one shown in the navbar (starts with `addr1...`)

To use your Nami wallet address instead, follow Option 1 or Option 2 above.
