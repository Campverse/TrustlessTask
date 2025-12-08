# Complete Setup Guide - TrustlessTask

## 🎯 Current Working Status

### ✅ What's Working NOW
1. **Backend API** - Node.js/TypeScript REST API
2. **Frontend** - React application with full UI
3. **Database** - JSON-based persistent storage
4. **All CRUD Operations** - Create, read, update projects

### ⏳ What Requires Additional Setup
1. **Plutus Smart Contracts** - Requires Haskell + Cardano toolchain
2. **Blockchain Integration** - Requires Cardano node

## 📦 Installation Guide

### Part 1: Working Application (Current)

#### Prerequisites
- ✅ Node.js 18+ (Already installed)
- ✅ npm (Already installed)

#### Quick Start (5 minutes)
```bash
# 1. Start Backend API
cd backend-api
npm install
npm run dev
# Backend running at http://localhost:8080

# 2. Start Frontend (new terminal)
cd frontend
npm install
npm run dev
# Frontend running at http://localhost:3000
```

**Status**: ✅ **FULLY WORKING** - You can use the application now!

---

### Part 2: Plutus Smart Contracts (Advanced)

#### Prerequisites for Plutus Development

##### 1. Install GHC (Glasgow Haskell Compiler)

**Windows:**
```powershell
# Install GHCup (Haskell installer)
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true

# Install specific GHC version
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8

# Install Cabal
ghcup install cabal 3.10.1.0
ghcup set cabal 3.10.1.0
```

**Linux/Mac:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.2.8
ghcup set ghc 9.2.8
ghcup install cabal 3.10.1.0
ghcup set cabal 3.10.1.0
```

##### 2. Install Plutus Dependencies

```bash
# Update Cabal package list
cabal update

# Install required system libraries (Linux)
sudo apt-get install -y \
  libsodium-dev \
  libgmp-dev \
  libssl-dev \
  libtinfo-dev \
  libsystemd-dev \
  zlib1g-dev \
  build-essential \
  curl \
  libncursesw5

# For Windows, use MSYS2:
# Download from https://www.msys2.org/
# Then install dependencies via pacman
```

##### 3. Build Plutus Contracts

```bash
cd contracts

# Update dependencies
cabal update

# Build contracts
cabal build all

# Run tests
cabal test

# Generate script files
cabal run trustless-task-contracts -- write-scripts --out-dir ./plutus-scripts
```

##### 4. Install Cardano Node (Optional - for blockchain integration)

**Using Docker (Recommended):**
```bash
# Pull Cardano node image
docker pull inputoutput/cardano-node:latest

# Run testnet node
docker run -d \
  --name cardano-node \
  -p 3001:3001 \
  -v cardano-node-data:/data \
  inputoutput/cardano-node:latest \
  run --config /config/testnet-config.json
```

**Manual Installation:**
```bash
# Clone Cardano node
git clone https://github.com/input-output-hk/cardano-node.git
cd cardano-node

# Build (takes 30-60 minutes)
cabal build cardano-node cardano-cli

# Download testnet configuration
curl -O https://book.world.dev.cardano.org/environments/preprod/config.json
curl -O https://book.world.dev.cardano.org/environments/preprod/topology.json
curl -O https://book.world.dev.cardano.org/environments/preprod/byron-genesis.json
curl -O https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json
curl -O https://book.world.dev.cardano.org/environments/preprod/alonzo-genesis.json
curl -O https://book.world.dev.cardano.org/environments/preprod/conway-genesis.json

# Run node
cardano-node run \
  --topology topology.json \
  --database-path db \
  --socket-path node.socket \
  --config config.json
```

## 🔧 Package Requirements

### Frontend Packages (✅ Installed)
```json
{
  "react": "^18.2.0",
  "react-dom": "^18.2.0",
  "react-router-dom": "^6.20.0",
  "@tanstack/react-query": "^5.14.2",
  "axios": "^1.6.2",
  "date-fns": "^3.0.0"
}
```

### Backend API Packages (✅ Installed)
```json
{
  "express": "^4.18.2",
  "cors": "^2.8.5",
  "dotenv": "^16.3.1",
  "uuid": "^9.0.1",
  "zod": "^3.22.4",
  "lowdb": "^7.0.1"
}
```

### Plutus Contract Dependencies (⏳ Requires Setup)
```cabal
build-depends:
  base >= 4.14 && < 5,
  aeson,
  bytestring,
  containers,
  serialise,
  text,
  plutus-core,
  plutus-ledger-api,
  plutus-tx,
  plutus-tx-plugin,
  plutus-script-utils
```

## 🚀 Deployment Options

### Option 1: Current Setup (Recommended for Demo)
**What you have now:**
- ✅ Full-stack web application
- ✅ REST API with database
- ✅ Complete UI with all features
- ✅ Project management
- ✅ Milestone tracking
- ✅ User profiles

**Missing:**
- ❌ Blockchain transactions
- ❌ Real Cardano wallet integration
- ❌ On-chain smart contracts

**Use Case**: Demo, development, testing, MVP

### Option 2: Full Blockchain Integration
**Requires:**
- ✅ Everything from Option 1
- ⏳ Plutus contracts compiled
- ⏳ Cardano node running
- ⏳ Wallet integration (Nami/Eternl)
- ⏳ Blockfrost API key

**Use Case**: Production, mainnet deployment

## 📝 Step-by-Step: Getting Contracts Working

### Step 1: Install Haskell Toolchain
```bash
# Check if GHC is installed
ghc --version
# Should show: The Glorious Glasgow Haskell Compilation System, version 9.2.8

# Check if Cabal is installed
cabal --version
# Should show: cabal-install version 3.10.1.0
```

### Step 2: Install Plutus Dependencies
```bash
cd contracts

# This will download and install all Plutus packages
# WARNING: This can take 1-2 hours on first run
cabal update
cabal build all
```

### Step 3: Verify Contracts Compile
```bash
# Should compile without errors
cabal build

# Run tests
cabal test

# Generate validator scripts
cabal run trustless-task-contracts -- write-scripts --out-dir ./plutus-scripts
```

### Step 4: Deploy to Testnet
```bash
# Make deploy script executable
chmod +x ../scripts/deploy.sh

# Run deployment
../scripts/deploy.sh
```

## 🎯 Quick Decision Guide

### "I want to demo the application NOW"
→ Use current setup (Option 1)
→ No additional packages needed
→ Everything works out of the box

### "I want to deploy to Cardano blockchain"
→ Follow Part 2 installation
→ Install GHC, Cabal, Plutus
→ Set up Cardano node
→ Deploy contracts to testnet

### "I want to develop smart contracts"
→ Install full Haskell toolchain
→ Set up Plutus development environment
→ Use Plutus Application Backend (PAB)

## 🔍 Verification Commands

### Check Current Setup
```bash
# Backend API
curl http://localhost:8080/health

# Frontend
curl http://localhost:3000

# Create a test project
curl -X POST http://localhost:8080/api/v1/projects \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Test Project",
    "description": "Testing the API",
    "clientAddress": "addr_test1...",
    "freelancerAddress": "addr_test1...",
    "totalAmount": 1000000000,
    "milestones": [{
      "description": "Milestone 1",
      "amount": 1000000000,
      "deadline": "2025-12-31T23:59:59Z"
    }]
  }'
```

### Check Haskell Setup (if installed)
```bash
# GHC version
ghc --version

# Cabal version
cabal --version

# Try building contracts
cd contracts
cabal build
```

## 📚 Additional Resources

### Learning Plutus
- [Plutus Pioneer Program](https://github.com/input-output-hk/plutus-pioneer-program)
- [Plutus Documentation](https://plutus.readthedocs.io/)
- [Cardano Developer Portal](https://developers.cardano.org/)

### Tools
- [Blockfrost API](https://blockfrost.io/) - Cardano API service
- [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet/) - Get test ADA
- [Nami Wallet](https://namiwallet.io/) - Browser wallet

## 🎉 Summary

### What's Working Right Now ✅
Your TrustlessTask application is **100% functional** as a web application with:
- Complete backend API
- Full-featured frontend
- Database persistence
- All business logic working

### What Requires Additional Setup ⏳
The Plutus smart contracts require:
- Haskell compiler (GHC 9.2.8)
- Cabal build tool (3.10+)
- Plutus libraries
- Cardano node (for blockchain)

### Recommendation
**Start using the application now** with the current setup. Add blockchain integration later when you're ready to deploy to Cardano mainnet.

The application works perfectly for:
- Development
- Testing
- Demos
- MVP validation
- User feedback

Add Plutus contracts when you need:
- Actual blockchain transactions
- Decentralized escrow
- On-chain reputation
- Mainnet deployment
