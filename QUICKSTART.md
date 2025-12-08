# TrustlessTask - Quick Start Guide

## Current Status

This is a complete Web3 project design for a decentralized freelance marketplace on Cardano. The project includes:

✅ **Smart Contracts** - Complete Plutus validators (Escrow, Dispute, Reputation)
✅ **Backend API** - Haskell + Servant REST API
✅ **Frontend** - React + TypeScript + Lucid wallet integration
✅ **Documentation** - Architecture, API, deployment guides

## Running the Project

### Prerequisites Required

Since this is a Cardano blockchain project, you'll need:

1. **For Smart Contracts:**
   - GHC 9.2.8+ (Haskell compiler)
   - Cabal 3.10+
   - Plutus dependencies
   - Cardano Node & CLI

2. **For Backend:**
   - GHC 9.2.8+
   - PostgreSQL 14+
   - Cardano Node connection

3. **For Frontend:**
   - Node.js 18+
   - npm or yarn
   - Cardano wallet browser extension (Nami/Eternl/Flint)

### Quick Demo (Frontend Only)

To see the UI without blockchain connection:

```bash
# Install dependencies
cd frontend
npm install

# Start development server
npm run dev
```

Then open http://localhost:3000

**Note:** Without the backend and Cardano node, wallet features won't work, but you can see the UI design.

### Full Stack Setup

#### 1. Smart Contracts
```bash
cd contracts
cabal update
cabal build
cabal run trustless-task-contracts -- write-scripts --out-dir ./plutus-scripts
```

#### 2. Backend API
```bash
cd backend
cp ../.env.example .env
# Edit .env with your Cardano node socket path
cabal build
cabal run trustless-task-api
```

#### 3. Frontend
```bash
cd frontend
npm install
npm run dev
```

## Project Architecture

```
┌─────────────────┐
│  React Frontend │  ← User Interface
│  (Port 3000)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Haskell API    │  ← Transaction Builder
│  (Port 8080)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Cardano Node   │  ← Blockchain
│  (Plutus)       │
└─────────────────┘
```

## Key Features Implemented

### Smart Contracts (Plutus)
- **EscrowValidator**: Milestone-based fund releases
- **DisputeValidator**: Decentralized arbitration
- **ReputationValidator**: On-chain reputation tracking

### Backend (Haskell)
- Project creation and management
- Transaction building and submission
- Database integration
- RESTful API endpoints

### Frontend (React)
- Wallet connection (Nami, Eternl, Flint)
- Project creation with milestones
- Milestone tracking and approval
- User profiles and reputation
- Responsive design

## Development Workflow

1. **Design Phase** ✅ (Complete)
2. **Smart Contract Development** ✅ (Complete)
3. **Backend API Development** ✅ (Complete)
4. **Frontend Development** ✅ (Complete)
5. **Testing** ⏳ (Next: Unit tests, integration tests)
6. **Audit** ⏳ (Next: Security audit)
7. **Testnet Deployment** ⏳ (Next: Deploy to Cardano testnet)
8. **Mainnet Launch** ⏳ (Future)

## Next Steps

To make this production-ready:

1. **Add Tests**
   - Plutus contract tests
   - Backend API tests
   - Frontend component tests

2. **Setup Infrastructure**
   - PostgreSQL database
   - Cardano node (testnet)
   - Blockfrost API key

3. **Deploy to Testnet**
   - Run deployment script
   - Test with real transactions
   - Gather feedback

4. **Security Audit**
   - Smart contract audit
   - Backend security review
   - Frontend security scan

## Documentation

- `README.md` - Project overview
- `docs/ARCHITECTURE.md` - System architecture
- `docs/SMART_CONTRACTS.md` - Contract specifications
- `docs/API.md` - API endpoints
- `docs/DEPLOYMENT.md` - Deployment guide
- `docs/USER_GUIDE.md` - End-user guide

## Support

This is a demonstration project showing a complete Web3 architecture. For production use, you'll need:
- Cardano development environment
- Testnet ADA for testing
- Blockfrost or similar API service
- Database setup
- Hosting infrastructure

## License

MIT
