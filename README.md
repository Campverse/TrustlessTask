# TrustlessTask

A decentralized freelance marketplace built on Cardano blockchain using Plutus smart contracts. Enables trustless collaboration between clients and freelancers through programmable escrow and milestone-based payments.

## Features

- **Smart Contract Escrow**: Funds locked in Plutus validators until conditions met
- **Milestone-Based Payments**: Break projects into verifiable milestones
- **Dispute Resolution**: Decentralized arbitration mechanism
- **Reputation System**: On-chain reputation tracking
- **Multi-Signature Releases**: Client approval required for fund release

## Architecture

```
┌─────────────┐         ┌──────────────┐         ┌─────────────┐
│   Frontend  │────────▶│   Backend    │────────▶│   Cardano   │
│  (React +   │         │  (Haskell +  │         │   Node      │
│   Lucid)    │         │   Servant)   │         │  (Plutus)   │
└─────────────┘         └──────────────┘         └─────────────┘
```

## Tech Stack

- **Smart Contracts**: Plutus (Haskell)
- **Backend**: Haskell + Servant API
- **Frontend**: React + TypeScript + Lucid
- **Blockchain**: Cardano (Testnet/Mainnet)

## Project Structure

```
trustless-task/
├── contracts/          # Plutus smart contracts
├── backend/           # Haskell backend API
├── frontend/          # React frontend
├── scripts/           # Deployment & testing scripts
└── docs/             # Documentation
```

## Quick Start

### Prerequisites

- GHC 9.2.8
- Cabal 3.10+
- Node.js 18+
- Cardano Node & CLI
- Plutus dependencies

### Setup

```bash
# Clone repository
git clone https://github.com/yourusername/trustless-task.git
cd trustless-task

# Build contracts
cd contracts
cabal build

# Start backend
cd ../backend
cabal run trustless-task-api

# Start frontend
cd ../frontend
npm install
npm run dev
```

## Smart Contract Overview

### EscrowValidator
Manages locked funds with milestone-based release conditions.

### DisputeValidator
Handles arbitration when parties disagree.

### ReputationValidator
Tracks on-chain reputation scores.

## License

MIT
