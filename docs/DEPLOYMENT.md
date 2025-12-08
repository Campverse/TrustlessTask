# Deployment Guide

## Prerequisites

- GHC 9.2.8+
- Cabal 3.10+
- Cardano Node 8.7.0+
- Cardano CLI
- PostgreSQL 14+
- Node.js 18+
- Blockfrost API key

## Smart Contract Deployment

### 1. Build Contracts
```bash
cd contracts
cabal update
cabal build all
```

### 2. Generate Script Files
```bash
cabal run trustless-task-contracts -- write-scripts --out-dir ./plutus-scripts
```

### 3. Deploy to Testnet
```bash
chmod +x ../scripts/deploy.sh
../scripts/deploy.sh
```

### 4. Note Script Addresses
Save the generated addresses for configuration.

## Backend Deployment

### 1. Setup Database
```bash
createdb trustlesstask
psql trustlesstask < schema.sql
```

### 2. Configure Environment
```bash
cp .env.example .env
# Edit .env with your values
```

### 3. Build and Run
```bash
cd backend
cabal build
cabal run trustless-task-api
```

## Frontend Deployment

### 1. Install Dependencies
```bash
cd frontend
npm install
```

### 2. Configure Environment
```bash
cp .env.example .env
# Add Blockfrost API key
```

### 3. Build
```bash
npm run build
```

### 4. Deploy
```bash
# Deploy dist/ folder to hosting service
# (Vercel, Netlify, AWS S3, etc.)
```

## Production Checklist

- [ ] Smart contracts audited
- [ ] Environment variables secured
- [ ] Database backups configured
- [ ] SSL certificates installed
- [ ] Rate limiting enabled
- [ ] Monitoring setup
- [ ] Error tracking configured
- [ ] CDN configured for frontend
