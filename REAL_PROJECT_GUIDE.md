# TrustlessTask - Real Working Project Guide

## 🎉 Your Project is Now Fully Functional!

This is a **real working full-stack application** with:
- ✅ **Backend API** (Node.js + Express + TypeScript)
- ✅ **Frontend** (React + TypeScript + Vite)
- ✅ **Database** (LowDB - JSON-based)
- ✅ **Real-time data persistence**
- ✅ **Full CRUD operations**

## 🚀 Currently Running

### Backend API
- **URL**: http://localhost:8080
- **Status**: ✅ Running
- **Database**: `backend-api/data/db.json`

### Frontend
- **URL**: http://localhost:3000
- **Status**: ✅ Running
- **Mode**: Connected to real backend

## 📋 Features Working

### ✅ Project Management
- Create new projects with milestones
- View all projects
- View project details
- Track milestone progress

### ✅ Milestone System
- Mark milestones as complete (freelancer)
- Approve milestones (client)
- Automatic status updates
- Progress tracking

### ✅ User Profiles
- View user statistics
- Track completed projects
- Monitor earnings
- Reputation system

### ✅ Dispute Resolution
- Raise disputes
- Track dispute status
- Resolve disputes

## 🧪 Testing the Application

### 1. Open the Frontend
Visit: http://localhost:3000

### 2. Connect Wallet (Demo Mode)
- Click "Connect Wallet"
- Select any wallet (Nami/Eternl/Flint)
- Demo wallet will be connected automatically

### 3. Create a Project
1. Click "Create Project"
2. Fill in project details:
   - Title: "Build a Website"
   - Description: "Create a responsive website"
   - Freelancer Address: `addr_test1qpw0djgj0x59ngrjvqthn7enhvruxnsavsw5th63la3mjel3tkc974sr23jmlzgq5zda4gtv8k9cy38756r9y3qgmkqqjz6aa7`
3. Add milestones:
   - Milestone 1: "Design" - 1000000000 lovelace (1000 ADA)
   - Milestone 2: "Development" - 2000000000 lovelace (2000 ADA)
4. Click "Create Project"

### 4. Test Milestone Flow
1. View the created project
2. As freelancer: Click "Mark Complete" on a milestone
3. As client: Click "Approve & Release Funds"
4. Watch the status update in real-time!

## 📊 API Endpoints

### Projects
```
POST   /api/v1/projects                    - Create project
GET    /api/v1/projects                    - List all projects
GET    /api/v1/projects/:id                - Get project details
POST   /api/v1/projects/:id/milestone/:mid/complete
POST   /api/v1/projects/:id/milestone/:mid/approve
POST   /api/v1/projects/:id/cancel         - Cancel project
```

### Users
```
GET    /api/v1/users/:address/profile      - Get user profile
```

### Disputes
```
POST   /api/v1/disputes                    - Create dispute
GET    /api/v1/disputes/:id                - Get dispute details
POST   /api/v1/disputes/:id/resolve        - Resolve dispute
```

### Health Check
```
GET    /health                             - API health status
```

## 🗄️ Database

The application uses **LowDB** (JSON-based database):
- **Location**: `backend-api/data/db.json`
- **Format**: Human-readable JSON
- **Persistence**: Automatic
- **Backup**: Just copy the JSON file

### View Database
```bash
cat backend-api/data/db.json
```

### Reset Database
```bash
rm backend-api/data/db.json
# Restart backend - it will create a fresh database
```

## 🔧 Development Commands

### Backend
```bash
cd backend-api

# Start development server (with hot reload)
npm run dev

# Build for production
npm run build

# Start production server
npm start
```

### Frontend
```bash
cd frontend

# Start development server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## 📁 Project Structure

```
TrustlessTask/
├── backend-api/              # Node.js Backend
│   ├── src/
│   │   ├── server.ts        # Express server
│   │   ├── db/
│   │   │   └── database.ts  # LowDB setup
│   │   ├── models/          # Data models
│   │   │   ├── Project.ts
│   │   │   ├── User.ts
│   │   │   └── Dispute.ts
│   │   └── routes/          # API routes
│   │       ├── projects.ts
│   │       ├── users.ts
│   │       └── disputes.ts
│   ├── data/                # Database storage
│   │   └── db.json
│   └── package.json
│
├── frontend/                 # React Frontend
│   ├── src/
│   │   ├── App.tsx
│   │   ├── components/      # React components
│   │   ├── pages/           # Page components
│   │   ├── services/        # API client
│   │   ├── hooks/           # Custom hooks
│   │   └── types/           # TypeScript types
│   └── package.json
│
├── contracts/                # Plutus Smart Contracts (for reference)
├── backend/                  # Haskell Backend (for reference)
└── docs/                     # Documentation
```

## 🔄 Data Flow

```
User Action (Frontend)
    ↓
API Request (axios)
    ↓
Express Route Handler
    ↓
Model Method
    ↓
LowDB (JSON file)
    ↓
Response to Frontend
    ↓
UI Update (React)
```

## 🎯 Next Steps

### For Development
1. **Add Authentication**: Implement JWT or session-based auth
2. **Add Validation**: More robust input validation
3. **Add Tests**: Unit and integration tests
4. **Add Logging**: Winston or Pino for logging
5. **Add Rate Limiting**: Protect API endpoints

### For Production
1. **Use PostgreSQL**: Replace LowDB with PostgreSQL
2. **Add Redis**: For caching and sessions
3. **Deploy Backend**: Heroku, Railway, or AWS
4. **Deploy Frontend**: Vercel, Netlify, or AWS S3
5. **Add Monitoring**: Sentry, DataDog, or New Relic

### For Blockchain Integration
1. **Connect to Cardano**: Use Blockfrost API
2. **Implement Wallet**: Real Cardano wallet integration
3. **Deploy Smart Contracts**: Deploy Plutus contracts to testnet
4. **Transaction Building**: Build and submit real transactions
5. **Event Listening**: Listen to blockchain events

## 🐛 Troubleshooting

### Backend not starting?
```bash
cd backend-api
rm -rf node_modules package-lock.json
npm install
npm run dev
```

### Frontend not connecting?
Check `frontend/src/services/api.ts`:
```typescript
const USE_MOCK_DATA = false; // Should be false
```

### Port already in use?
```bash
# Kill process on port 8080
npx kill-port 8080

# Kill process on port 3000
npx kill-port 3000
```

### Database issues?
```bash
# Reset database
rm backend-api/data/db.json
# Restart backend
```

## 📝 Environment Variables

### Backend (`backend-api/.env`)
```env
PORT=8080
DATABASE_PATH=./data/db.json
NODE_ENV=development
CORS_ORIGIN=http://localhost:3000
```

### Frontend (`frontend/.env`)
```env
VITE_API_URL=http://localhost:8080/api/v1
VITE_BLOCKFROST_URL=https://cardano-preprod.blockfrost.io/api/v0
VITE_BLOCKFROST_PROJECT_ID=your_project_id
VITE_NETWORK=Preprod
```

## 🎓 Learning Resources

- **Express.js**: https://expressjs.com/
- **React**: https://react.dev/
- **TypeScript**: https://www.typescriptlang.org/
- **LowDB**: https://github.com/typicode/lowdb
- **Cardano**: https://docs.cardano.org/

## 🤝 Contributing

This is a demonstration project. Feel free to:
- Fork and modify
- Add features
- Improve documentation
- Share with others

## 📄 License

MIT License - Feel free to use for learning and commercial projects!

---

**Enjoy building with TrustlessTask!** 🚀
