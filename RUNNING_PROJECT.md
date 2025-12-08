# 🚀 TrustlessTask - Project Running Guide

## ✅ Current Status

Your project is **LIVE and RUNNING**!

### Running Services

1. **Backend API** 
   - URL: http://localhost:8080
   - Status: ✅ Running
   - Database: JSON-based (LowDB)
   - Health Check: http://localhost:8080/health

2. **Frontend**
   - URL: http://localhost:3000
   - Status: ✅ Running
   - Framework: React + Vite
   - Connected to: Real Backend API

## 🎯 How to Use

### 1. Open the Application

Open your browser and go to:
```
http://localhost:3000
```

### 2. Connect Wallet (Demo Mode)

Click "Connect Wallet" button in the top right. This will simulate a wallet connection with:
- Demo Address: `addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp`
- Demo Balance: 1000 ADA

### 3. Create a Project

1. Click "Create Project" in the navigation
2. Fill in the form:
   - **Title**: e.g., "Website Development"
   - **Description**: Project details
   - **Freelancer Address**: Any Cardano address
   - **Milestones**: Add one or more milestones with:
     - Description
     - Amount (in lovelace, e.g., 1000000000 = 1000 ADA)
     - Deadline

3. Click "Create Project"
4. Project will be saved to the database and displayed

### 4. View Projects

- Click "Projects" to see all projects
- Click on any project card to view details
- See milestone progress and status

### 5. Manage Milestones

As a **Freelancer**:
- Click "Mark Complete" on milestones you've finished

As a **Client**:
- Click "Approve & Release Funds" to approve completed milestones

### 6. View Profile

- Click "Profile" to see your stats:
  - Completed projects
  - Total earned
  - Average rating
  - Disputes

## 🔧 API Endpoints

All endpoints are available at `http://localhost:8080/api/v1`

### Projects
- `POST /projects` - Create new project
- `GET /projects` - List all projects
- `GET /projects/:id` - Get project details
- `POST /projects/:id/milestone/:mid/complete` - Complete milestone
- `POST /projects/:id/milestone/:mid/approve` - Approve milestone
- `POST /projects/:id/cancel` - Cancel project

### Users
- `GET /users/:address/profile` - Get user profile

### Disputes
- `POST /disputes` - Create dispute
- `GET /disputes/:id` - Get dispute details
- `POST /disputes/:id/resolve` - Resolve dispute

## 📊 Database

Data is stored in: `backend-api/data/db.json`

You can view and edit this file directly to see all stored data:
- Projects
- Users
- Disputes

## 🛠️ Development Commands

### Stop Services

To stop the running services:

**Backend:**
```bash
# Find the process and stop it
# Or use Ctrl+C in the terminal
```

**Frontend:**
```bash
# Find the process and stop it
# Or use Ctrl+C in the terminal
```

### Restart Services

**Backend:**
```bash
cd backend-api
npm run dev
```

**Frontend:**
```bash
cd frontend
npm run dev
```

### Clear Database

To reset all data:
```bash
# Delete the database file
rm backend-api/data/db.json

# Restart the backend - it will create a fresh database
```

## 🧪 Testing the API

### Using curl:

```bash
# Health check
curl http://localhost:8080/health

# Get all projects
curl http://localhost:8080/api/v1/projects

# Get user profile
curl http://localhost:8080/api/v1/users/addr_test1.../profile
```

### Using Browser:

Visit these URLs directly:
- http://localhost:8080/health
- http://localhost:8080/api/v1/projects

## 🎨 Features Working

✅ **Project Creation** - Create projects with multiple milestones
✅ **Project Listing** - View all projects with status
✅ **Project Details** - See full project information
✅ **Milestone Management** - Complete and approve milestones
✅ **User Profiles** - Track reputation and stats
✅ **Real-time Updates** - Changes reflect immediately
✅ **Persistent Storage** - Data saved to JSON database
✅ **RESTful API** - Full backend API working
✅ **Responsive UI** - Works on all screen sizes

## 🔐 Demo vs Production

### Current Setup (Demo Mode)
- ✅ Wallet connection simulated
- ✅ Real backend API with database
- ✅ All CRUD operations working
- ✅ Data persistence
- ❌ No blockchain transactions
- ❌ No real Cardano wallet integration

### For Production
To make this production-ready with real blockchain:
1. Deploy Plutus smart contracts to Cardano
2. Integrate real wallet (Nami, Eternl, Flint)
3. Connect to Cardano node
4. Use Blockfrost or similar API
5. Replace simulated transactions with real ones

## 📝 Next Steps

1. **Test the Application**
   - Create multiple projects
   - Complete milestones
   - Test all features

2. **Customize**
   - Modify UI styling
   - Add more features
   - Enhance validation

3. **Deploy**
   - Deploy backend to a server
   - Deploy frontend to Vercel/Netlify
   - Set up production database

## 🐛 Troubleshooting

### Backend not responding
```bash
# Check if backend is running
curl http://localhost:8080/health

# Restart backend
cd backend-api
npm run dev
```

### Frontend not loading
```bash
# Check if frontend is running
# Should see Vite dev server at http://localhost:3000

# Restart frontend
cd frontend
npm run dev
```

### CORS errors
- Backend is configured to allow requests from http://localhost:3000
- If you change ports, update `backend-api/.env`

## 🎉 Success!

Your TrustlessTask decentralized freelance marketplace is now running with:
- ✅ Full-stack application
- ✅ Real backend API
- ✅ Persistent database
- ✅ Complete UI
- ✅ All features working

**Open http://localhost:3000 and start using it!**
