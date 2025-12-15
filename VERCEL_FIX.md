# 🔧 Fix Vercel Deployment

## Issue
The deployed frontend has stopped working because Vercel is trying to build from the wrong directory.

## Solution

### Step 1: Configure Root Directory in Vercel Dashboard

1. Go to your Vercel project: https://vercel.com/dashboard
2. Click on your TrustlessTask project
3. Go to **Settings** → **General**
4. Scroll to **Root Directory**
5. Click **Edit**
6. Enter: `frontend`
7. Click **Save**

### Step 2: Verify Build Settings

While in Settings, verify these are correct:

**Build & Development Settings:**
- **Framework Preset**: Vite
- **Build Command**: `npm run build` (or leave empty for auto-detect)
- **Output Directory**: `dist` (or leave empty for auto-detect)
- **Install Command**: `npm install` (or leave empty for auto-detect)

### Step 3: Redeploy

1. Go to **Deployments** tab
2. Click the three dots (...) on the latest deployment
3. Click **Redeploy**
4. Wait for build to complete

## Alternative: Deploy via CLI

If you prefer using the CLI:

```bash
cd frontend
vercel --prod
```

This will deploy only the frontend directory.

## What Changed

The root `vercel.json` was trying to build both frontend and backend together, which caused conflicts. By setting the root directory to `frontend`, Vercel will:

1. ✅ Only build the frontend
2. ✅ Use the correct package.json
3. ✅ Output to the correct dist folder
4. ✅ Serve the static files properly

## Backend Deployment

The backend should be deployed separately. Options:

### Option 1: Railway.app (Recommended)
```
1. Visit https://railway.app
2. New Project → Deploy from GitHub
3. Select TrustlessTask repository
4. Root Directory: backend-api
5. Add environment variables
6. Deploy
```

### Option 2: Render.com
```
1. Visit https://render.com
2. New Web Service
3. Root Directory: backend-api
4. Build: npm install
5. Start: npm start
6. Deploy
```

### Option 3: Separate Vercel Project
```
1. Create new Vercel project
2. Same repository
3. Root Directory: backend-api
4. Deploy
```

## After Backend is Deployed

Update frontend environment variables in Vercel:

1. Go to **Settings** → **Environment Variables**
2. Add/Update:
   ```
   VITE_API_URL=https://your-backend-url.com/api/v1
   VITE_BLOCKFROST_PROJECT_ID=preprodYourKey
   VITE_NETWORK=preprod
   ```
3. Redeploy frontend

## Verification

After redeployment, your frontend should:
- ✅ Load at your Vercel URL
- ✅ Show the homepage
- ✅ Allow wallet connection
- ✅ Display projects (if backend is connected)

## Troubleshooting

### Build Still Fails
- Clear build cache in Vercel settings
- Check that Root Directory is set to `frontend`
- Verify Node version is 18.x or higher

### Frontend Loads but API Fails
- Backend not deployed yet (expected)
- Check VITE_API_URL environment variable
- Verify backend is running and accessible

### Wallet Connection Issues
- This is expected in production without Lucid
- Wallet connection works in development mode
- For production, consider using wallet connector libraries

---

**Quick Fix**: Set Root Directory to `frontend` in Vercel project settings, then redeploy.
