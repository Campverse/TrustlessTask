import express from 'express';
import cors from 'cors';
import dotenv from 'dotenv';
import { initDatabase } from './db/database.js';
import projectsRouter from './routes/projects.js';
import usersRouter from './routes/users.js';
import disputesRouter from './routes/disputes.js';

dotenv.config();

const app = express();
const PORT = process.env.PORT || 8080;

// Initialize database
await initDatabase();

// Middleware
app.use(cors({
  origin: process.env.CORS_ORIGIN || 'http://localhost:3000',
  credentials: true,
}));
app.use(express.json());

// Routes
app.use('/api/v1/projects', projectsRouter);
app.use('/api/v1/users', usersRouter);
app.use('/api/v1/disputes', disputesRouter);

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Error handling
app.use((err: any, req: express.Request, res: express.Response, next: express.NextFunction) => {
  console.error(err.stack);
  res.status(500).json({ error: 'Internal server error' });
});

app.listen(PORT, () => {
  console.log(`🚀 TrustlessTask API running on http://localhost:${PORT}`);
  console.log(`📊 Environment: ${process.env.NODE_ENV || 'development'}`);
});
