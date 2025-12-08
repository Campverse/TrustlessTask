import { Low } from 'lowdb';
import { JSONFile } from 'lowdb/node';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { mkdirSync, existsSync } from 'fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

interface Project {
  id: string;
  title: string;
  description: string;
  clientAddress: string;
  freelancerAddress: string;
  totalAmount: number;
  status: string;
  createdAt: string;
  txHash?: string;
  arbiterAddress?: string;
  milestones: Milestone[];
}

interface Milestone {
  id: number;
  description: string;
  amount: number;
  deadline: string;
  completed: boolean;
  approved: boolean;
}

interface User {
  address: string;
  completedProjects: number;
  totalEarned: number;
  averageRating: number;
  disputes: number;
}

interface Dispute {
  id: string;
  projectId: string;
  reason: string;
  raiserAddress: string;
  arbiterAddress?: string;
  resolved: boolean;
  outcome?: boolean;
  createdAt: string;
}

interface Database {
  projects: Project[];
  users: User[];
  disputes: Dispute[];
}

const dbPath = process.env.DATABASE_PATH || join(__dirname, '../../data/db.json');
const dbDir = dirname(dbPath);

if (!existsSync(dbDir)) {
  mkdirSync(dbDir, { recursive: true });
}

const adapter = new JSONFile<Database>(dbPath);
export const db = new Low<Database>(adapter, { projects: [], users: [], disputes: [] });

export async function initDatabase() {
  await db.read();
  db.data ||= { projects: [], users: [], disputes: [] };
  await db.write();
  console.log('✅ Database initialized');
}
