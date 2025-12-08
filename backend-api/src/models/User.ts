import { db } from '../db/database.js';

export interface UserProfile {
  address: string;
  completedProjects: number;
  totalEarned: number;
  averageRating: number;
  disputes: number;
}

export class UserModel {
  static async getProfile(address: string): Promise<UserProfile> {
    await db.read();
    let user = db.data!.users.find(u => u.address === address);

    if (!user) {
      user = {
        address,
        completedProjects: 0,
        totalEarned: 0,
        averageRating: 0.0,
        disputes: 0,
      };
      db.data!.users.push(user);
      await db.write();
    }

    return user;
  }

  static async updateStats(address: string, completedProjects: number, earned: number) {
    await db.read();
    const user = db.data!.users.find(u => u.address === address);
    if (user) {
      user.completedProjects += completedProjects;
      user.totalEarned += earned;
      await db.write();
    }
  }

  static async incrementDisputes(address: string) {
    await db.read();
    const user = db.data!.users.find(u => u.address === address);
    if (user) {
      user.disputes += 1;
      await db.write();
    }
  }
}
