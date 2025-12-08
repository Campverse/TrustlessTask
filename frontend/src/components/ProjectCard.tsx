import React from 'react';
import { formatDistance } from 'date-fns';
import type { Project } from '../types';

interface ProjectCardProps {
  project: Project;
  onSelect: (id: string) => void;
}

export const ProjectCard: React.FC<ProjectCardProps> = ({ project, onSelect }) => {
  const completedMilestones = project.milestones.filter(m => m.completed).length;
  const totalMilestones = project.milestones.length;

  const statusColors = {
    Created: 'bg-blue-100 text-blue-800',
    InProgress: 'bg-yellow-100 text-yellow-800',
    UnderReview: 'bg-purple-100 text-purple-800',
    Disputed: 'bg-red-100 text-red-800',
    Completed: 'bg-green-100 text-green-800',
    Cancelled: 'bg-gray-100 text-gray-800',
  };

  return (
    <div 
      className="border rounded-lg p-6 hover:shadow-lg transition-shadow cursor-pointer"
      onClick={() => onSelect(project.id)}
    >
      <div className="flex justify-between items-start mb-4">
        <h3 className="text-xl font-semibold">{project.title}</h3>
        <span className={`px-3 py-1 rounded-full text-sm ${statusColors[project.status]}`}>
          {project.status}
        </span>
      </div>

      <p className="text-gray-600 mb-4 line-clamp-2">{project.description}</p>

      <div className="space-y-2 text-sm">
        <div className="flex justify-between">
          <span className="text-gray-500">Total Amount:</span>
          <span className="font-medium">{(project.totalAmount / 1_000_000).toFixed(2)} ₳</span>
        </div>
        <div className="flex justify-between">
          <span className="text-gray-500">Milestones:</span>
          <span className="font-medium">{completedMilestones}/{totalMilestones}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-gray-500">Created:</span>
          <span className="font-medium">
            {formatDistance(new Date(project.createdAt), new Date(), { addSuffix: true })}
          </span>
        </div>
      </div>

      <div className="mt-4 pt-4 border-t">
        <div className="w-full bg-gray-200 rounded-full h-2">
          <div 
            className="bg-blue-600 h-2 rounded-full transition-all"
            style={{ width: `${(completedMilestones / totalMilestones) * 100}%` }}
          />
        </div>
      </div>
    </div>
  );
};
