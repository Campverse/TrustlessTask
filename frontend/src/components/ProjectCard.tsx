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
      className="bg-white border rounded-lg p-4 sm:p-6 hover:shadow-lg transition-all cursor-pointer active:scale-98"
      onClick={() => onSelect(project.id)}
    >
      <div className="flex flex-col sm:flex-row sm:justify-between sm:items-start gap-3 mb-4">
        <h3 className="text-lg sm:text-xl font-semibold line-clamp-2 flex-1">{project.title}</h3>
        <span className={`px-3 py-1 rounded-full text-xs sm:text-sm font-medium ${statusColors[project.status]} whitespace-nowrap self-start`}>
          {project.status}
        </span>
      </div>

      <p className="text-gray-600 text-sm sm:text-base mb-4 line-clamp-2">{project.description}</p>

      <div className="grid grid-cols-2 sm:flex sm:flex-col gap-2 text-sm">
        <div className="flex flex-col sm:flex-row sm:justify-between">
          <span className="text-gray-500 text-xs sm:text-sm">Total Amount:</span>
          <span className="font-semibold text-blue-600">{(project.totalAmount / 1_000_000).toFixed(2)} ₳</span>
        </div>
        <div className="flex flex-col sm:flex-row sm:justify-between">
          <span className="text-gray-500 text-xs sm:text-sm">Milestones:</span>
          <span className="font-medium">{completedMilestones}/{totalMilestones}</span>
        </div>
        <div className="col-span-2 flex flex-col sm:flex-row sm:justify-between">
          <span className="text-gray-500 text-xs sm:text-sm">Created:</span>
          <span className="font-medium text-xs sm:text-sm">
            {formatDistance(new Date(project.createdAt), new Date(), { addSuffix: true })}
          </span>
        </div>
      </div>

      <div className="mt-4 pt-4 border-t">
        <div className="flex items-center justify-between mb-2">
          <span className="text-xs text-gray-500">Progress</span>
          <span className="text-xs font-medium text-gray-700">{Math.round((completedMilestones / totalMilestones) * 100)}%</span>
        </div>
        <div className="w-full bg-gray-200 rounded-full h-2">
          <div 
            className="bg-blue-600 h-2 rounded-full transition-all duration-300"
            style={{ width: `${(completedMilestones / totalMilestones) * 100}%` }}
          />
        </div>
      </div>
    </div>
  );
};
