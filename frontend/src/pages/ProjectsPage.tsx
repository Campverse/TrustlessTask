import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { useNavigate } from 'react-router-dom';
import { projectsApi } from '../services/api';
import { ProjectCard } from '../components/ProjectCard';

export const ProjectsPage: React.FC = () => {
  const navigate = useNavigate();
  
  const { data: projects, isLoading, isError, error } = useQuery({
    queryKey: ['projects'],
    queryFn: projectsApi.list,
    retry: 2,
    staleTime: 30000, // 30 seconds
  });

  if (isLoading) {
    return (
      <div className="text-center py-12 px-4">
        <div className="inline-block animate-spin rounded-full h-10 w-10 sm:h-12 sm:w-12 border-b-2 border-blue-600"></div>
        <p className="mt-4 text-sm sm:text-base text-gray-600">Loading projects...</p>
      </div>
    );
  }

  if (isError) {
    return (
      <div className="text-center py-12 px-4">
        <div className="bg-red-100 text-red-700 p-4 sm:p-6 rounded-lg max-w-md mx-auto">
          <h2 className="text-lg sm:text-xl font-semibold mb-2">Failed to load projects</h2>
          <p className="text-xs sm:text-sm">{error instanceof Error ? error.message : 'Unknown error'}</p>
          <button
            onClick={() => window.location.reload()}
            className="mt-4 px-4 py-2 text-sm sm:text-base bg-red-600 text-white rounded hover:bg-red-700"
          >
            Retry
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="px-4 sm:px-6">
      <div className="flex flex-col sm:flex-row sm:justify-between sm:items-center gap-4 mb-6 sm:mb-8">
        <h1 className="text-2xl sm:text-3xl font-bold">All Projects</h1>
        <button
          onClick={() => navigate('/create')}
          className="w-full sm:w-auto px-6 py-3 text-sm sm:text-base bg-blue-600 text-white rounded-lg hover:bg-blue-700"
        >
          Create New Project
        </button>
      </div>

      {projects && projects.length > 0 ? (
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4 sm:gap-6">
          {projects.map((project) => (
            <ProjectCard
              key={project.id}
              project={project}
              onSelect={(id) => navigate(`/project/${id}`)}
            />
          ))}
        </div>
      ) : (
        <div className="text-center py-12 text-sm sm:text-base text-gray-500">
          No projects found. Create your first project!
        </div>
      )}
    </div>
  );
};
