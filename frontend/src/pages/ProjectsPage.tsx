import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { useNavigate } from 'react-router-dom';
import { projectsApi } from '../services/api';
import { ProjectCard } from '../components/ProjectCard';

export const ProjectsPage: React.FC = () => {
  const navigate = useNavigate();
  
  const { data: projects, isLoading } = useQuery({
    queryKey: ['projects'],
    queryFn: projectsApi.list,
  });

  if (isLoading) {
    return <div className="text-center py-12">Loading projects...</div>;
  }

  return (
    <div>
      <div className="flex justify-between items-center mb-8">
        <h1 className="text-3xl font-bold">All Projects</h1>
        <button
          onClick={() => navigate('/create')}
          className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
        >
          Create New Project
        </button>
      </div>

      {projects && projects.length > 0 ? (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {projects.map((project) => (
            <ProjectCard
              key={project.id}
              project={project}
              onSelect={(id) => navigate(`/project/${id}`)}
            />
          ))}
        </div>
      ) : (
        <div className="text-center py-12 text-gray-500">
          No projects found. Create your first project!
        </div>
      )}
    </div>
  );
};
