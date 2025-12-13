import React, { useState } from 'react';
import type { CreateProjectRequest, MilestoneRequest } from '../types';

interface CreateProjectFormProps {
  onSubmit: (data: CreateProjectRequest) => void;
  clientAddress: string;
}

export const CreateProjectForm: React.FC<CreateProjectFormProps> = ({ onSubmit, clientAddress }) => {
  const [formData, setFormData] = useState({
    title: '',
    description: '',
    freelancerAddress: '',
    arbiterAddress: '',
  });

  const [milestones, setMilestones] = useState<MilestoneRequest[]>([
    { description: '', amount: 0, deadline: '' }
  ]);

  const addMilestone = () => {
    setMilestones([...milestones, { description: '', amount: 0, deadline: '' }]);
  };

  const updateMilestone = (index: number, field: keyof MilestoneRequest, value: any) => {
    const updated = [...milestones];
    updated[index] = { ...updated[index], [field]: value };
    setMilestones(updated);
  };

  const removeMilestone = (index: number) => {
    setMilestones(milestones.filter((_, i) => i !== index));
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    
    // Validate client address
    if (!clientAddress || clientAddress.trim().length === 0) {
      alert('Client address is missing. Please reconnect your wallet.');
      return;
    }
    
    // Validate basic fields
    if (!formData.title.trim()) {
      alert('Please enter a project title');
      return;
    }

    if (!formData.description.trim()) {
      alert('Please enter a project description');
      return;
    }

    if (!formData.freelancerAddress.trim()) {
      alert('Please enter a freelancer address');
      return;
    }

    // Validate milestones
    if (milestones.length === 0) {
      alert('Please add at least one milestone');
      return;
    }

    if (milestones.some(m => !m.description.trim())) {
      alert('Please fill in all milestone descriptions');
      return;
    }

    if (milestones.some(m => !m.amount || m.amount <= 0)) {
      alert('Please enter valid amounts for all milestones (minimum 1,000,000 lovelace)');
      return;
    }

    if (milestones.some(m => !m.deadline)) {
      alert('Please set deadlines for all milestones');
      return;
    }

    const totalAmount = milestones.reduce((sum, m) => sum + m.amount, 0);
    
    const projectData = {
      title: formData.title.trim(),
      description: formData.description.trim(),
      clientAddress,
      freelancerAddress: formData.freelancerAddress.trim(),
      totalAmount,
      milestones: milestones.map(m => ({
        description: m.description.trim(),
        amount: m.amount,
        deadline: m.deadline,
      })),
      arbiterAddress: formData.arbiterAddress.trim() || undefined,
    };

    console.log('📋 Submitting project data:', JSON.stringify(projectData, null, 2));
    
    onSubmit(projectData);
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-6">
      <div>
        <label className="block text-sm font-medium mb-2">Project Title</label>
        <input
          type="text"
          required
          className="w-full px-4 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500"
          value={formData.title}
          onChange={(e) => setFormData({ ...formData, title: e.target.value })}
        />
      </div>

      <div>
        <label className="block text-sm font-medium mb-2">Description</label>
        <textarea
          required
          rows={4}
          className="w-full px-4 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500"
          value={formData.description}
          onChange={(e) => setFormData({ ...formData, description: e.target.value })}
        />
      </div>

      <div>
        <label className="block text-sm font-medium mb-2">Freelancer Address</label>
        <input
          type="text"
          required
          className="w-full px-4 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500"
          value={formData.freelancerAddress}
          onChange={(e) => setFormData({ ...formData, freelancerAddress: e.target.value })}
        />
      </div>

      <div>
        <label className="block text-sm font-medium mb-2">Arbiter Address (Optional)</label>
        <input
          type="text"
          className="w-full px-4 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500"
          value={formData.arbiterAddress}
          onChange={(e) => setFormData({ ...formData, arbiterAddress: e.target.value })}
        />
      </div>

      <div>
        <div className="flex justify-between items-center mb-4">
          <h3 className="text-lg font-semibold">Milestones</h3>
          <button
            type="button"
            onClick={addMilestone}
            className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
          >
            Add Milestone
          </button>
        </div>

        {milestones.map((milestone, index) => (
          <div key={index} className="border rounded-lg p-4 mb-4">
            <div className="flex justify-between items-center mb-3">
              <h4 className="font-medium">Milestone {index + 1}</h4>
              {milestones.length > 1 && (
                <button
                  type="button"
                  onClick={() => removeMilestone(index)}
                  className="text-red-600 hover:text-red-800"
                >
                  Remove
                </button>
              )}
            </div>

            <div className="space-y-3">
              <input
                type="text"
                placeholder="Description"
                required
                className="w-full px-3 py-2 border rounded"
                value={milestone.description}
                onChange={(e) => updateMilestone(index, 'description', e.target.value)}
              />
              <input
                type="number"
                placeholder="Amount (lovelace)"
                required
                min="1000000"
                className="w-full px-3 py-2 border rounded"
                value={milestone.amount || ''}
                onChange={(e) => {
                  const value = parseInt(e.target.value);
                  updateMilestone(index, 'amount', isNaN(value) ? 0 : value);
                }}
              />
              <input
                type="datetime-local"
                required
                className="w-full px-3 py-2 border rounded"
                value={milestone.deadline}
                onChange={(e) => updateMilestone(index, 'deadline', e.target.value)}
              />
            </div>
          </div>
        ))}
      </div>

      <button
        type="submit"
        className="w-full py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 font-medium"
      >
        Create Project
      </button>
    </form>
  );
};
