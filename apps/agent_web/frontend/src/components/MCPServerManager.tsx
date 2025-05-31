import React, { useState, useEffect } from 'react';
import { Shield, Server, Key, Globe, RefreshCw, Plus, Trash2, Edit2, Check, X } from 'lucide-react';

interface MCPServer {
  id: string;
  name: string;
  category: string;
  url: string;
  auth_type: 'oauth2' | 'api_key' | 'open';
  maintainer: string;
  description: string;
  capabilities: any[];
  status: 'active' | 'inactive';
  last_checked: string;
}

interface MCPServerManagerProps {
  selectedServers?: string[];
  onServersChange?: (serverIds: string[]) => void;
  mode?: 'manage' | 'select';
}

export const MCPServerManager: React.FC<MCPServerManagerProps> = ({
  selectedServers = [],
  onServersChange,
  mode = 'manage'
}) => {
  const [servers, setServers] = useState<MCPServer[]>([]);
  const [loading, setLoading] = useState(true);
  const [editingId, setEditingId] = useState<string | null>(null);
  const [showAddForm, setShowAddForm] = useState(false);
  const [newServer, setNewServer] = useState<Partial<MCPServer>>({
    auth_type: 'oauth2',
    category: 'Software Development',
    status: 'active'
  });

  useEffect(() => {
    fetchServers();
  }, []);

  const fetchServers = async () => {
    try {
      const response = await fetch('/api/mcp-servers');
      const data = await response.json();
      setServers(data);
    } catch (error) {
      console.error('Failed to fetch MCP servers:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleRefreshCapabilities = async (serverId: string) => {
    try {
      await fetch(`/api/mcp-servers/${serverId}/discover`, { method: 'POST' });
      fetchServers();
    } catch (error) {
      console.error('Failed to refresh capabilities:', error);
    }
  };

  const handleAddServer = async () => {
    try {
      const response = await fetch('/api/mcp-servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(newServer)
      });
      if (response.ok) {
        fetchServers();
        setShowAddForm(false);
        setNewServer({ auth_type: 'oauth2', category: 'Software Development', status: 'active' });
      }
    } catch (error) {
      console.error('Failed to add server:', error);
    }
  };

  const handleDeleteServer = async (serverId: string) => {
    if (confirm('Are you sure you want to delete this server?')) {
      try {
        await fetch(`/api/mcp-servers/${serverId}`, { method: 'DELETE' });
        fetchServers();
      } catch (error) {
        console.error('Failed to delete server:', error);
      }
    }
  };

  const handleToggleServer = (serverId: string) => {
    if (mode === 'select' && onServersChange) {
      const newSelection = selectedServers.includes(serverId)
        ? selectedServers.filter(id => id !== serverId)
        : [...selectedServers, serverId];
      onServersChange(newSelection);
    }
  };

  const getAuthIcon = (authType: string) => {
    switch (authType) {
      case 'oauth2': return <Shield className="w-4 h-4" />;
      case 'api_key': return <Key className="w-4 h-4" />;
      case 'open': return <Globe className="w-4 h-4" />;
      default: return <Server className="w-4 h-4" />;
    }
  };

  const getAuthColor = (authType: string) => {
    switch (authType) {
      case 'oauth2': return 'text-green-600 bg-green-100';
      case 'api_key': return 'text-yellow-600 bg-yellow-100';
      case 'open': return 'text-blue-600 bg-blue-100';
      default: return 'text-gray-600 bg-gray-100';
    }
  };

  const categories = [...new Set(servers.map(s => s.category))].sort();

  if (loading) {
    return <div className="flex justify-center items-center h-64">Loading MCP servers...</div>;
  }

  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold">MCP Server {mode === 'manage' ? 'Management' : 'Selection'}</h2>
        {mode === 'manage' && (
          <button
            onClick={() => setShowAddForm(true)}
            className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
          >
            <Plus className="w-4 h-4" />
            Add Server
          </button>
        )}
      </div>

      {showAddForm && (
        <div className="bg-gray-50 p-4 rounded-lg border">
          <h3 className="font-semibold mb-4">Add New MCP Server</h3>
          <div className="grid grid-cols-2 gap-4">
            <input
              type="text"
              placeholder="Server Name"
              value={newServer.name || ''}
              onChange={(e) => setNewServer({ ...newServer, name: e.target.value })}
              className="px-3 py-2 border rounded"
            />
            <input
              type="text"
              placeholder="Server ID"
              value={newServer.id || ''}
              onChange={(e) => setNewServer({ ...newServer, id: e.target.value })}
              className="px-3 py-2 border rounded"
            />
            <input
              type="text"
              placeholder="URL"
              value={newServer.url || ''}
              onChange={(e) => setNewServer({ ...newServer, url: e.target.value })}
              className="px-3 py-2 border rounded"
            />
            <select
              value={newServer.auth_type || 'oauth2'}
              onChange={(e) => setNewServer({ ...newServer, auth_type: e.target.value as any })}
              className="px-3 py-2 border rounded"
            >
              <option value="oauth2">OAuth 2.0</option>
              <option value="api_key">API Key</option>
              <option value="open">Open</option>
            </select>
            <input
              type="text"
              placeholder="Category"
              value={newServer.category || ''}
              onChange={(e) => setNewServer({ ...newServer, category: e.target.value })}
              className="px-3 py-2 border rounded"
            />
            <input
              type="text"
              placeholder="Maintainer"
              value={newServer.maintainer || ''}
              onChange={(e) => setNewServer({ ...newServer, maintainer: e.target.value })}
              className="px-3 py-2 border rounded"
            />
            <textarea
              placeholder="Description"
              value={newServer.description || ''}
              onChange={(e) => setNewServer({ ...newServer, description: e.target.value })}
              className="col-span-2 px-3 py-2 border rounded"
              rows={3}
            />
          </div>
          <div className="flex gap-2 mt-4">
            <button
              onClick={handleAddServer}
              className="px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700"
            >
              <Check className="w-4 h-4" />
            </button>
            <button
              onClick={() => {
                setShowAddForm(false);
                setNewServer({ auth_type: 'oauth2', category: 'Software Development', status: 'active' });
              }}
              className="px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
            >
              <X className="w-4 h-4" />
            </button>
          </div>
        </div>
      )}

      {categories.map(category => (
        <div key={category} className="space-y-2">
          <h3 className="text-lg font-semibold text-gray-700">{category}</h3>
          <div className="grid gap-4">
            {servers
              .filter(server => server.category === category)
              .map(server => (
                <div
                  key={server.id}
                  className={`border rounded-lg p-4 ${
                    mode === 'select' && selectedServers.includes(server.id)
                      ? 'border-blue-500 bg-blue-50'
                      : 'border-gray-200'
                  }`}
                >
                  <div className="flex justify-between items-start">
                    <div className="flex-1">
                      <div className="flex items-center gap-3">
                        {mode === 'select' && (
                          <input
                            type="checkbox"
                            checked={selectedServers.includes(server.id)}
                            onChange={() => handleToggleServer(server.id)}
                            className="w-4 h-4"
                          />
                        )}
                        <h4 className="font-semibold">{server.name}</h4>
                        <span className={`flex items-center gap-1 px-2 py-1 rounded-full text-xs ${getAuthColor(server.auth_type)}`}>
                          {getAuthIcon(server.auth_type)}
                          {server.auth_type.toUpperCase()}
                        </span>
                        <span className={`px-2 py-1 rounded-full text-xs ${
                          server.status === 'active' ? 'bg-green-100 text-green-700' : 'bg-gray-100 text-gray-700'
                        }`}>
                          {server.status}
                        </span>
                      </div>
                      <p className="text-sm text-gray-600 mt-1">{server.description}</p>
                      <div className="flex items-center gap-4 mt-2 text-xs text-gray-500">
                        <span>URL: {server.url}</span>
                        <span>Maintainer: {server.maintainer}</span>
                        {server.capabilities.length > 0 && (
                          <span>{server.capabilities.length} capabilities</span>
                        )}
                      </div>
                    </div>
                    {mode === 'manage' && (
                      <div className="flex gap-2">
                        <button
                          onClick={() => handleRefreshCapabilities(server.id)}
                          className="p-1 text-gray-600 hover:text-blue-600"
                          title="Refresh capabilities"
                        >
                          <RefreshCw className="w-4 h-4" />
                        </button>
                        <button
                          onClick={() => setEditingId(server.id)}
                          className="p-1 text-gray-600 hover:text-green-600"
                          title="Edit"
                        >
                          <Edit2 className="w-4 h-4" />
                        </button>
                        <button
                          onClick={() => handleDeleteServer(server.id)}
                          className="p-1 text-gray-600 hover:text-red-600"
                          title="Delete"
                        >
                          <Trash2 className="w-4 h-4" />
                        </button>
                      </div>
                    )}
                  </div>
                </div>
              ))}
          </div>
        </div>
      ))}
    </div>
  );
};