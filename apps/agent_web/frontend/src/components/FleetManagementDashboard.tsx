import React, { useState, useEffect } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { 
  Users, 
  Play, 
  Pause, 
  MessageSquare, 
  Plus, 
  Activity, 
  Zap,
  Settings,
  RefreshCw,
  Send
} from 'lucide-react';

interface Agent {
  id: string;
  name: string;
  type: string;
  status: string;
  autonomous_mode: boolean;
  tools: string[];
  conversation_length: number;
  created_at: string;
  metadata?: {
    created_via?: string;
    [key: string]: any;
  };
}

interface FleetMetrics {
  total_agents: number;
  active_agents: number;
  autonomous_agents: number;
  offline_agents: number;
  fleet_health: number;
}

interface FleetStatus {
  success: boolean;
  fleet_size: number;
  agents: Agent[];
  metrics: FleetMetrics;
  timestamp: number;
}

const FleetManagementDashboard: React.FC = () => {
  const [fleetStatus, setFleetStatus] = useState<FleetStatus | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [selectedAgents, setSelectedAgents] = useState<string[]>([]);
  const [broadcastMessage, setBroadcastMessage] = useState('');
  const [autonomousMessage, setAutonomousMessage] = useState('');
  const [newFleetSize, setNewFleetSize] = useState(3);
  const [fleetTemplate, setFleetTemplate] = useState('ai');

  // Fetch fleet status
  const fetchFleetStatus = async () => {
    setLoading(true);
    try {
      const response = await fetch('/api/fleet/status');
      const data = await response.json();
      if (data.success) {
        setFleetStatus(data);
        setError(null);
      } else {
        setError(data.error || 'Failed to fetch fleet status');
      }
    } catch (err) {
      setError('Failed to connect to fleet management API');
      console.error('Fleet status error:', err);
    } finally {
      setLoading(false);
    }
  };

  // Create new fleet
  const createFleet = async () => {
    setLoading(true);
    try {
      const template = {
        type: fleetTemplate,
        tools: ['shell', 'who_am_i', 'get_system_state', 'file_read'],
        system_prompt: 'You are an autonomous fleet agent capable of multi-turn function calling and independent operation.',
        autonomous_mode: false,
        max_autonomous_turns: 10
      };

      const response = await fetch('/api/fleet/agents/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          count: newFleetSize,
          template: template
        })
      });

      const data = await response.json();
      if (data.success) {
        setError(null);
        await fetchFleetStatus(); // Refresh fleet status
      } else {
        setError(data.error || 'Failed to create fleet');
      }
    } catch (err) {
      setError('Failed to create fleet');
      console.error('Fleet creation error:', err);
    } finally {
      setLoading(false);
    }
  };

  // Enable autonomous mode for selected agents
  const enableAutonomousMode = async () => {
    if (selectedAgents.length === 0) {
      setError('Please select agents first');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch('/api/fleet/autonomous/enable', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ agent_ids: selectedAgents })
      });

      const data = await response.json();
      if (data.success) {
        setError(null);
        await fetchFleetStatus(); // Refresh to see updated status
      } else {
        setError(data.error || 'Failed to enable autonomous mode');
      }
    } catch (err) {
      setError('Failed to enable autonomous mode');
      console.error('Autonomous mode error:', err);
    } finally {
      setLoading(false);
    }
  };

  // Execute autonomous operations across fleet
  const executeAutonomousOperations = async () => {
    if (selectedAgents.length === 0) {
      setError('Please select agents first');
      return;
    }
    if (!autonomousMessage.trim()) {
      setError('Please enter a message for autonomous execution');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch('/api/fleet/autonomous/execute', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: autonomousMessage,
          agent_ids: selectedAgents
        })
      });

      const data = await response.json();
      if (data.success) {
        setError(null);
        setAutonomousMessage('');
        // Optionally show results in a modal or separate component
        console.log('Autonomous execution results:', data.results);
      } else {
        setError(data.error || 'Failed to execute autonomous operations');
      }
    } catch (err) {
      setError('Failed to execute autonomous operations');
      console.error('Autonomous execution error:', err);
    } finally {
      setLoading(false);
    }
  };

  // Broadcast message to fleet
  const broadcastToFleet = async () => {
    if (!broadcastMessage.trim()) {
      setError('Please enter a message to broadcast');
      return;
    }

    setLoading(true);
    try {
      const response = await fetch('/api/fleet/broadcast', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message: broadcastMessage })
      });

      const data = await response.json();
      if (data.success) {
        setError(null);
        setBroadcastMessage('');
      } else {
        setError(data.error || 'Failed to broadcast message');
      }
    } catch (err) {
      setError('Failed to broadcast message');
      console.error('Broadcast error:', err);
    } finally {
      setLoading(false);
    }
  };

  // Toggle agent selection
  const toggleAgentSelection = (agentId: string) => {
    setSelectedAgents(prev => 
      prev.includes(agentId) 
        ? prev.filter(id => id !== agentId)
        : [...prev, agentId]
    );
  };

  // Select all agents
  const selectAllAgents = () => {
    if (!fleetStatus) return;
    const allIds = fleetStatus.agents.map(agent => agent.id);
    setSelectedAgents(allIds);
  };

  // Clear selection
  const clearSelection = () => {
    setSelectedAgents([]);
  };

  // Auto-refresh
  useEffect(() => {
    fetchFleetStatus();
    const interval = setInterval(fetchFleetStatus, 10000); // Refresh every 10 seconds
    return () => clearInterval(interval);
  }, []);

  const getStatusColor = (status: string) => {
    switch (status.toLowerCase()) {
      case 'active': return 'bg-green-500';
      case 'offline': return 'bg-red-500';
      case 'error': return 'bg-yellow-500';
      default: return 'bg-gray-500';
    }
  };

  return (
    <div className="p-6 space-y-6 max-w-7xl mx-auto">
      <div className="flex items-center justify-between">
        <h1 className="text-3xl font-bold flex items-center gap-2">
          <Users className="h-8 w-8" />
          Fleet Management Dashboard
        </h1>
        <Button onClick={fetchFleetStatus} disabled={loading} variant="outline">
          <RefreshCw className={`h-4 w-4 mr-2 ${loading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {/* Fleet Overview */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Agents</CardTitle>
            <Users className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{fleetStatus?.metrics.total_agents || 0}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Active Agents</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-600">
              {fleetStatus?.metrics.active_agents || 0}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Autonomous Agents</CardTitle>
            <Zap className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-blue-600">
              {fleetStatus?.metrics.autonomous_agents || 0}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Fleet Health</CardTitle>
            <Settings className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {fleetStatus?.metrics.fleet_health?.toFixed(1) || '0.0'}%
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Fleet Creation */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Plus className="h-5 w-5" />
            Create New Fleet
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex gap-4 items-end">
            <div className="flex-1">
              <label className="text-sm font-medium">Fleet Size</label>
              <Input
                type="number"
                min={1}
                max={20}
                value={newFleetSize}
                onChange={(e) => setNewFleetSize(parseInt(e.target.value) || 1)}
                placeholder="Number of agents to create"
              />
            </div>
            <div className="flex-1">
              <label className="text-sm font-medium">Agent Type</label>
              <Select value={fleetTemplate} onValueChange={setFleetTemplate}>
                <SelectTrigger>
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="ai">AI Agent</SelectItem>
                  <SelectItem value="researcher">Researcher Agent</SelectItem>
                  <SelectItem value="analyst">Analyst Agent</SelectItem>
                  <SelectItem value="coder">Coder Agent</SelectItem>
                </SelectContent>
              </Select>
            </div>
            <Button onClick={createFleet} disabled={loading}>
              <Plus className="h-4 w-4 mr-2" />
              Create Fleet
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Fleet Operations */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Autonomous Operations */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Zap className="h-5 w-5" />
              Autonomous Operations
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="text-sm text-muted-foreground">
              Selected: {selectedAgents.length} agent(s)
            </div>
            <Textarea
              placeholder="Enter task for autonomous execution..."
              value={autonomousMessage}
              onChange={(e) => setAutonomousMessage(e.target.value)}
              rows={3}
            />
            <div className="flex gap-2">
              <Button onClick={enableAutonomousMode} disabled={loading || selectedAgents.length === 0}>
                <Play className="h-4 w-4 mr-2" />
                Enable Autonomous Mode
              </Button>
              <Button onClick={executeAutonomousOperations} disabled={loading || selectedAgents.length === 0}>
                <Zap className="h-4 w-4 mr-2" />
                Execute Autonomously
              </Button>
            </div>
          </CardContent>
        </Card>

        {/* Fleet Broadcast */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <MessageSquare className="h-5 w-5" />
              Fleet Broadcast
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <Textarea
              placeholder="Message to broadcast to all agents..."
              value={broadcastMessage}
              onChange={(e) => setBroadcastMessage(e.target.value)}
              rows={3}
            />
            <Button onClick={broadcastToFleet} disabled={loading} className="w-full">
              <Send className="h-4 w-4 mr-2" />
              Broadcast to Fleet
            </Button>
          </CardContent>
        </Card>
      </div>

      {/* Agent List */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle>Fleet Agents</CardTitle>
            <div className="flex gap-2">
              <Button variant="outline" size="sm" onClick={selectAllAgents}>
                Select All
              </Button>
              <Button variant="outline" size="sm" onClick={clearSelection}>
                Clear Selection
              </Button>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          <div className="space-y-2">
            {fleetStatus?.agents.map((agent) => (
              <div
                key={agent.id}
                className={`p-4 border rounded-lg cursor-pointer transition-colors ${
                  selectedAgents.includes(agent.id) 
                    ? 'border-blue-500 bg-blue-50' 
                    : 'border-gray-200 hover:border-gray-300'
                }`}
                onClick={() => toggleAgentSelection(agent.id)}
              >
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    <div className={`w-3 h-3 rounded-full ${getStatusColor(agent.status || 'unknown')}`} />
                    <div>
                      <div className="font-medium">{agent.name}</div>
                      <div className="text-sm text-muted-foreground">ID: {agent.id}</div>
                    </div>
                  </div>
                  <div className="flex items-center gap-2">
                    <Badge variant={agent.autonomous_mode ? "default" : "secondary"}>
                      {agent.autonomous_mode ? "Autonomous" : "Manual"}
                    </Badge>
                    <Badge variant="outline">{agent.type}</Badge>
                    {agent.tools && (
                      <Badge variant="outline">{agent.tools.length} tools</Badge>
                    )}
                  </div>
                </div>
              </div>
            ))}
            {(!fleetStatus?.agents || fleetStatus.agents.length === 0) && (
              <div className="text-center py-8 text-muted-foreground">
                No agents in fleet. Create some agents to get started.
              </div>
            )}
          </div>
        </CardContent>
      </Card>
    </div>
  );
};

export default FleetManagementDashboard;