import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from './ui/card';

interface SystemMetrics {
  node: string;
  uptime: number;
  total_memory: number;
  used_memory: number;
  process_count: number;
  run_queue: number;
  schedulers: number;
  cpuUsage: number;
  memoryUsage: number;
}

interface AgentData {
  id: string;
  pid: string;
  status: string;
  memory: number;
  message_queue: number;
}

interface DashboardData {
  system: SystemMetrics;
  agents: AgentData[];
  timestamp: number;
}

const CleanSystemDashboard: React.FC = () => {
  const [data, setData] = useState<DashboardData | null>(null);
  const [connected, setConnected] = useState(false);
  const [ws, setWs] = useState<WebSocket | null>(null);

  useEffect(() => {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const websocket = new WebSocket(`${protocol}//${window.location.host}/ws`);
    
    websocket.onopen = () => {
      setConnected(true);
      // Subscribe to monitoring updates
      websocket.send(JSON.stringify({ type: 'subscribe_monitoring' }));
    };
    
    websocket.onmessage = (event) => {
      try {
        const message = JSON.parse(event.data);
        if (message.type === 'monitoring_update') {
          setData(message.data);
        }
      } catch (error) {
        console.error('Dashboard parse error:', error);
      }
    };
    
    websocket.onclose = () => {
      setConnected(false);
      // Attempt to reconnect after 3 seconds
      setTimeout(() => {
        window.location.reload();
      }, 3000);
    };
    
    websocket.onerror = () => {
      setConnected(false);
    };
    
    setWs(websocket);
    
    return () => {
      websocket.close();
    };
  }, []);

  const formatMemory = (bytes: number): string => {
    const mb = bytes / (1024 * 1024);
    return `${mb.toFixed(1)} MB`;
  };

  const formatUptime = (ms: number): string => {
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(seconds / 60);
    const hours = Math.floor(minutes / 60);
    
    if (hours > 0) return `${hours}h ${minutes % 60}m`;
    if (minutes > 0) return `${minutes}m ${seconds % 60}s`;
    return `${seconds}s`;
  };

  if (!connected) {
    return (
      <div className="p-6 bg-red-50 border border-red-200 rounded-lg">
        <div className="text-red-800 font-semibold">🔴 Disconnected</div>
        <div className="text-red-600 text-sm">Attempting to reconnect...</div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="p-6 bg-blue-50 border border-blue-200 rounded-lg">
        <div className="text-blue-800 font-semibold">🔄 Loading Dashboard</div>
        <div className="text-blue-600 text-sm">Waiting for system data...</div>
      </div>
    );
  }

  return (
    <div className="p-6 space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <h1 className="text-2xl font-bold text-gray-900">System Dashboard</h1>
        <div className="flex items-center space-x-2">
          <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse"></div>
          <span className="text-sm text-gray-600">Live</span>
        </div>
      </div>

      {/* System Overview */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm text-gray-600">Node</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-lg font-semibold">{data.system.node}</div>
            <div className="text-xs text-gray-500">Uptime: {formatUptime(data.system.uptime)}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm text-gray-600">CPU Usage</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{data.system.cpuUsage}%</div>
            <div className="text-xs text-gray-500">
              {data.system.schedulers} schedulers, queue: {data.system.run_queue}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm text-gray-600">Memory</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-lg font-semibold">{data.system.memoryUsage}%</div>
            <div className="text-xs text-gray-500">
              {formatMemory(data.system.used_memory)} / {formatMemory(data.system.total_memory)}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm text-gray-600">Processes</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{data.system.process_count}</div>
            <div className="text-xs text-gray-500">{data.agents.length} agents active</div>
          </CardContent>
        </Card>
      </div>

      {/* Agents Table */}
      <Card>
        <CardHeader>
          <CardTitle>Active Agents</CardTitle>
        </CardHeader>
        <CardContent>
          {data.agents.length === 0 ? (
            <div className="text-gray-500 text-center py-4">No agents running</div>
          ) : (
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead>
                  <tr className="border-b border-gray-200">
                    <th className="text-left py-2 font-medium text-gray-600">Agent ID</th>
                    <th className="text-left py-2 font-medium text-gray-600">Status</th>
                    <th className="text-right py-2 font-medium text-gray-600">Memory</th>
                    <th className="text-right py-2 font-medium text-gray-600">Queue</th>
                    <th className="text-left py-2 font-medium text-gray-600">Process ID</th>
                  </tr>
                </thead>
                <tbody>
                  {data.agents.map((agent, index) => (
                    <tr key={agent.id} className={index % 2 === 0 ? 'bg-gray-50' : 'bg-white'}>
                      <td className="py-2 font-mono text-xs">{agent.id}</td>
                      <td className="py-2">
                        <span className={`inline-flex items-center px-2 py-1 rounded-full text-xs font-medium ${
                          agent.status === 'alive' 
                            ? 'bg-green-100 text-green-800' 
                            : 'bg-red-100 text-red-800'
                        }`}>
                          {agent.status === 'alive' ? '🟢' : '🔴'} {agent.status}
                        </span>
                      </td>
                      <td className="py-2 text-right font-mono text-xs">
                        {formatMemory(agent.memory)}
                      </td>
                      <td className="py-2 text-right font-mono text-xs">
                        {agent.message_queue}
                      </td>
                      <td className="py-2 font-mono text-xs text-gray-500">
                        {agent.pid.slice(1, -1)} {/* Remove < > from PID */}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </CardContent>
      </Card>

      {/* Footer */}
      <div className="text-center text-xs text-gray-500">
        Last updated: {new Date(data.timestamp).toLocaleTimeString()}
      </div>
    </div>
  );
};

export default CleanSystemDashboard;