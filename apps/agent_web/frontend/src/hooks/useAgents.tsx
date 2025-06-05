import { useEffect, useRef, useState } from 'react';

interface Agent {
  id: string;
  name?: string;
  type: string;
  status: string;
  model?: string;
}

export function useAgents() {
  const [agents, setAgents] = useState<Map<string, Agent>>(new Map());
  const wsRef = useRef<WebSocket | null>(null);
  const lastSnapshotRef = useRef<Agent[]>([]);
  const reconnectAttemptsRef = useRef(0);
  const pingIntervalRef = useRef<ReturnType<typeof setInterval> | null>(null);

  useEffect(() => {
    loadInitialAgents();
    connect();

    return () => {
      if (wsRef.current) {
        wsRef.current.close(1000, 'Component unmounting');
      }
      if (pingIntervalRef.current) {
        clearInterval(pingIntervalRef.current);
      }
    };
  }, []);

  const loadInitialAgents = async () => {
    try {
      const response = await fetch('/api/agents');
      const data = await response.json();
      const agentsMap = new Map<string, Agent>();
      data.agents.forEach((agent: Agent) => {
        agentsMap.set(agent.id, agent);
      });
      setAgents(agentsMap);
      
      // Store initial snapshot for comparison
      const snapshot = Array.from(agentsMap.values()).sort((a, b) => a.id.localeCompare(b.id));
      lastSnapshotRef.current = snapshot;
    } catch (error) {
      console.error('Failed to load initial agents:', error);
    }
  };

  const connect = (retries = 0) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      return; // Already connected
    }

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const ws = new WebSocket(`${protocol}//${window.location.host}/ws`);
    wsRef.current = ws;

    // Setup ping to keep connection alive
    const setupPing = () => {
      if (pingIntervalRef.current) {
        clearInterval(pingIntervalRef.current);
      }
      pingIntervalRef.current = setInterval(() => {
        if (ws.readyState === WebSocket.OPEN) {
          ws.send(JSON.stringify({ type: 'ping', timestamp: Date.now() }));
        }
      }, 12000); // Send ping every 12 seconds
    };

    ws.onopen = () => {
      console.log('✅ WebSocket connected');
      reconnectAttemptsRef.current = 0;
      setupPing();
      
      // Send initial ping to establish connection
      ws.send(JSON.stringify({
        type: 'ping',
        timestamp: Date.now()
      }));
    };

    ws.onmessage = (evt) => {
      try {
        const data = JSON.parse(evt.data);
        
        if (data.type === 'agent_update' || data.type === 'agents_snapshot') {
          // Handle agent updates with smart diffing
          const snapshot = data.agents || [];
          updateAgentsWithDiff(snapshot);
        } else {
          // Forward other messages to global handler
          window.dispatchEvent(new CustomEvent('agent_stream', { detail: data }));
        }
      } catch (error) {
        console.error('Failed to parse WebSocket message:', error);
      }
    };

    ws.onerror = (error) => {
      console.error('❌ WebSocket error:', error);
    };

    ws.onclose = (event) => {
      console.log('🔌 WebSocket disconnected:', event.code, event.reason);
      
      if (pingIntervalRef.current) {
        clearInterval(pingIntervalRef.current);
        pingIntervalRef.current = null;
      }

      // Only reconnect if it wasn't a normal closure or component unmounting
      if (event.code !== 1000 && event.code !== 1001) {
        const delay = Math.min(2 ** retries * 1000, 15000);
        console.log(`🔄 Will attempt to reconnect WebSocket in ${delay}ms... (attempt ${retries + 1})`);
        
        setTimeout(() => {
          reconnectAttemptsRef.current = retries + 1;
          connect(retries + 1);
        }, delay);
      }
    };
  };

  const updateAgentsWithDiff = (snapshot: Agent[]) => {
    // Sort snapshot for stable comparison
    const sortedSnapshot = [...snapshot].sort((a, b) => a.id.localeCompare(b.id));
    
    // Check if anything actually changed
    let hasChanges = false;
    
    if (sortedSnapshot.length !== lastSnapshotRef.current.length) {
      hasChanges = true;
    } else {
      // Deep comparison of agents
      for (let i = 0; i < sortedSnapshot.length; i++) {
        const current = sortedSnapshot[i];
        const previous = lastSnapshotRef.current[i];
        
        if (!previous || 
            current.id !== previous.id || 
            current.status !== previous.status || 
            current.name !== previous.name ||
            current.model !== previous.model ||
            current.type !== previous.type) {
          hasChanges = true;
          break;
        }
      }
    }

    // Only update if something actually changed
    if (hasChanges) {
      console.log('📝 Updating agents state (detected changes)');
      const agentsMap = new Map<string, Agent>();
      sortedSnapshot.forEach(agent => {
        agentsMap.set(agent.id, agent);
      });
      
      setAgents(agentsMap);
      lastSnapshotRef.current = sortedSnapshot;
    } else {
      console.log('⏭️ Skipping agent update (no changes detected)');
    }
  };

  const refreshAgents = async () => {
    try {
      const response = await fetch('/api/agents');
      const data = await response.json();
      updateAgentsWithDiff(data.agents || []);
    } catch (error) {
      console.error('Failed to refresh agents:', error);
    }
  };

  const createAgent = async (type: string, name: string, tools?: string[]) => {
    try {
      const response = await fetch('/api/agents', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ type, name, tools })
      });
      
      if (response.ok) {
        await refreshAgents();
        return true;
      }
      return false;
    } catch (error) {
      console.error('Failed to create agent:', error);
      return false;
    }
  };

  const deleteAgent = async (agentId: string) => {
    try {
      const response = await fetch(`/api/agents/${agentId}`, { 
        method: 'DELETE' 
      });
      
      if (response.ok) {
        await refreshAgents();
        return true;
      }
      return false;
    } catch (error) {
      console.error('Failed to delete agent:', error);
      return false;
    }
  };

  return {
    agents,
    ws: wsRef.current,
    isConnected: wsRef.current?.readyState === WebSocket.OPEN,
    refreshAgents,
    createAgent,
    deleteAgent
  };
}