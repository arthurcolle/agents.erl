import { useState, useEffect } from 'react'
import { Card } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { ScrollArea } from '@/components/ui/scroll-area'
import { 
  Activity, Bot, Network, MessageSquare, BarChart3, Terminal,
  Settings, Play, Plug, Clock, Brain, Home, BookOpen, GitBranch,
  Zap, MessageCircle, ChevronLeft, ChevronRight, X, Plus
} from 'lucide-react'
import Dashboard from './Dashboard'
import AgentList from './AgentList'
import NetworkTopology from './NetworkTopology'
import ChatInterface from './ChatInterface'
import MonitoringPanel from './MonitoringPanel'
import ExamplesPanel from './ExamplesPanel'
import LogsViewer from './LogsViewer'
import CreateAgentDialog from './CreateAgentDialog'
import AdvancedMCPDashboard from './AdvancedMCPDashboard'
import MCPOrchestrationEngine from './MCPOrchestrationEngine'
import Timeline from './Timeline'
import AdaptiveAIInterface from './AdaptiveAIInterface'
import AgentCommunicationPanel from './AgentCommunicationPanel'
import AgentQuorumPanel from './AgentQuorumPanel'

interface Panel {
  id: string
  component: string
  props?: any
  width?: number
}

export default function DenseApp() {
  const [agents, setAgents] = useState<Map<string, any>>(new Map())
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)
  const [activeTab, setActiveTab] = useState('dashboard')
  const [ws, setWs] = useState<WebSocket | null>(null)
  const [sidebarCollapsed, setSidebarCollapsed] = useState(false)
  const [panels, setPanels] = useState<Panel[]>([
    { id: 'main', component: 'dashboard', width: 70 },
    { id: 'comm', component: 'communication', width: 30 }
  ])
  const [systemMetrics, setSystemMetrics] = useState({
    cpuUsage: 0,
    memoryUsage: 0,
    processCount: 0,
    schedulers: 0,
    runQueue: 0
  })

  useEffect(() => {
    loadAgents()
    connectWebSocket()
    return () => {
      if (ws) ws.close()
    }
  }, [])

  const loadAgents = async () => {
    try {
      const response = await fetch('/api/agents')
      const data = await response.json()
      const agentsMap = new Map()
      data.agents.forEach((agent: any) => {
        agentsMap.set(agent.id, agent)
      })
      setAgents(agentsMap)
    } catch (error) {
      console.error('Failed to load agents:', error)
    }
  }

  const connectWebSocket = () => {
    const websocket = new WebSocket(`ws://${window.location.host}/ws`)
    
    websocket.onopen = () => console.log('WebSocket connected')
    websocket.onmessage = (event) => {
      const data = JSON.parse(event.data)
      handleWebSocketMessage(data)
    }
    websocket.onerror = (error) => console.error('WebSocket error:', error)
    websocket.onclose = () => {
      console.log('WebSocket disconnected')
      setTimeout(() => connectWebSocket(), 5000)
    }
    setWs(websocket)
  }

  const handleWebSocketMessage = (data: any) => {
    switch (data.type) {
      case 'system_metrics':
        setSystemMetrics(data.data)
        break
      case 'agent_update':
        loadAgents()
        break
      case 'stream_token':
      case 'stream_complete':
      case 'agent_event':
      case 'agent_communication':
        window.dispatchEvent(new CustomEvent('agent_stream', { detail: data }))
        break
      default:
        console.log('Received message:', data)
    }
  }

  const createAgent = async (type: string, name: string, tools?: string[]) => {
    try {
      const response = await fetch('/api/agents', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ type, name, tools })
      })
      if (response.ok) await loadAgents()
    } catch (error) {
      console.error('Failed to create agent:', error)
    }
  }

  const tabs = [
    { id: 'dashboard', icon: Home, label: 'Dashboard' },
    { id: 'agents', icon: Bot, label: 'Agents' },
    { id: 'communication', icon: MessageCircle, label: 'Communication' },
    { id: 'quorum', icon: GitBranch, label: 'Quorum' },
    { id: 'network', icon: Network, label: 'Network' },
    { id: 'chat', icon: MessageSquare, label: 'Chat' },
    { id: 'timeline', icon: Clock, label: 'Timeline' },
    { id: 'mcp', icon: Plug, label: 'MCP' },
    { id: 'monitor', icon: Activity, label: 'Monitor' },
    { id: 'ai', icon: Brain, label: 'AI' },
    { id: 'logs', icon: Terminal, label: 'Logs' },
    { id: 'settings', icon: Settings, label: 'Settings' }
  ]

  const renderPanel = (panel: Panel) => {
    switch (panel.component) {
      case 'dashboard':
        return <Dashboard agents={agents} systemMetrics={systemMetrics} onRefresh={loadAgents} />
      case 'agents':
        return <DenseAgentList agents={agents} onSelectAgent={setSelectedAgent} />
      case 'communication':
        return <AgentCommunicationPanel agents={agents} ws={ws} />
      case 'quorum':
        return <AgentQuorumPanel agents={agents} ws={ws} />
      case 'network':
        return <NetworkTopology agents={agents} />
      case 'chat':
        return <ChatInterface selectedAgent={selectedAgent} agents={agents} ws={ws} />
      case 'timeline':
        return <Timeline ws={ws} agents={agents} />
      case 'mcp':
        return <AdvancedMCPDashboard />
      case 'monitor':
        return <MonitoringPanel selectedAgent={selectedAgent} agents={agents} systemMetrics={{cpuUsage: 0, memoryUsage: 0, processCount: 0}} />
      case 'ai':
        return <AdaptiveAIInterface />
      case 'logs':
        return <LogsViewer />
      default:
        return null
    }
  }

  return (
    <div className="h-screen flex flex-col bg-gray-950 text-gray-100">
      {/* Compact Header */}
      <header className="h-10 border-b border-gray-800 flex items-center justify-between px-2 flex-shrink-0">
        <div className="flex items-center gap-2">
          <Brain className="w-5 h-5 text-cyan-500" />
          <span className="text-sm font-bold">Neural Agent System</span>
          <div className="flex items-center gap-1 text-xs">
            <Badge variant="outline" className="py-0 px-1">
              {agents.size} agents
            </Badge>
            <Badge variant="outline" className="py-0 px-1">
              CPU: {systemMetrics.cpuUsage}%
            </Badge>
            <Badge variant="outline" className="py-0 px-1">
              MEM: {systemMetrics.memoryUsage}%
            </Badge>
            <Badge variant="outline" className="py-0 px-1">
              Q: {systemMetrics.runQueue}
            </Badge>
          </div>
        </div>
        <div className="flex items-center gap-1">
          <CreateAgentDialog onCreateAgent={createAgent} />
          <Button size="icon" variant="ghost" className="h-6 w-6">
            <Settings className="h-3 w-3" />
          </Button>
        </div>
      </header>

      <div className="flex-1 flex overflow-hidden">
        {/* Collapsible Sidebar */}
        <aside className={`${sidebarCollapsed ? 'w-10' : 'w-40'} transition-all duration-200 border-r border-gray-800 flex flex-col bg-gray-900`}>
          <div className="flex items-center justify-between p-1">
            <Button
              size="icon"
              variant="ghost"
              className="h-6 w-6"
              onClick={() => setSidebarCollapsed(!sidebarCollapsed)}
            >
              {sidebarCollapsed ? <ChevronRight className="h-3 w-3" /> : <ChevronLeft className="h-3 w-3" />}
            </Button>
          </div>
          
          <ScrollArea className="flex-1">
            <div className="space-y-0.5 p-1">
              {tabs.map(({ id, icon: Icon, label }) => (
                <button
                  key={id}
                  onClick={() => setActiveTab(id)}
                  className={`w-full flex items-center gap-2 px-2 py-1 rounded text-xs transition-colors ${
                    activeTab === id
                      ? 'bg-blue-600 text-white'
                      : 'text-gray-400 hover:text-white hover:bg-gray-800'
                  }`}
                >
                  <Icon className="h-3 w-3 flex-shrink-0" />
                  {!sidebarCollapsed && <span className="truncate">{label}</span>}
                </button>
              ))}
            </div>
          </ScrollArea>

          {!sidebarCollapsed && (
            <div className="p-2 border-t border-gray-800">
              <DenseAgentList 
                agents={agents} 
                selectedAgent={selectedAgent}
                onSelectAgent={setSelectedAgent}
                mini={true}
              />
            </div>
          )}
        </aside>

        {/* Multi-Panel Content Area */}
        <main className="flex-1 flex">
          {panels.map((panel, index) => (
            <div
              key={panel.id}
              className="h-full flex flex-col border-r border-gray-800 last:border-r-0"
              style={{ width: `${panel.width}%` }}
            >
              <div className="h-8 bg-gray-900 flex items-center justify-between px-2 border-b border-gray-800">
                <span className="text-xs font-medium">{panel.component}</span>
                <div className="flex items-center gap-1">
                  <Button
                    size="icon"
                    variant="ghost"
                    className="h-5 w-5"
                    onClick={() => {
                      const newPanels = [...panels]
                      newPanels[index].component = activeTab
                      setPanels(newPanels)
                    }}
                  >
                    <Plus className="h-3 w-3" />
                  </Button>
                  {panels.length > 1 && (
                    <Button
                      size="icon"
                      variant="ghost"
                      className="h-5 w-5"
                      onClick={() => setPanels(panels.filter((_, i) => i !== index))}
                    >
                      <X className="h-3 w-3" />
                    </Button>
                  )}
                </div>
              </div>
              <div className="flex-1 overflow-auto p-2">
                {renderPanel(panel)}
              </div>
            </div>
          ))}
        </main>
      </div>
    </div>
  )
}

// Dense Agent List Component
function DenseAgentList({ agents, selectedAgent, onSelectAgent, mini = false }: any) {
  return (
    <div className="space-y-1">
      {mini && <div className="text-xs font-medium text-gray-500 mb-1">Active Agents</div>}
      {Array.from(agents.values()).map((agent: any) => (
        <button
          key={agent.id}
          onClick={() => onSelectAgent(agent.id)}
          className={`w-full flex items-center gap-1 px-1 py-0.5 rounded text-xs transition-colors ${
            selectedAgent === agent.id
              ? 'bg-blue-600 text-white'
              : 'text-gray-400 hover:text-white hover:bg-gray-800'
          }`}
        >
          <Bot className="h-3 w-3 flex-shrink-0" />
          <span className="truncate flex-1 text-left">{agent.name || agent.type}</span>
          <div className={`w-1 h-1 rounded-full ${
            agent.status === 'active' ? 'bg-green-500' : 'bg-gray-500'
          }`} />
        </button>
      ))}
    </div>
  )
}