import { useState, useEffect } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { 
  Activity, 
  Bot, 
  Network, 
  MessageSquare, 
  BarChart3,
  Terminal,
  Settings,
  Play
} from 'lucide-react'
import Dashboard from './components/Dashboard'
import AgentList from './components/AgentList'
import NetworkTopology from './components/NetworkTopology'
import ChatInterface from './components/ChatInterface'
import MonitoringPanel from './components/MonitoringPanel'
import ExamplesPanel from './components/ExamplesPanel'
import LogsViewer from './components/LogsViewer'
import CreateAgentDialog from './components/CreateAgentDialog'

function App() {
  const [agents, setAgents] = useState<Map<string, any>>(new Map())
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)
  const [activeTab, setActiveTab] = useState('dashboard')
  const [ws, setWs] = useState<WebSocket | null>(null)
  const [systemMetrics, setSystemMetrics] = useState({
    cpuUsage: 0,
    memoryUsage: 0,
    processCount: 0
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
    
    websocket.onopen = () => {
      console.log('WebSocket connected')
    }

    websocket.onmessage = (event) => {
      const data = JSON.parse(event.data)
      handleWebSocketMessage(data)
    }

    websocket.onerror = (error) => {
      console.error('WebSocket error:', error)
    }

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
      
      if (response.ok) {
        await loadAgents()
      }
    } catch (error) {
      console.error('Failed to create agent:', error)
    }
  }

  return (
    <div className="min-h-screen bg-background">
      <header className="border-b">
        <div className="container mx-auto px-4 py-4">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold">Erlang Agent System</h1>
              <p className="text-muted-foreground">Advanced Multi-Agent Framework with AI Integration</p>
            </div>
            <div className="flex items-center gap-4">
              <Button variant="outline" size="icon">
                <Settings className="h-4 w-4" />
              </Button>
            </div>
          </div>
        </div>
      </header>

      <div className="container mx-auto px-4 py-6">
        <div className="grid grid-cols-12 gap-6">
          <aside className="col-span-3">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center justify-between">
                  <span>Active Agents</span>
                  <CreateAgentDialog onCreateAgent={createAgent} />
                </CardTitle>
              </CardHeader>
              <CardContent>
                <AgentList 
                  agents={agents} 
                  selectedAgent={selectedAgent}
                  onSelectAgent={setSelectedAgent}
                />
              </CardContent>
            </Card>
          </aside>

          <main className="col-span-9">
            <Tabs value={activeTab} onValueChange={setActiveTab}>
              <TabsList className="grid w-full grid-cols-7">
                <TabsTrigger value="dashboard">
                  <Activity className="h-4 w-4 mr-2" />
                  Dashboard
                </TabsTrigger>
                <TabsTrigger value="agents">
                  <Bot className="h-4 w-4 mr-2" />
                  Agents
                </TabsTrigger>
                <TabsTrigger value="network">
                  <Network className="h-4 w-4 mr-2" />
                  Network
                </TabsTrigger>
                <TabsTrigger value="chat">
                  <MessageSquare className="h-4 w-4 mr-2" />
                  Chat
                </TabsTrigger>
                <TabsTrigger value="monitoring">
                  <BarChart3 className="h-4 w-4 mr-2" />
                  Monitor
                </TabsTrigger>
                <TabsTrigger value="examples">
                  <Play className="h-4 w-4 mr-2" />
                  Examples
                </TabsTrigger>
                <TabsTrigger value="logs">
                  <Terminal className="h-4 w-4 mr-2" />
                  Logs
                </TabsTrigger>
              </TabsList>

              <TabsContent value="dashboard">
                <Dashboard 
                  agents={agents}
                  systemMetrics={systemMetrics}
                  onRefresh={loadAgents}
                />
              </TabsContent>

              <TabsContent value="agents">
                <div className="grid gap-4">
                  {Array.from(agents.values()).map(agent => (
                    <Card key={agent.id}>
                      <CardHeader>
                        <CardTitle className="flex items-center justify-between">
                          <span>{agent.name || agent.type}</span>
                          <div className="flex gap-2">
                            {agent.model && (
                              <Badge variant="secondary" className="text-xs">
                                {agent.model}
                              </Badge>
                            )}
                            <Badge variant="outline" className="text-xs">
                              {String(agent.id).substring(0, 8)}
                            </Badge>
                          </div>
                        </CardTitle>
                        <CardDescription>Status: {agent.status || 'Active'}</CardDescription>
                      </CardHeader>
                      <CardContent>
                        <div className="flex gap-2">
                          <Button size="sm" onClick={() => setSelectedAgent(agent.id)}>
                            Select
                          </Button>
                          <Button size="sm" variant="outline">
                            Details
                          </Button>
                          <Button size="sm" variant="destructive">
                            Stop
                          </Button>
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </TabsContent>

              <TabsContent value="network">
                <NetworkTopology agents={agents} />
              </TabsContent>

              <TabsContent value="chat">
                <ChatInterface 
                  selectedAgent={selectedAgent}
                  agents={agents}
                  ws={ws}
                />
              </TabsContent>

              <TabsContent value="monitoring">
                <MonitoringPanel 
                  agents={agents}
                  systemMetrics={systemMetrics}
                />
              </TabsContent>

              <TabsContent value="examples">
                <ExamplesPanel ws={ws} />
              </TabsContent>

              <TabsContent value="logs">
                <LogsViewer />
              </TabsContent>
            </Tabs>
          </main>
        </div>
      </div>
    </div>
  )
}

export default App