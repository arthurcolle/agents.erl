import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  Activity, 
  Bot, 
  Network, 
  MessageSquare, 
  BarChart3,
  Terminal,
  Settings,
  Play,
  Plug,
  Clock,
  Brain,
  Plus,
  Users,
  Cpu,
  MemoryStick
} from 'lucide-react'
import MCPOrchestrationEngine from './MCPOrchestrationEngine'
import ClaudeCLI from './ClaudeCLI'
import ChatInterface from './ChatInterface'

interface Agent {
  id: string
  name: string
  type: string
  status: 'active' | 'inactive' | 'pending'
  model?: string
  lastActivity?: string
}

interface MainLayoutWithSidebarProps {
  agents: Map<string, any>
  systemMetrics: {
    cpuUsage: number
    memoryUsage: number
    processCount: number
  }
  onRefresh: () => void
  onCreateAgent: (type: string, name: string, tools?: string[]) => void
  ws?: WebSocket | null
}

export default function MainLayoutWithSidebar({ 
  agents, 
  systemMetrics, 
  onRefresh, 
  onCreateAgent,
  ws 
}: MainLayoutWithSidebarProps) {
  const [activeTab, setActiveTab] = useState('orchestration')
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)

  // Generate mock agent data if none provided
  const mockAgents = [
    { id: 'c229a555', name: 'Task Orchestrator', type: 'gpt-4o', status: 'active' },
    { id: 'b8a0585c', name: 'Research Assistant', type: 'gpt-4.1', status: 'active' },
    { id: '87f419f3', name: 'Data Analyst', type: 'gpt-4o', status: 'active' },
    { id: '9014e327', name: 'Educational Assistant', type: 'gpt-4o', status: 'active' },
    { id: '70f6e4a5', name: 'Debug Assistant', type: 'gpt-4o', status: 'active' },
    { id: '69e2585', name: 'Language Translator', type: 'gpt-4o', status: 'active' },
    { id: '3fb7ec1e', name: 'Code Assistant', type: 'gpt-4o', status: 'active' },
    { id: '295d6c87', name: 'System Monitor', type: 'gpt-4o-mini', status: 'active' }
  ]

  const agentList = agents.size > 0 ? Array.from(agents.values()) : mockAgents

  const tabs = [
    { id: 'orchestration', icon: Plug, label: 'MCP' },
    { id: 'cli', icon: Terminal, label: 'CLI' },
    { id: 'network', icon: Network, label: 'Network' },
    { id: 'chat', icon: MessageSquare, label: 'Chat' },
    { id: 'timeline', icon: Clock, label: 'Timeline' },
    { id: 'monitoring', icon: BarChart3, label: 'Monitor' },
    { id: 'examples', icon: Play, label: 'Examples' },
    { id: 'logs', icon: Terminal, label: 'Logs' }
  ]

  return (
    <div className="h-screen flex bg-gray-50 dark:bg-gray-900">
      {/* Left Sidebar - Active Agents */}
      <div className="w-80 bg-white dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700 flex flex-col shadow-lg">
        {/* Sidebar Header */}
        <div className="p-6 border-b border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-lg font-bold text-gray-900 dark:text-white">Active Agents</h2>
            <Button size="sm" className="bg-blue-600 hover:bg-blue-700">
              <Plus className="w-4 h-4 mr-1" />
              New Agent
            </Button>
          </div>
          
          {/* System Status Indicators */}
          <div className="space-y-3">
            <div className="flex items-center justify-between text-sm">
              <div className="flex items-center gap-2">
                <Cpu className="w-4 h-4 text-blue-500" />
                <span className="text-gray-600 dark:text-gray-300">CPU Usage</span>
              </div>
              <div className="flex items-center gap-2">
                <span className="font-medium text-gray-900 dark:text-white">{systemMetrics.cpuUsage || 1}%</span>
                <div className="w-16">
                  <Progress value={systemMetrics.cpuUsage || 1} className="h-2" />
                </div>
              </div>
            </div>
            
            <div className="flex items-center justify-between text-sm">
              <div className="flex items-center gap-2">
                <MemoryStick className="w-4 h-4 text-green-500" />
                <span className="text-gray-600 dark:text-gray-300">Memory</span>
              </div>
              <div className="flex items-center gap-2">
                <span className="font-medium text-gray-900 dark:text-white">{systemMetrics.memoryUsage || 100}%</span>
                <div className="w-16">
                  <Progress value={systemMetrics.memoryUsage || 100} className="h-2" />
                </div>
              </div>
            </div>
            
            <div className="flex items-center justify-between text-sm">
              <div className="flex items-center gap-2">
                <Activity className="w-4 h-4 text-purple-500" />
                <span className="text-gray-600 dark:text-gray-300">Processes</span>
              </div>
              <span className="font-medium text-gray-900 dark:text-white">{systemMetrics.processCount || 0}</span>
            </div>
          </div>
        </div>

        {/* Agent List */}
        <div className="flex-1 overflow-y-auto p-4 space-y-3">
          {agentList.map((agent) => (
            <Card 
              key={agent.id} 
              className={`hover:shadow-md transition-shadow cursor-pointer group ${
                selectedAgent === agent.id ? 'ring-2 ring-blue-500 bg-blue-50 dark:bg-blue-900' : ''
              }`}
              onClick={() => {
                setSelectedAgent(agent.id)
                setActiveTab('chat')
              }}
            >
              <CardContent className="p-4">
                <div className="flex items-center justify-between mb-2">
                  <div className="flex items-center gap-2">
                    <Bot className="w-4 h-4 text-blue-500" />
                    <span className="font-medium text-sm text-gray-900 dark:text-white">
                      {agent.name || agent.type}
                    </span>
                  </div>
                  <div className={`w-2 h-2 rounded-full ${
                    agent.status === 'active' ? 'bg-green-500' : 'bg-gray-400'
                  }`} />
                </div>
                
                <div className="text-xs text-gray-500 dark:text-gray-400 mb-2">
                  {agent.type}
                </div>
                
                <div className="flex items-center justify-between">
                  <Badge 
                    variant={agent.status === 'active' ? 'default' : 'secondary'} 
                    className="text-xs"
                  >
                    {agent.id.substring(0, 8)}
                  </Badge>
                  <span className="text-xs text-gray-500 dark:text-gray-400">
                    {agent.status}
                  </span>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>
      </div>

      {/* Main Content Area */}
      <div className="flex-1 flex flex-col">
        {/* Top Header */}
        <header className="bg-white dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 px-6 py-4 shadow-sm">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 dark:text-white">
                Erlang Agent System
              </h1>
              <p className="text-sm text-gray-600 dark:text-gray-300">
                Advanced Multi-Agent Framework with AI Integration
              </p>
            </div>
            
            <div className="flex items-center gap-2">
              <Settings className="w-5 h-5 text-gray-400 cursor-pointer hover:text-gray-600" />
            </div>
          </div>
        </header>

        {/* Navigation Tabs */}
        <Tabs value={activeTab} onValueChange={setActiveTab} className="flex-1 flex flex-col">
          <TabsList className="mx-6 mt-4 grid grid-cols-8 bg-gray-100 dark:bg-gray-700">
            {tabs.map(({ id, icon: Icon, label }) => (
              <TabsTrigger key={id} value={id} className="flex items-center gap-2 text-xs">
                <Icon className="w-3 w-3" />
                {label}
              </TabsTrigger>
            ))}
          </TabsList>

          {/* Tab Content */}
          <div className="flex-1 overflow-hidden">
            <TabsContent value="orchestration" className="h-full m-0">
              <MCPOrchestrationEngine />
            </TabsContent>
            
            <TabsContent value="cli" className="h-full m-0 p-6">
              <ClaudeCLI className="h-full" />
            </TabsContent>
            
            <TabsContent value="network" className="h-full m-0 p-6">
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <Network className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                  <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">Network Topology</h3>
                  <p className="text-gray-600 dark:text-gray-300">Network visualization coming soon</p>
                </div>
              </div>
            </TabsContent>
            
            <TabsContent value="chat" className="h-full m-0 p-0">
              {selectedAgent ? (
                <div className="h-full p-3">
                  <ChatInterface 
                    selectedAgent={selectedAgent}
                    agents={agents}
                    ws={ws}
                  />
                </div>
              ) : (
                <div className="h-full flex items-center justify-center">
                  <div className="text-center">
                    <MessageSquare className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                    <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">Chat Interface</h3>
                    <p className="text-gray-600 dark:text-gray-300">Select an agent from the sidebar to start chatting</p>
                  </div>
                </div>
              )}
            </TabsContent>
            
            <TabsContent value="timeline" className="h-full m-0 p-6">
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <Clock className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                  <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">Timeline</h3>
                  <p className="text-gray-600 dark:text-gray-300">Timeline view coming soon</p>
                </div>
              </div>
            </TabsContent>
            
            <TabsContent value="monitoring" className="h-full m-0 p-6">
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <BarChart3 className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                  <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">System Monitor</h3>
                  <p className="text-gray-600 dark:text-gray-300">Monitoring dashboard coming soon</p>
                </div>
              </div>
            </TabsContent>
            
            <TabsContent value="examples" className="h-full m-0 p-6">
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <Play className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                  <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">Examples</h3>
                  <p className="text-gray-600 dark:text-gray-300">Example workflows coming soon</p>
                </div>
              </div>
            </TabsContent>
            
            <TabsContent value="logs" className="h-full m-0 p-6">
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <Terminal className="w-16 h-16 text-gray-400 mx-auto mb-4" />
                  <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">System Logs</h3>
                  <p className="text-gray-600 dark:text-gray-300">Log viewer coming soon</p>
                </div>
              </div>
            </TabsContent>
          </div>
        </Tabs>
      </div>
    </div>
  )
}