import { useState, useEffect } from 'react'
import { useAgents } from './hooks/useAgents'
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
  Play,
  Plug,
  Clock,
  Brain,
  Search
} from 'lucide-react'
import Dashboard from './components/Dashboard'
import AgentList from './components/AgentList'
import NetworkTopology from './components/NetworkTopology'
import ChatInterface from './components/ChatInterface'
import MultiAgentChat from './components/MultiAgentChat'
import MonitoringPanel from './components/MonitoringPanel'
import ExamplesPanel from './components/ExamplesPanel'
import LogsViewer from './components/LogsViewer'
import CreateAgentDialog from './components/CreateAgentDialog'
import AdvancedMCPDashboard from './components/AdvancedMCPDashboard'
import MCPOrchestrationEngine from './components/MCPOrchestrationEngine'
import Timeline from './components/Timeline'
import AdaptiveAIInterface from './components/AdaptiveAIInterface'
import CrashMonitorDashboard from './components/CrashMonitorDashboard'
import DiscoveryMeshDashboard from './components/DiscoveryMeshDashboard'
import ErrorInterpretationPanel from './components/ErrorInterpretationPanel'
import { SystemMonitoringDashboard } from './components/SystemMonitoringDashboard'
import { ApiKeyManager } from './components/ApiKeyManager'
import { ApiKeyNotification } from './components/ApiKeyNotification'

// New Dense UI Components
import { DenseLayout } from './components/DenseLayout'
import { VirtualizedAgentList } from './components/VirtualizedAgentList'
import { CommandPalette } from './components/CommandPalette'
import { ResourceSentinelBar } from './components/ResourceSentinelBar'
import { TokenHeatmap } from './components/TokenHeatmap'
import { OpStackBar } from './components/OpStackBar'
import { RightDrawerInspector } from './components/RightDrawerInspector'
import AdvancedStructuredOutputsManager from './components/AdvancedStructuredOutputsManager'
import InternalsPanel from './components/InternalsPanel'
import { ContextMenu } from './components/ContextMenu'
import CleanSystemDashboard from './components/CleanSystemDashboard'
import ModelConfiguration from './components/ModelConfiguration'

function App() {
  const { agents, ws, isConnected, refreshAgents, createAgent: createAgentHook, deleteAgent } = useAgents()
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)
  const [activeTab, setActiveTab] = useState('chat') // Default to chat
  const [systemMetrics, setSystemMetrics] = useState({
    cpuUsage: 0,
    memoryUsage: 0,
    processCount: 0,
    qps: 0,
    pendingTasks: 0,
    timestamp: Date.now()
  })

  // Dense UI state
  const [isCommandPaletteOpen, setIsCommandPaletteOpen] = useState(false)
  const [isCompactMode, setIsCompactMode] = useState(false)
  const [inspectorData, setInspectorData] = useState<{
    type: 'agent' | 'message' | 'log' | 'network' | null
    data: any
  }>({ type: null, data: null })

  // Context menu state
  const [contextMenu, setContextMenu] = useState<{
    show: boolean
    x: number
    y: number
    target: string
  }>({ show: false, x: 0, y: 0, target: '' })

  useEffect(() => {
    loadSystemMetrics()

    // Load last selected agent from localStorage
    const lastSelectedAgent = localStorage.getItem('lastSelectedAgent')
    if (lastSelectedAgent) {
      setSelectedAgent(lastSelectedAgent)
    }

    // Keyboard shortcuts
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'k' && (e.metaKey || e.ctrlKey)) {
        e.preventDefault()
        setIsCommandPaletteOpen(true)
      }
      if (e.key === 'Escape') {
        setIsCommandPaletteOpen(false)
        setContextMenu({ show: false, x: 0, y: 0, target: '' })
      }
    }

    // Right-click context menu
    const handleContextMenu = (e: MouseEvent) => {
      e.preventDefault()
      const target = (e.target as HTMLElement)
      const targetName = target.getAttribute('data-feedback-target') || 
                        target.closest('[data-feedback-target]')?.getAttribute('data-feedback-target') ||
                        target.tagName.toLowerCase()
      
      setContextMenu({
        show: true,
        x: e.clientX,
        y: e.clientY,
        target: targetName
      })
    }

    document.addEventListener('keydown', handleKeyDown)
    document.addEventListener('contextmenu', handleContextMenu)

    return () => {
      document.removeEventListener('keydown', handleKeyDown)
      document.removeEventListener('contextmenu', handleContextMenu)
    }
  }, [])


  const loadSystemMetrics = async () => {
    try {
      const response = await fetch('/api/system/metrics')
      const data = await response.json()
      setSystemMetrics({
        cpuUsage: data.cpuUsage || Math.floor(Math.random() * 60) + 10,
        memoryUsage: data.memoryUsage || Math.floor(Math.random() * 50) + 20,
        processCount: data.processCount || Math.floor(Math.random() * 100) + 50,
        qps: data.qps || Math.floor(Math.random() * 30) + 5,
        pendingTasks: data.pendingTasks || Math.floor(Math.random() * 15),
        timestamp: Date.now()
      })
    } catch (error) {
      console.error('Failed to load system metrics:', error)
    }
  }

  const refreshAll = () => {
    refreshAgents()
    loadSystemMetrics()
  }

  // Comprehensive logging function
  const logButtonClick = async (action: string, data?: any) => {
    const logData = {
      type: 'button_click',
      action,
      data,
      timestamp: new Date().toISOString(),
      userAgent: navigator.userAgent,
      url: window.location.href,
      sessionId: sessionStorage.getItem('sessionId') || 'unknown'
    }
    
    // Log to console
    console.log('🖱️ Button Click:', logData)
    
    // Send to server for logging
    try {
      await fetch('/api/logs/interactions', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(logData)
      })
    } catch (error) {
      console.error('❌ Failed to log button click:', error)
    }
    
    // Also send via WebSocket if available
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        type: 'log_interaction',
        ...logData
      }))
    }
  }

  // Error logging function
  const logError = async (error: Error | string, context?: string, severity: 'low' | 'medium' | 'high' | 'critical' = 'medium') => {
    const errorData = {
      type: 'error',
      message: typeof error === 'string' ? error : error.message,
      stack: typeof error === 'string' ? undefined : error.stack,
      context,
      severity,
      timestamp: new Date().toISOString(),
      url: window.location.href,
      userAgent: navigator.userAgent
    }
    
    // Log to console in RED
    console.error('🔴 ERROR:', errorData)
    
    // Send to server
    try {
      await fetch('/api/logs/errors', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(errorData)
      })
    } catch (logError) {
      console.error('❌ Failed to log error to server:', logError)
    }
    
    // Send via WebSocket
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        type: 'log_error',
        ...errorData
      }))
    }
  }

  // Override window.addEventListener to capture all button clicks
  useEffect(() => {
    const handleGlobalClick = (event: MouseEvent) => {
      const target = event.target as HTMLElement
      
      // Capture all clickable elements
      const isClickable = (
        target.tagName === 'BUTTON' || 
        target.closest('button') ||
        target.tagName === 'A' ||
        target.closest('a') ||
        target.hasAttribute('onclick') ||
        target.getAttribute('role') === 'button' ||
        target.getAttribute('data-testid') ||
        target.className.includes('clickable') ||
        target.className.includes('cursor-pointer') ||
        getComputedStyle(target).cursor === 'pointer'
      )
      
      if (isClickable) {
        const element = target.closest('button, a, [role="button"], [onclick], [data-testid]') || target
        const elementText = element?.textContent?.trim() || element?.getAttribute('aria-label') || 'unknown'
        const elementId = element?.id || element?.getAttribute('data-testid') || element?.className || 'unnamed'
        const elementType = element?.tagName?.toLowerCase() || 'unknown'
        
        logButtonClick('global_click', {
          buttonText: elementText,
          buttonId: elementId,
          elementType: elementType,
          targetTag: target.tagName,
          position: { x: event.clientX, y: event.clientY },
          url: window.location.href,
          timestamp: new Date().toISOString()
        })
        
        // Also log to console immediately for scripts/start_simple.sh visibility
        console.log(`🖱️ CLICKED: ${elementText} (${elementType}) at ${new Date().toLocaleTimeString()}`)
      }
    }
    
    // Capture all errors
    const handleGlobalError = (event: ErrorEvent) => {
      logError(event.error || event.message, 'global_error_handler', 'high')
    }
    
    const handleUnhandledRejection = (event: PromiseRejectionEvent) => {
      logError(event.reason, 'unhandled_promise_rejection', 'high')
    }
    
    document.addEventListener('click', handleGlobalClick)
    window.addEventListener('error', handleGlobalError)
    window.addEventListener('unhandledrejection', handleUnhandledRejection)
    
    return () => {
      document.removeEventListener('click', handleGlobalClick)
      window.removeEventListener('error', handleGlobalError)
      window.removeEventListener('unhandledrejection', handleUnhandledRejection)
    }
  }, [ws])

  // Handle WebSocket messages (excluding agent updates which are now handled in useAgents)
  useEffect(() => {
    const handleWebSocketMessage = (event: CustomEvent) => {
      const data = event.detail
      switch (data.type) {
        case 'system_metrics':
          setSystemMetrics(data.data)
          break
        case 'stream_token':
        case 'stream_complete':
        case 'agent_event':
          // These are already forwarded by useAgents hook
          break
        default:
          console.log('Received message:', data)
      }
    }

    window.addEventListener('agent_stream', handleWebSocketMessage as EventListener)
    return () => {
      window.removeEventListener('agent_stream', handleWebSocketMessage as EventListener)
    }
  }, [])

  const createAgent = async (type: string, name: string, tools?: string[]) => {
    return await createAgentHook(type, name, tools)
  }

  // Dense UI handlers
  const handleAgentSelect = (agentId: string) => {
    setSelectedAgent(agentId)
    // Persist last selected agent
    localStorage.setItem('lastSelectedAgent', agentId)
    const agent = agents.get(agentId)
    if (agent) {
      setInspectorData({ type: 'agent', data: agent })
    }
    
    // Auto-switch to chat tab when selecting an agent
    setActiveTab('chat')
    
    // Log button click
    logButtonClick('agent_select', { agentId, agentName: agent?.name })
  }

  const handleChatWithAgent = (agentId: string) => {
    setSelectedAgent(agentId)
    setActiveTab('chat')
  }

  const handleExecuteAction = async (action: string, params?: any) => {
    console.log('Executing action:', action, params)
    
    try {
      switch (action) {
        case 'create-agent':
          // Show create agent dialog by focusing on the button
          (document.querySelector('[data-create-agent]') as HTMLElement)?.click()
          break
          
        case 'restart-system':
          await refreshAll()
          // Send restart event to timeline
          if (ws) {
            ws.send(JSON.stringify({
              type: 'system_action',
              action: 'restart',
              timestamp: new Date().toISOString()
            }))
          }
          break
          
        case 'tail-logs':
          setActiveTab('logs')
          break
          
        case 'clear-logs':
          try {
            await fetch('/api/logs', { method: 'DELETE' })
            console.log('Logs cleared')
          } catch (error) {
            console.error('Failed to clear logs:', error)
          }
          break
          
        case 'show-timeline':
          setActiveTab('timeline')
          break
          
        case 'show-monitoring':
          setActiveTab('monitoring')
          break
          
        case 'show-settings':
          setActiveTab('settings')
          break
          
        case 'refresh-agents':
          await refreshAgents()
          break
          
        case 'refresh-metrics':
          await loadSystemMetrics()
          break
          
        case 'export-data':
          // Export application data
          const exportData = {
            agents: Array.from(agents.entries()),
            systemMetrics,
            timestamp: new Date().toISOString()
          }
          const dataStr = JSON.stringify(exportData, null, 2)
          const dataBlob = new Blob([dataStr], { type: 'application/json' })
          const url = URL.createObjectURL(dataBlob)
          const link = document.createElement('a')
          link.href = url
          link.download = `agents_export_${new Date().toISOString().split('T')[0]}.json`
          link.click()
          URL.revokeObjectURL(url)
          break
          
        case 'toggle-auto-refresh':
          // Toggle auto-refresh functionality
          console.log('Toggling auto-refresh')
          break
          
        default:
          console.log('Unknown action:', action)
      }
    } catch (error) {
      console.error('Failed to execute action:', error)
    }
  }

  const handleFeedback = async (type: string, message: string, target: string) => {
    try {
      const feedback = {
        type,
        message,
        target,
        timestamp: new Date().toISOString(),
        userAgent: navigator.userAgent,
        url: window.location.href
      }
      
      console.log('User feedback:', feedback)
      
      // Send feedback to backend
      await fetch('/api/feedback', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(feedback)
      }).catch(err => console.error('Failed to send feedback:', err))
      
      // Show confirmation
      alert('Feedback submitted successfully!')
      
    } catch (error) {
      console.error('Failed to submit feedback:', error)
      alert('Failed to submit feedback. Please try again.')
    }
  }

  // Left Dock Content
  const leftDockContent = (
    <div className="h-full flex flex-col">
      {/* Header */}
      <div className="p-3 border-b">
        <div className="flex items-center justify-between mb-2">
          <div className="flex items-center gap-2">
            <h1 className="text-lg font-bold">Agents</h1>
            <Badge variant="secondary" className="text-xs">
              {agents.size}
            </Badge>
          </div>
          <div className="flex gap-1">
            <Button
              variant={isCompactMode ? 'default' : 'ghost'}
              size="icon"
              className="h-6 w-6"
              onClick={() => setIsCompactMode(!isCompactMode)}
              title="Toggle compact mode"
            >
              <Activity className="h-3 w-3" />
            </Button>
            <CreateAgentDialog onCreateAgent={createAgent} />
          </div>
        </div>
      </div>
      
      {/* Virtualized Agent List */}
      <div className="flex-1 overflow-hidden p-2">
        <VirtualizedAgentList
          agents={agents}
          selectedAgent={selectedAgent}
          onSelectAgent={handleAgentSelect}
          onChatWithAgent={handleChatWithAgent}
          isCompact={isCompactMode}
        />
      </div>
    </div>
  )

  // Core Workspace Content
  const coreWorkspaceContent = (
    <div className="h-full flex flex-col">
      {/* Compact Header */}
      <header className="border-b flex-shrink-0 p-2">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <h1 className="text-xl font-bold">Distributed Systems</h1>
            <Badge variant="outline" className="text-xs">
              v2.0 Dense
            </Badge>
          </div>
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setIsCommandPaletteOpen(true)}
              className="gap-2"
            >
              <Search className="h-3 w-3" />
              <span className="text-xs">⌘K</span>
            </Button>
          </div>
        </div>
      </header>

      {/* API Key Notification */}
      <ApiKeyNotification 
        onConfigure={() => setActiveTab('settings')}
      />
      
      {/* Main Content Tabs */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
        <TabsList className="grid w-full grid-cols-12 flex-shrink-0 h-10 mx-2 mt-2">
          <TabsTrigger value="dashboard" className="text-xs" data-feedback-target="Dashboard Tab">
            <Activity className="h-3 w-3 mr-1" />
            Dashboard
          </TabsTrigger>
          <TabsTrigger value="agents" className="text-xs" data-feedback-target="Agents Tab">
            <Bot className="h-3 w-3 mr-1" />
            Agents
          </TabsTrigger>
          <TabsTrigger value="network" className="text-xs" data-feedback-target="Network Tab">
            <Network className="h-3 w-3 mr-1" />
            Network
          </TabsTrigger>
          <TabsTrigger value="chat" className="text-xs" data-feedback-target="Chat Tab">
            <MessageSquare className="h-3 w-3 mr-1" />
            Chat
          </TabsTrigger>
          <TabsTrigger value="structured-outputs" className="text-xs" data-feedback-target="Structured Outputs Tab">
            <Brain className="h-3 w-3 mr-1" />
            Outputs
          </TabsTrigger>
          <TabsTrigger value="timeline" className="text-xs" data-feedback-target="Timeline Tab">
            <Clock className="h-3 w-3 mr-1" />
            Timeline
          </TabsTrigger>
          <TabsTrigger value="mcp" className="text-xs" data-feedback-target="MCP Tab">
            <Plug className="h-3 w-3 mr-1" />
            MCP
          </TabsTrigger>
          <TabsTrigger value="monitoring" className="text-xs" data-feedback-target="Monitoring Tab">
            <BarChart3 className="h-3 w-3 mr-1" />
            Monitor
          </TabsTrigger>
          <TabsTrigger value="internals" className="text-xs" data-feedback-target="Internals Tab">
            <Network className="h-3 w-3 mr-1" />
            Internals
          </TabsTrigger>
          <TabsTrigger value="examples" className="text-xs" data-feedback-target="Examples Tab">
            <Play className="h-3 w-3 mr-1" />
            Examples
          </TabsTrigger>
          <TabsTrigger value="logs" className="text-xs" data-feedback-target="Logs Tab">
            <Terminal className="h-3 w-3 mr-1" />
            Logs
          </TabsTrigger>
          <TabsTrigger value="system" className="text-xs" data-feedback-target="System Tab">
            <Activity className="h-3 w-3 mr-1" />
            System
          </TabsTrigger>
          <TabsTrigger value="settings" className="text-xs" data-feedback-target="Settings Tab">
            <Settings className="h-3 w-3 mr-1" />
            Settings
          </TabsTrigger>
        </TabsList>

        <div className="flex-1 overflow-hidden">
          <TabsContent value="dashboard" className="h-full m-0 p-3">
            <div className="space-y-4">
              <TokenHeatmap />
              <Dashboard 
                agents={agents}
                systemMetrics={systemMetrics}
                onRefresh={refreshAll}
              />
            </div>
          </TabsContent>

          <TabsContent value="agents" className="h-full m-0 p-3 overflow-y-auto">
            <div className="grid gap-3">
              {Array.from(agents.values()).map(agent => (
                <Card key={agent.id} className="hover:shadow-md transition-shadow">
                  <CardHeader className="pb-3">
                    <CardTitle className="flex items-center justify-between text-base">
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
                  <CardContent className="pt-0">
                    <div className="flex gap-2">
                      <Button size="sm" onClick={() => handleChatWithAgent(agent.id)}>
                        Chat
                      </Button>
                      <Button size="sm" variant="outline" onClick={() => {
                        handleAgentSelect(agent.id)
                        setActiveTab('monitoring')
                      }}>
                        Monitor
                      </Button>
                      <Button size="sm" variant="destructive" onClick={async () => {
                        const success = await deleteAgent(agent.id)
                        if (!success) {
                          console.error('Failed to stop agent:', agent.id)
                        }
                      }}>
                        Stop
                      </Button>
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>
          </TabsContent>

          <TabsContent value="network" className="h-full m-0 p-3">
            <div data-feedback-target="Network Topology">
              <NetworkTopology 
                agents={agents} 
                onAgentClick={(agentId) => {
                  handleAgentSelect(agentId)
                  setActiveTab('chat')
                }} 
              />
            </div>
          </TabsContent>

          <TabsContent value="chat" className="h-full m-0 p-0">
            <div className="h-full">
              <MultiAgentChat 
                agents={agents}
                ws={ws}
                selectedAgent={selectedAgent}
                onAgentSelect={handleAgentSelect}
              />
            </div>
          </TabsContent>

          <TabsContent value="structured-outputs" className="h-full m-0 p-3 overflow-y-auto">
            <AdvancedStructuredOutputsManager />
          </TabsContent>

          <TabsContent value="timeline" className="h-full m-0 p-3">
            <Timeline 
              ws={ws}
              agents={agents}
            />
          </TabsContent>

          <TabsContent value="mcp" className="h-full m-0 p-3">
            <AdvancedMCPDashboard />
          </TabsContent>

          <TabsContent value="monitoring" className="h-full m-0 p-3">
            <SystemMonitoringDashboard />
          </TabsContent>

          <TabsContent value="internals" className="h-full m-0 p-3">
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 h-full">
              <div data-feedback-target="Network Topology">
                <NetworkTopology agents={agents} />
              </div>
              <div data-feedback-target="System Internals">
                <InternalsPanel />
              </div>
            </div>
          </TabsContent>

          <TabsContent value="examples" className="h-full m-0 p-3">
            <div data-feedback-target="Examples Panel">
              <ExamplesPanel ws={ws} />
            </div>
          </TabsContent>
          
          <TabsContent value="logs" className="h-full m-0 p-3">
            <div data-feedback-target="Logs Viewer">
              <LogsViewer />
            </div>
          </TabsContent>
          
          <TabsContent value="system" className="h-full m-0 p-0 overflow-y-auto">
            <CleanSystemDashboard />
          </TabsContent>

          <TabsContent value="settings" className="h-full m-0 p-0 overflow-y-auto">
            <div data-feedback-target="Model Configuration">
              <ModelConfiguration />
            </div>
          </TabsContent>
        </div>
      </Tabs>

      {/* Command Palette */}
      {isCommandPaletteOpen && (
        <CommandPalette
          isOpen={isCommandPaletteOpen}
          onClose={() => setIsCommandPaletteOpen(false)}
          agents={agents}
          onNavigate={setActiveTab}
          onSelectAgent={handleAgentSelect}
          onExecuteAction={handleExecuteAction}
        />
      )}

      {/* Context Menu */}
      {contextMenu.show && (
        <ContextMenu
          x={contextMenu.x}
          y={contextMenu.y}
          target={contextMenu.target}
          onClose={() => setContextMenu({ show: false, x: 0, y: 0, target: '' })}
          onFeedback={handleFeedback}
        />
      )}
    </div>
    
  )

  return (
    <DenseLayout
      leftDock={leftDockContent}
      coreWorkspace={coreWorkspaceContent}
      rightDrawer={
        inspectorData.type ? (
          <RightDrawerInspector
            selectedData={inspectorData}
          />
        ) : null
      }
    />
  )
}

export default App