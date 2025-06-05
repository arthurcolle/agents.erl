import { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { ScrollArea } from '@/components/ui/scroll-area'
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible'
import { 
  ChevronDown, 
  ChevronRight, 
  Activity, 
  Cpu, 
  MemoryStick, 
  HardDrive,
  Network,
  RefreshCw,
  Zap,
  Clock,
  TrendingUp
} from 'lucide-react'

interface SystemMetrics {
  timestamp: number
  memory: {
    total: number
    allocated: number
    erlang_total: number
    erlang_processes: number
    erlang_system: number
    erlang_atom: number
    erlang_binary: number
    erlang_code: number
    erlang_ets: number
  }
  cpu: {
    utilization: number
    runtime: number
  }
  processes: {
    count: number
    limit: number
    utilization: number
  }
  reductions: {
    total: number
  }
  agents: {
    count: number
  }
  uptime: number
}

interface SupervisionTreeNode {
  name: string
  type: string
  pid: string
  status: string
  children: SupervisionTreeNode[]
}

interface SupervisionTree {
  timestamp: number
  tree: SupervisionTreeNode[]
}

const formatBytes = (bytes: number) => {
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB']
  if (bytes === 0) return '0 Bytes'
  const i = Math.floor(Math.log(bytes) / Math.log(1024))
  return Math.round(bytes / Math.pow(1024, i) * 100) / 100 + ' ' + sizes[i]
}

const formatDuration = (ms: number) => {
  const seconds = Math.floor(ms / 1000)
  const minutes = Math.floor(seconds / 60)
  const hours = Math.floor(minutes / 60)
  const days = Math.floor(hours / 24)
  
  if (days > 0) return `${days}d ${hours % 24}h`
  if (hours > 0) return `${hours}h ${minutes % 60}m`
  if (minutes > 0) return `${minutes}m ${seconds % 60}s`
  return `${seconds}s`
}

const SupervisionTreeNode = ({ node, level = 0 }: { node: SupervisionTreeNode; level?: number }) => {
  const [isOpen, setIsOpen] = useState(level < 2)
  const hasChildren = node.children && node.children.length > 0
  
  const getStatusColor = (status: string) => {
    switch (status.toLowerCase()) {
      case 'running': return 'bg-green-500'
      case 'waiting': return 'bg-yellow-500'
      case 'suspended': return 'bg-orange-500'
      case 'terminated': return 'bg-red-500'
      case 'undefined': return 'bg-gray-500'
      default: return 'bg-blue-500'
    }
  }
  
  const getTypeIcon = (type: string) => {
    switch (type) {
      case 'supervisor': return <Network className="h-4 w-4" />
      case 'worker': return <Zap className="h-4 w-4" />
      default: return <Activity className="h-4 w-4" />
    }
  }
  
  return (
    <div className={`ml-${level * 4}`}>
      <Collapsible open={isOpen} onOpenChange={setIsOpen}>
        <CollapsibleTrigger asChild>
          <div className="flex items-center gap-2 p-2 hover:bg-gray-50 rounded cursor-pointer">
            {hasChildren ? (
              isOpen ? <ChevronDown className="h-4 w-4" /> : <ChevronRight className="h-4 w-4" />
            ) : (
              <div className="w-4" />
            )}
            {getTypeIcon(node.type)}
            <span className="font-medium">{node.name}</span>
            <Badge variant="outline" className="text-xs">
              {node.type}
            </Badge>
            <div className={`w-2 h-2 rounded-full ${getStatusColor(node.status)}`} title={node.status} />
            <span className="text-xs text-gray-500 font-mono">{node.pid}</span>
          </div>
        </CollapsibleTrigger>
        {hasChildren && (
          <CollapsibleContent>
            <div className="ml-4 border-l border-gray-200">
              {node.children.map((child, index) => (
                <SupervisionTreeNode key={index} node={child} level={level + 1} />
              ))}
            </div>
          </CollapsibleContent>
        )}
      </Collapsible>
    </div>
  )
}

export default function InternalsPanel() {
  const [systemMetrics, setSystemMetrics] = useState<SystemMetrics | null>(null)
  const [supervisionTree, setSupervisionTree] = useState<SupervisionTree | null>(null)
  const [ws, setWs] = useState<WebSocket | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  
  useEffect(() => {
    loadSupervisionTree()
    connectMetricsWebSocket()
    
    return () => {
      if (ws) ws.close()
    }
  }, [])
  
  const loadSupervisionTree = async () => {
    try {
      const response = await fetch('/api/system/supervision-tree')
      const data = await response.json()
      setSupervisionTree(data)
    } catch (error) {
      console.error('Failed to load supervision tree:', error)
    } finally {
      setIsLoading(false)
    }
  }
  
  const connectMetricsWebSocket = () => {
    const websocket = new WebSocket(`ws://${window.location.host}/ws/system/metrics`)
    
    websocket.onopen = () => {
      console.log('Metrics WebSocket connected')
    }
    
    websocket.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data)
        setSystemMetrics(data)
      } catch (error) {
        console.error('Failed to parse metrics data:', error)
      }
    }
    
    websocket.onerror = (error) => {
      console.error('Metrics WebSocket error:', error)
    }
    
    websocket.onclose = () => {
      console.log('Metrics WebSocket disconnected')
      setTimeout(() => connectMetricsWebSocket(), 5000)
    }
    
    setWs(websocket)
  }
  
  const refresh = () => {
    loadSupervisionTree()
    setIsLoading(true)
  }
  
  return (
    <div className="h-full flex flex-col space-y-4">
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold">System Internals</h2>
        <Button onClick={refresh} disabled={isLoading} className="gap-2">
          <RefreshCw className={`h-4 w-4 ${isLoading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>
      
      <Tabs defaultValue="metrics" className="flex-1 flex flex-col">
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="metrics" className="gap-2">
            <Activity className="h-4 w-4" />
            Real-time Metrics
          </TabsTrigger>
          <TabsTrigger value="supervision" className="gap-2">
            <Network className="h-4 w-4" />
            Supervision Tree
          </TabsTrigger>
          <TabsTrigger value="memory" className="gap-2">
            <MemoryStick className="h-4 w-4" />
            Memory Analysis
          </TabsTrigger>
        </TabsList>
        
        <TabsContent value="metrics" className="flex-1 space-y-4">
          {systemMetrics && (
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">CPU Usage</CardTitle>
                  <Cpu className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {systemMetrics.cpu.utilization.toFixed(1)}%
                  </div>
                  <p className="text-xs text-muted-foreground">
                    Runtime: {formatDuration(systemMetrics.cpu.runtime)}
                  </p>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">Memory</CardTitle>
                  <MemoryStick className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {formatBytes(systemMetrics.memory.erlang_total)}
                  </div>
                  <p className="text-xs text-muted-foreground">
                    Processes: {formatBytes(systemMetrics.memory.erlang_processes)}
                  </p>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">Processes</CardTitle>
                  <Activity className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {systemMetrics.processes.count}
                  </div>
                  <p className="text-xs text-muted-foreground">
                    {systemMetrics.processes.utilization.toFixed(1)}% of limit
                  </p>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">Agents</CardTitle>
                  <Network className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {systemMetrics.agents.count}
                  </div>
                  <p className="text-xs text-muted-foreground">
                    Active agents
                  </p>
                </CardContent>
              </Card>
              
              <Card className="col-span-2">
                <CardHeader>
                  <CardTitle className="text-sm font-medium">Reductions</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {systemMetrics.reductions.total.toLocaleString()}
                  </div>
                  <p className="text-xs text-muted-foreground">
                    Total function calls executed
                  </p>
                </CardContent>
              </Card>
              
              <Card className="col-span-2">
                <CardHeader>
                  <CardTitle className="text-sm font-medium">System Uptime</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">
                    {formatDuration(systemMetrics.uptime)}
                  </div>
                  <p className="text-xs text-muted-foreground">
                    Since last restart
                  </p>
                </CardContent>
              </Card>
            </div>
          )}
          
          {!systemMetrics && (
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-center">
                  <div className="text-center">
                    <RefreshCw className="h-8 w-8 animate-spin mx-auto mb-2" />
                    <p className="text-sm text-muted-foreground">Loading real-time metrics...</p>
                  </div>
                </div>
              </CardContent>
            </Card>
          )}
        </TabsContent>
        
        <TabsContent value="supervision" className="flex-1">
          <Card className="h-full">
            <CardHeader>
              <CardTitle>OTP Supervision Tree</CardTitle>
              <CardDescription>
                Live view of the Erlang/OTP supervision hierarchy
              </CardDescription>
            </CardHeader>
            <CardContent className="flex-1">
              <ScrollArea className="h-[600px]">
                {supervisionTree ? (
                  <div className="space-y-2">
                    {supervisionTree.tree.map((rootNode, index) => (
                      <SupervisionTreeNode key={index} node={rootNode} />
                    ))}
                  </div>
                ) : isLoading ? (
                  <div className="flex items-center justify-center h-40">
                    <RefreshCw className="h-8 w-8 animate-spin" />
                  </div>
                ) : (
                  <p className="text-muted-foreground">No supervision tree data available</p>
                )}
              </ScrollArea>
            </CardContent>
          </Card>
        </TabsContent>
        
        <TabsContent value="memory" className="flex-1 space-y-4">
          {systemMetrics && (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <Card>
                <CardHeader>
                  <CardTitle>Erlang Memory Breakdown</CardTitle>
                </CardHeader>
                <CardContent className="space-y-3">
                  <div className="flex justify-between">
                    <span>Total:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_total)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Processes:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_processes)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>System:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_system)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Atoms:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_atom)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Binary:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_binary)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Code:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_code)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>ETS:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.erlang_ets)}</span>
                  </div>
                </CardContent>
              </Card>
              
              <Card>
                <CardHeader>
                  <CardTitle>System Memory</CardTitle>
                </CardHeader>
                <CardContent className="space-y-3">
                  <div className="flex justify-between">
                    <span>Total System:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.total)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Allocated:</span>
                    <span className="font-mono">{formatBytes(systemMetrics.memory.allocated)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span>Usage:</span>
                    <span className="font-mono">
                      {((systemMetrics.memory.allocated / systemMetrics.memory.total) * 100).toFixed(1)}%
                    </span>
                  </div>
                </CardContent>
              </Card>
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  )
}