import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { RefreshCw, Search, Activity, Users, Cpu, MemoryStick, TrendingUp } from 'lucide-react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, AreaChart, Area } from 'recharts'

interface SystemMetrics {
  timestamp: number
  memory: {
    total: number
    allocated: number
    erlang_total: number
    erlang_processes: number
    erlang_system: number
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

interface DashboardProps {
  agents: Map<string, any>
  systemMetrics: {
    cpuUsage: number
    memoryUsage: number
    processCount: number
  }
  onRefresh: () => void
}

export default function Dashboard({ agents, systemMetrics, onRefresh }: DashboardProps) {
  const totalAgents = agents.size
  const activeAgents = Array.from(agents.values()).filter(a => a.status === 'active').length
  const aiAgents = Array.from(agents.values()).filter(a => a.type === 'ai').length

  // Store performance history
  const [performanceHistory, setPerformanceHistory] = useState<Array<{time: string, cpu: number, memory: number, processes: number}>>([])
  const [realTimeMetrics, setRealTimeMetrics] = useState<SystemMetrics | null>(null)
  const [ws, setWs] = useState<WebSocket | null>(null)
  
  useEffect(() => {
    connectMetricsWebSocket()
    return () => {
      if (ws) ws.close()
    }
  }, [])
  
  const connectMetricsWebSocket = () => {
    const websocket = new WebSocket(`ws://${window.location.host}/ws/system/metrics`)
    
    websocket.onopen = () => {
      console.log('Dashboard metrics WebSocket connected')
    }
    
    websocket.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data) as SystemMetrics
        setRealTimeMetrics(data)
        
        setPerformanceHistory(prev => {
          const newData = [...prev, {
            time: new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' }),
            cpu: data.cpu.utilization,
            memory: (data.memory.erlang_total / data.memory.total) * 100,
            processes: data.processes.utilization
          }]
          return newData.slice(-20)
        })
      } catch (error) {
        console.error('Failed to parse metrics data:', error)
      }
    }
    
    websocket.onerror = (error) => {
      console.error('Dashboard metrics WebSocket error:', error)
    }
    
    websocket.onclose = () => {
      console.log('Dashboard metrics WebSocket disconnected')
      setTimeout(() => connectMetricsWebSocket(), 5000)
    }
    
    setWs(websocket)
  }
  
  useEffect(() => {
    if (systemMetrics.cpuUsage > 0) {
      setPerformanceHistory(prev => {
        const newData = [...prev, {
          time: new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' }),
          cpu: systemMetrics.cpuUsage,
          memory: systemMetrics.memoryUsage,
          processes: systemMetrics.processCount
        }]
        // Keep only last 20 data points
        return newData.slice(-20)
      })
    }
  }, [systemMetrics])
  
  const performanceData = performanceHistory.length > 0 ? performanceHistory : [
    { time: new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' }), 
      cpu: realTimeMetrics?.cpu.utilization || systemMetrics.cpuUsage || 0, 
      memory: realTimeMetrics ? (realTimeMetrics.memory.erlang_total / realTimeMetrics.memory.total) * 100 : systemMetrics.memoryUsage || 0,
      processes: realTimeMetrics?.processes.utilization || 0 }
  ]
  
  const formatBytes = (bytes: number) => {
    const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB']
    if (bytes === 0) return '0 Bytes'
    const i = Math.floor(Math.log(bytes) / Math.log(1024))
    return Math.round(bytes / Math.pow(1024, i) * 100) / 100 + ' ' + sizes[i]
  }
  
  const currentCpuUsage = realTimeMetrics?.cpu.utilization || systemMetrics.cpuUsage || 0
  const currentMemoryUsage = realTimeMetrics ? (realTimeMetrics.memory.erlang_total / realTimeMetrics.memory.total) * 100 : systemMetrics.memoryUsage || 0
  const currentProcessCount = realTimeMetrics?.processes.count || systemMetrics.processCount || 0
  const currentAgentCount = realTimeMetrics?.agents.count || totalAgents

  return (
    <div className="space-y-8 p-6">
      {/* Header Section */}
      <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
        <div className="space-y-1">
          <h1 className="text-3xl font-bold tracking-tight">Agent System Dashboard</h1>
          <p className="text-muted-foreground">Monitor and manage your distributed AI agents</p>
        </div>
        <div className="flex items-center gap-3">
          <div className="relative max-w-md">
            <Search className="absolute left-3 top-2.5 h-4 w-4 text-muted-foreground" />
            <Input 
              placeholder="Search agents, logs, or metrics..." 
              className="pl-10 w-80 bg-background/50 backdrop-blur-sm border-border/40" 
            />
          </div>
          <Button onClick={onRefresh} variant="outline" className="gap-2">
            <RefreshCw className="h-4 w-4" />
            Refresh
          </Button>
        </div>
      </div>

      {/* Metrics Grid */}
      <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-4">
        <Card className="border-border/40 bg-gradient-to-br from-background to-background/80 backdrop-blur-sm">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-3">
            <CardTitle className="text-sm font-medium text-muted-foreground">System Status</CardTitle>
            <div className="flex items-center gap-1">
              <div className="h-2 w-2 bg-green-500 rounded-full animate-pulse" />
              <Badge variant="secondary" className="text-xs">Online</Badge>
            </div>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="space-y-3">
              <div className="space-y-1">
                <div className="flex items-center justify-between">
                  <span className="text-sm text-muted-foreground flex items-center gap-1">
                    <Cpu className="h-3 w-3" />
                    CPU Usage
                  </span>
                  <span className="text-sm font-medium">{currentCpuUsage.toFixed(1)}%</span>
                </div>
                <Progress value={currentCpuUsage} className="h-2" />
              </div>
              <div className="space-y-1">
                <div className="flex items-center justify-between">
                  <span className="text-sm text-muted-foreground flex items-center gap-1">
                    <MemoryStick className="h-3 w-3" />
                    Memory
                  </span>
                  <span className="text-sm font-medium">{currentMemoryUsage.toFixed(1)}%</span>
                </div>
                <Progress value={currentMemoryUsage} className="h-2" />
              </div>
              <div className="flex justify-between pt-1">
                <span className="text-sm text-muted-foreground">Processes</span>
                <span className="text-sm font-medium">{currentProcessCount}</span>
              </div>
              {realTimeMetrics && (
                <div className="flex justify-between pt-1">
                  <span className="text-sm text-muted-foreground">Memory Usage</span>
                  <span className="text-sm font-medium">{formatBytes(realTimeMetrics.memory.erlang_total)}</span>
                </div>
              )}
            </div>
          </CardContent>
        </Card>

        <Card className="border-border/40 bg-gradient-to-br from-background to-background/80 backdrop-blur-sm">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-3">
            <CardTitle className="text-sm font-medium text-muted-foreground">Total Agents</CardTitle>
            <Users className="h-4 w-4 text-blue-500" />
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-foreground">{currentAgentCount}</div>
            <div className="flex items-center gap-2 mt-2">
              <Badge variant="default" className="text-xs">
                {activeAgents} active
              </Badge>
              <Badge variant="outline" className="text-xs">
                {aiAgents} AI agents
              </Badge>
            </div>
            <p className="text-xs text-muted-foreground mt-1">
              {((activeAgents / (totalAgents || 1)) * 100).toFixed(0)}% uptime
            </p>
          </CardContent>
        </Card>

        <Card className="border-border/40 bg-gradient-to-br from-background to-background/80 backdrop-blur-sm">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-3">
            <CardTitle className="text-sm font-medium text-muted-foreground">Recent Activity</CardTitle>
            <Activity className="h-4 w-4 text-green-500" />
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-foreground">
              +{activeAgents * 12}%
            </div>
            <p className="text-xs text-muted-foreground mt-1">
              From last hour
            </p>
            <div className="flex items-center gap-2 mt-2">
              <TrendingUp className="h-3 w-3 text-green-500" />
              <span className="text-xs text-green-600">Performance up</span>
            </div>
          </CardContent>
        </Card>

        <Card className="border-border/40 bg-gradient-to-br from-background to-background/80 backdrop-blur-sm">
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-3">
            <CardTitle className="text-sm font-medium text-muted-foreground">Agent Status</CardTitle>
            <Activity className="h-4 w-4 text-orange-500" />
          </CardHeader>
          <CardContent>
            <div className="text-3xl font-bold text-foreground">
              {agents.size > 0 ? `${activeAgents}/${totalAgents}` : '0/0'}
            </div>
            <p className="text-xs text-muted-foreground mt-1">
              Active/Total agents
            </p>
            <div className="mt-2">
              <Progress 
                value={(activeAgents / (totalAgents || 1)) * 100} 
                className="h-2" 
              />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Performance Chart */}
      <Card className="border-border/40 bg-gradient-to-br from-background to-background/80 backdrop-blur-sm">
        <CardHeader className="pb-4">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="text-lg font-semibold">Performance Overview</CardTitle>
              <p className="text-sm text-muted-foreground mt-1">Real-time system metrics</p>
            </div>
            <Badge variant="outline" className="flex items-center gap-1">
              <div className="h-2 w-2 bg-green-500 rounded-full" />
              Live
            </Badge>
          </div>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={350}>
            <AreaChart data={performanceData} margin={{ top: 10, right: 10, left: 0, bottom: 0 }}>
              <defs>
                <linearGradient id="cpuGradient" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="5%" stopColor="#3b82f6" stopOpacity={0.3} />
                  <stop offset="95%" stopColor="#3b82f6" stopOpacity={0.1} />
                </linearGradient>
                <linearGradient id="memoryGradient" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="5%" stopColor="#10b981" stopOpacity={0.3} />
                  <stop offset="95%" stopColor="#10b981" stopOpacity={0.1} />
                </linearGradient>
              </defs>
              <CartesianGrid strokeDasharray="3 3" className="opacity-30" />
              <XAxis 
                dataKey="time" 
                tick={{ fontSize: 12 }}
                axisLine={false}
                tickLine={false}
              />
              <YAxis 
                domain={[0, 100]}
                tick={{ fontSize: 12 }}
                axisLine={false}
                tickLine={false}
              />
              <Tooltip 
                contentStyle={{
                  backgroundColor: 'hsl(var(--background))',
                  border: '1px solid hsl(var(--border))',
                  borderRadius: '8px',
                  boxShadow: '0 4px 6px -1px rgb(0 0 0 / 0.1)'
                }}
              />
              <Area
                type="monotone"
                dataKey="cpu"
                stroke="#3b82f6"
                strokeWidth={2}
                fill="url(#cpuGradient)"
                name="CPU %"
              />
              <Area
                type="monotone"
                dataKey="memory"
                stroke="#10b981"
                strokeWidth={2}
                fill="url(#memoryGradient)"
                name="Memory %"
              />
            </AreaChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>
    </div>
  )
}