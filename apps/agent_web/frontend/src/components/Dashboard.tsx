import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { RefreshCw, Search } from 'lucide-react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts'

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

  const performanceData = [
    { time: '00:00', cpu: 20, memory: 30 },
    { time: '00:05', cpu: 25, memory: 35 },
    { time: '00:10', cpu: 30, memory: 40 },
    { time: '00:15', cpu: systemMetrics.cpuUsage, memory: systemMetrics.memoryUsage },
  ]

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-4 flex-1">
          <div className="relative flex-1 max-w-md">
            <Search className="absolute left-2 top-2.5 h-4 w-4 text-muted-foreground" />
            <Input placeholder="Search agents, logs, or metrics..." className="pl-8" />
          </div>
        </div>
        <Button onClick={onRefresh}>
          <RefreshCw className="h-4 w-4 mr-2" />
          Refresh
        </Button>
      </div>

      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">System Status</CardTitle>
            <div className="h-2 w-2 bg-green-500 rounded-full animate-pulse" />
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">CPU Usage</span>
                <span className="text-sm font-medium">{systemMetrics.cpuUsage}%</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Memory</span>
                <span className="text-sm font-medium">{systemMetrics.memoryUsage}%</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm text-muted-foreground">Processes</span>
                <span className="text-sm font-medium">{systemMetrics.processCount}</span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Agent Overview</CardTitle>
            <svg
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              className="h-4 w-4 text-muted-foreground"
            >
              <path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2" />
              <circle cx="9" cy="7" r="4" />
              <path d="M22 21v-2a4 4 0 0 0-3-3.87M16 3.13a4 4 0 0 1 0 7.75" />
            </svg>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{totalAgents}</div>
            <p className="text-xs text-muted-foreground">
              {activeAgents} active, {aiAgents} AI agents
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Recent Activity</CardTitle>
            <svg
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              className="h-4 w-4 text-muted-foreground"
            >
              <path d="M22 12h-4l-3 9L9 3l-3 9H2" />
            </svg>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">+12%</div>
            <p className="text-xs text-muted-foreground">
              From last hour
            </p>
          </CardContent>
        </Card>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Performance Overview</CardTitle>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={performanceData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="time" />
              <YAxis />
              <Tooltip />
              <Line 
                type="monotone" 
                dataKey="cpu" 
                stroke="#8884d8" 
                strokeWidth={2}
                name="CPU %"
              />
              <Line 
                type="monotone" 
                dataKey="memory" 
                stroke="#82ca9d" 
                strokeWidth={2}
                name="Memory %"
              />
            </LineChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>
    </div>
  )
}