import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { 
  Bot, Activity, MessageSquare, Cpu, HardDrive, 
  Zap, GitBranch, Clock, TrendingUp, Hash
} from 'lucide-react'

interface DashboardProps {
  agents: Map<string, any>
  systemMetrics: any
  onRefresh?: () => void
}

export default function DenseDashboard({ agents, systemMetrics }: DashboardProps) {
  const activeAgents = Array.from(agents.values()).filter(a => a.status === 'active' || a.status === 'idle')
  const totalMessages = Array.from(agents.values()).reduce((sum, agent) => sum + (agent.message_count || 0), 0)
  const totalTools = Array.from(agents.values()).reduce((sum, agent) => sum + (agent.tools?.length || 0), 0)
  const avgMemory = agents.size > 0 
    ? Array.from(agents.values()).reduce((sum, agent) => sum + (agent.memory || 0), 0) / agents.size
    : 0

  const metrics = [
    { icon: Bot, label: 'Agents', value: activeAgents.length, total: agents.size, color: 'text-blue-500' },
    { icon: MessageSquare, label: 'Messages', value: totalMessages, trend: '+12%', color: 'text-green-500' },
    { icon: Zap, label: 'Tools', value: totalTools, avg: Math.floor(totalTools / agents.size), color: 'text-yellow-500' },
    { icon: Cpu, label: 'CPU', value: `${systemMetrics.cpuUsage}%`, progress: systemMetrics.cpuUsage, color: 'text-purple-500' },
    { icon: HardDrive, label: 'Memory', value: `${systemMetrics.memoryUsage}%`, progress: systemMetrics.memoryUsage, color: 'text-red-500' },
    { icon: GitBranch, label: 'Schedulers', value: systemMetrics.schedulers || 0, queue: systemMetrics.runQueue || 0, color: 'text-cyan-500' }
  ]

  return (
    <div className="space-y-2">
      {/* Ultra-Compact Metrics Grid */}
      <div className="grid gap-1 grid-cols-3 lg:grid-cols-6">
        {metrics.map((metric, i) => (
          <Card key={i} className="p-2">
            <div className="flex items-center justify-between mb-1">
              <metric.icon className={`h-3 w-3 ${metric.color}`} />
              <span className="text-xs text-muted-foreground">{metric.label}</span>
            </div>
            <div className="text-sm font-bold">{metric.value}</div>
            {metric.total && (
              <div className="text-xs text-muted-foreground">of {metric.total}</div>
            )}
            {metric.trend && (
              <div className="flex items-center gap-1">
                <TrendingUp className="h-3 w-3 text-green-500" />
                <span className="text-xs text-green-500">{metric.trend}</span>
              </div>
            )}
            {metric.avg && (
              <div className="text-xs text-muted-foreground">~{metric.avg} avg</div>
            )}
            {metric.progress !== undefined && (
              <Progress value={metric.progress} className="h-1 mt-1" />
            )}
            {metric.queue !== undefined && (
              <div className="text-xs text-muted-foreground">Q: {metric.queue}</div>
            )}
          </Card>
        ))}
      </div>

      {/* Agent Activity Table */}
      <Card className="p-2">
        <div className="text-xs font-medium mb-2 flex items-center gap-2">
          <Activity className="h-3 w-3" />
          Agent Activity
        </div>
        <div className="overflow-x-auto">
          <table className="w-full text-xs">
            <thead>
              <tr className="border-b">
                <th className="text-left py-1">Agent</th>
                <th className="text-left py-1">Type</th>
                <th className="text-left py-1">Status</th>
                <th className="text-left py-1">Tools</th>
                <th className="text-left py-1">Memory</th>
                <th className="text-left py-1">Queue</th>
                <th className="text-left py-1">Uptime</th>
              </tr>
            </thead>
            <tbody>
              {Array.from(agents.values()).slice(0, 5).map(agent => (
                <tr key={agent.id} className="border-b hover:bg-gray-900/50">
                  <td className="py-1">
                    <div className="flex items-center gap-1">
                      <Bot className="h-3 w-3" />
                      <span className="truncate max-w-[100px]">{agent.name || agent.type}</span>
                    </div>
                  </td>
                  <td className="py-1">
                    <Badge variant="outline" className="py-0 px-1 text-xs">
                      {agent.type}
                    </Badge>
                  </td>
                  <td className="py-1">
                    <div className={`w-2 h-2 rounded-full ${
                      agent.status === 'active' ? 'bg-green-500' :
                      agent.status === 'idle' ? 'bg-yellow-500' : 'bg-gray-500'
                    }`} />
                  </td>
                  <td className="py-1">{agent.tools?.length || 0}</td>
                  <td className="py-1">{formatBytes(agent.memory || 0)}</td>
                  <td className="py-1">{agent.message_queue_len || 0}</td>
                  <td className="py-1">{formatUptime(agent.created_at)}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </Card>

      {/* Real-time Activity Feed */}
      <Card className="p-2">
        <div className="text-xs font-medium mb-2 flex items-center gap-2">
          <Clock className="h-3 w-3" />
          Real-time Activity
        </div>
        <div className="space-y-1 max-h-32 overflow-y-auto">
          {[
            { time: '2s ago', event: 'Agent "Research Assistant" executed jina_search', type: 'tool' },
            { time: '5s ago', event: 'New collaboration created: "Data Analysis"', type: 'collab' },
            { time: '12s ago', event: 'Quorum decision approved: "Enable caching"', type: 'quorum' },
            { time: '18s ago', event: 'Agent "Code Helper" sent message to 3 agents', type: 'message' },
            { time: '25s ago', event: 'System health check: All systems operational', type: 'system' }
          ].map((activity, i) => (
            <div key={i} className="flex items-center gap-2 text-xs">
              <span className="text-muted-foreground w-12">{activity.time}</span>
              <Badge variant="outline" className="py-0 px-1">
                {activity.type}
              </Badge>
              <span className="truncate flex-1">{activity.event}</span>
            </div>
          ))}
        </div>
      </Card>
    </div>
  )
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return bytes + 'B'
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + 'KB'
  return (bytes / (1024 * 1024)).toFixed(1) + 'MB'
}

function formatUptime(createdAt: number): string {
  const seconds = Math.floor((Date.now() - createdAt) / 1000)
  if (seconds < 60) return seconds + 's'
  if (seconds < 3600) return Math.floor(seconds / 60) + 'm'
  if (seconds < 86400) return Math.floor(seconds / 3600) + 'h'
  return Math.floor(seconds / 86400) + 'd'
}