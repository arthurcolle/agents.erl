import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'

interface MonitoringPanelProps {
  agents: Map<string, any>
  systemMetrics: {
    cpuUsage: number
    memoryUsage: number
    processCount: number
  }
}

export default function MonitoringPanel({ agents, systemMetrics }: MonitoringPanelProps) {
  return (
    <div className="space-y-6">
      <div className="grid gap-4 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle>System Metrics</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div>
                <div className="flex justify-between mb-1">
                  <span className="text-sm font-medium">CPU Usage</span>
                  <span className="text-sm">{systemMetrics.cpuUsage}%</span>
                </div>
                <div className="w-full bg-secondary rounded-full h-2">
                  <div 
                    className="bg-primary h-2 rounded-full transition-all"
                    style={{ width: `${systemMetrics.cpuUsage}%` }}
                  />
                </div>
              </div>
              
              <div>
                <div className="flex justify-between mb-1">
                  <span className="text-sm font-medium">Memory Usage</span>
                  <span className="text-sm">{systemMetrics.memoryUsage}%</span>
                </div>
                <div className="w-full bg-secondary rounded-full h-2">
                  <div 
                    className="bg-primary h-2 rounded-full transition-all"
                    style={{ width: `${systemMetrics.memoryUsage}%` }}
                  />
                </div>
              </div>
              
              <div className="pt-2 border-t">
                <div className="flex justify-between">
                  <span className="text-sm font-medium">Active Processes</span>
                  <span className="text-sm">{systemMetrics.processCount}</span>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Agent Statistics</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm">Total Agents</span>
                <Badge>{agents.size}</Badge>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">AI Agents</span>
                <Badge variant="secondary">
                  {Array.from(agents.values()).filter(a => a.type === 'ai').length}
                </Badge>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Simple Agents</span>
                <Badge variant="secondary">
                  {Array.from(agents.values()).filter(a => a.type === 'simple').length}
                </Badge>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Agent Details</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            {Array.from(agents.values()).map(agent => (
              <div key={agent.id} className="border rounded-lg p-4">
                <div className="flex items-center justify-between mb-2">
                  <h4 className="font-medium">{agent.type} Agent</h4>
                  <Badge variant="outline">{String(agent.id).substring(0, 8)}</Badge>
                </div>
                <div className="grid grid-cols-3 gap-4 text-sm">
                  <div>
                    <span className="text-muted-foreground">Status:</span>
                    <Badge className="ml-2" variant="secondary">Active</Badge>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Memory:</span>
                    <span className="ml-2">-- MB</span>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Messages:</span>
                    <span className="ml-2">0</span>
                  </div>
                </div>
                <div className="mt-3 flex gap-2">
                  <Button size="sm" variant="outline">View Details</Button>
                  <Button size="sm" variant="outline">Restart</Button>
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </div>
  )
}