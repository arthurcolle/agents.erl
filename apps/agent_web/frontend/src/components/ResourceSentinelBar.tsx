import { useState, useEffect } from 'react'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  Cpu, 
  MemoryStick, 
  Activity, 
  Clock,
  TrendingUp,
  TrendingDown,
  Minus
} from 'lucide-react'
import { cn } from '@/lib/utils'

interface SystemMetrics {
  cpuUsage: number
  memoryUsage: number
  qps: number
  pendingTasks: number
  timestamp: number
}

interface ResourceSentinelBarProps {
  metrics: SystemMetrics
  onOpenMonitor: () => void
  className?: string
}

export function ResourceSentinelBar({ metrics, onOpenMonitor, className }: ResourceSentinelBarProps) {
  const [previousMetrics, setPreviousMetrics] = useState<SystemMetrics | null>(null)
  const [trends, setTrends] = useState({
    cpu: 'stable' as 'up' | 'down' | 'stable',
    memory: 'stable' as 'up' | 'down' | 'stable',
    qps: 'stable' as 'up' | 'down' | 'stable'
  })

  useEffect(() => {
    if (previousMetrics) {
      setTrends({
        cpu: metrics.cpuUsage > previousMetrics.cpuUsage + 2 ? 'up' : 
             metrics.cpuUsage < previousMetrics.cpuUsage - 2 ? 'down' : 'stable',
        memory: metrics.memoryUsage > previousMetrics.memoryUsage + 2 ? 'up' : 
                metrics.memoryUsage < previousMetrics.memoryUsage - 2 ? 'down' : 'stable',
        qps: metrics.qps > previousMetrics.qps + 1 ? 'up' : 
             metrics.qps < previousMetrics.qps - 1 ? 'down' : 'stable'
      })
    }
    setPreviousMetrics(metrics)
  }, [metrics, previousMetrics])

  const getThresholdColor = (value: number, type: 'cpu' | 'memory' | 'qps') => {
    switch (type) {
      case 'cpu':
      case 'memory':
        if (value >= 90) return 'text-red-500'
        if (value >= 70) return 'text-yellow-500'
        return 'text-green-500'
      case 'qps':
        if (value >= 100) return 'text-red-500'
        if (value >= 50) return 'text-yellow-500'
        return 'text-green-500'
      default:
        return 'text-muted-foreground'
    }
  }

  const getTrendIcon = (trend: 'up' | 'down' | 'stable') => {
    switch (trend) {
      case 'up': return <TrendingUp className="h-2 w-2" />
      case 'down': return <TrendingDown className="h-2 w-2" />
      default: return <Minus className="h-2 w-2" />
    }
  }

  const MicroChart = ({ value, max = 100, color }: { value: number, max?: number, color: string }) => {
    const percentage = Math.min((value / max) * 100, 100)
    
    return (
      <div className="w-8 h-3 bg-muted rounded-sm overflow-hidden">
        <div 
          className={cn("h-full transition-all duration-300", color)}
          style={{ width: `${percentage}%` }}
        />
      </div>
    )
  }

  return (
    <Card 
      className={cn(
        "fixed top-2 right-2 z-50 p-2 cursor-pointer hover:shadow-lg transition-all",
        "bg-background/95 backdrop-blur-sm border-2",
        className
      )}
      onClick={onOpenMonitor}
    >
      <div className="flex items-center gap-3">
        {/* CPU */}
        <div className="flex items-center gap-1">
          <Cpu className="h-3 w-3 text-muted-foreground" />
          <div className="flex flex-col items-center gap-0.5">
            <div className="flex items-center gap-1">
              <span className={cn("text-xs font-mono", getThresholdColor(metrics.cpuUsage, 'cpu'))}>
                {metrics.cpuUsage}%
              </span>
              <div className={getThresholdColor(metrics.cpuUsage, 'cpu')}>
                {getTrendIcon(trends.cpu)}
              </div>
            </div>
            <MicroChart 
              value={metrics.cpuUsage} 
              color={getThresholdColor(metrics.cpuUsage, 'cpu').replace('text-', 'bg-')}
            />
          </div>
        </div>

        {/* Memory */}
        <div className="flex items-center gap-1">
          <MemoryStick className="h-3 w-3 text-muted-foreground" />
          <div className="flex flex-col items-center gap-0.5">
            <div className="flex items-center gap-1">
              <span className={cn("text-xs font-mono", getThresholdColor(metrics.memoryUsage, 'memory'))}>
                {metrics.memoryUsage}%
              </span>
              <div className={getThresholdColor(metrics.memoryUsage, 'memory')}>
                {getTrendIcon(trends.memory)}
              </div>
            </div>
            <MicroChart 
              value={metrics.memoryUsage} 
              color={getThresholdColor(metrics.memoryUsage, 'memory').replace('text-', 'bg-')}
            />
          </div>
        </div>

        {/* QPS */}
        <div className="flex items-center gap-1">
          <Activity className="h-3 w-3 text-muted-foreground" />
          <div className="flex flex-col items-center gap-0.5">
            <div className="flex items-center gap-1">
              <span className={cn("text-xs font-mono", getThresholdColor(metrics.qps, 'qps'))}>
                {metrics.qps}
              </span>
              <div className={getThresholdColor(metrics.qps, 'qps')}>
                {getTrendIcon(trends.qps)}
              </div>
            </div>
            <MicroChart 
              value={metrics.qps} 
              max={200}
              color={getThresholdColor(metrics.qps, 'qps').replace('text-', 'bg-')}
            />
          </div>
        </div>

        {/* Pending Tasks */}
        <div className="flex items-center gap-1">
          <Clock className="h-3 w-3 text-muted-foreground" />
          <Badge variant={metrics.pendingTasks > 10 ? 'destructive' : 'secondary'} className="text-xs px-1 py-0">
            {metrics.pendingTasks}
          </Badge>
        </div>
      </div>
    </Card>
  )
}