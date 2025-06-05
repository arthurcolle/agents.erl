import { useState, useEffect } from 'react'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  Bot, 
  Circle, 
  MessageSquare, 
  Activity, 
  Zap,
  AlertTriangle,
  Moon,
  TrendingUp
} from 'lucide-react'
import { cn } from '@/lib/utils'

interface AgentPillXLProps {
  agent: {
    id: string
    name?: string
    type: string
    status?: string
    model?: string
    lastActivity?: number
    tokenUsage?: number[]
  }
  isSelected?: boolean
  isCompact?: boolean
  onClick?: () => void
  onChat?: () => void
}

export function AgentPillXL({ 
  agent, 
  isSelected, 
  isCompact = false,
  onClick, 
  onChat 
}: AgentPillXLProps) {
  const [sparklineData, setSparklineData] = useState<number[]>([])
  
  useEffect(() => {
    // Generate mini sparkline for token usage (last 15 minutes)
    if (agent.tokenUsage) {
      setSparklineData(agent.tokenUsage)
    } else {
      // Mock data for demo
      setSparklineData([2, 5, 3, 8, 12, 6, 4, 9, 15, 7])
    }
  }, [agent.tokenUsage])

  const getStatusIcon = () => {
    switch (agent.status) {
      case 'active':
        return <Circle className="h-2 w-2 fill-green-500 text-green-500" />
      case 'error':
        return <AlertTriangle className="h-2 w-2 fill-red-500 text-red-500" />
      case 'idle':
        return <Moon className="h-2 w-2 fill-yellow-500 text-yellow-500" />
      default:
        return <Circle className="h-2 w-2 fill-gray-500 text-gray-500" />
    }
  }

  const getStatusBadgeVariant = () => {
    switch (agent.status) {
      case 'active': return 'default'
      case 'error': return 'destructive'
      case 'idle': return 'secondary'
      default: return 'outline'
    }
  }

  const MiniSparkline = ({ data }: { data: number[] }) => {
    const max = Math.max(...data)
    const points = data.map((value, index) => {
      const x = (index / (data.length - 1)) * 40
      const y = 12 - (value / max) * 10
      return `${x},${y}`
    }).join(' ')

    return (
      <svg width="40" height="12" className="opacity-60">
        <polyline
          fill="none"
          stroke="currentColor"
          strokeWidth="1"
          points={points}
        />
      </svg>
    )
  }

  if (isCompact) {
    return (
      <div
        className={cn(
          "flex items-center justify-between p-1 rounded-md cursor-pointer transition-all",
          "hover:bg-accent/50 border border-transparent",
          isSelected && "bg-accent border-accent-foreground/20"
        )}
        onClick={() => {
          onClick?.()
          onChat?.()
        }}
      >
        <div className="flex items-center gap-1 min-w-0 flex-1">
          <Bot className="h-3 w-3 flex-shrink-0" />
          <span className="text-xs font-medium truncate">
            {agent.name || agent.type}
          </span>
          {getStatusIcon()}
        </div>
        <Button
          variant="ghost"
          size="icon"
          className="h-4 w-4 ml-1"
          onClick={(e) => {
            e.stopPropagation()
            onChat?.()
          }}
        >
          <MessageSquare className="h-2 w-2" />
        </Button>
      </div>
    )
  }

  return (
    <div
      className={cn(
        "flex items-center justify-between p-2 rounded-md cursor-pointer transition-all",
        "hover:bg-accent/50 border border-transparent",
        isSelected && "bg-accent border-accent-foreground/20"
      )}
      onClick={() => {
        onClick?.()
        onChat?.()
      }}
    >
      <div className="flex items-center gap-2 min-w-0 flex-1">
        <div className="flex items-center gap-1">
          <Bot className="h-4 w-4 flex-shrink-0" />
          {getStatusIcon()}
        </div>
        
        <div className="min-w-0 flex-1">
          <div className="flex items-center gap-1">
            <span className="text-sm font-medium truncate">
              {agent.name || agent.type}
            </span>
            <Badge variant={getStatusBadgeVariant()} className="text-xs px-1 py-0">
              {agent.status || 'unknown'}
            </Badge>
          </div>
          
          <div className="flex items-center gap-2 mt-1">
            <span className="text-xs text-muted-foreground truncate">
              {String(agent.id).substring(0, 8)}
            </span>
            {agent.model && (
              <span className="text-xs text-muted-foreground">
                {agent.model}
              </span>
            )}
          </div>
        </div>
      </div>

      <div className="flex items-center gap-1 ml-2">
        {/* Mini sparkline */}
        <div className="text-muted-foreground">
          <MiniSparkline data={sparklineData} />
        </div>
        
        <Button
          variant="ghost"
          size="icon"
          className="h-6 w-6"
          onClick={(e) => {
            e.stopPropagation()
            onChat?.()
          }}
        >
          <MessageSquare className="h-3 w-3" />
        </Button>
      </div>
    </div>
  )
}