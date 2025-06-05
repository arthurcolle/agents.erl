import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Calendar, TrendingUp, DollarSign } from 'lucide-react'
import { cn } from '@/lib/utils'

interface TokenUsageData {
  date: string
  tokens: number
  cost: number
  requests: number
}

interface TokenHeatmapProps {
  data?: TokenUsageData[]
  timeRange?: '7d' | '30d' | '90d'
  onTimeRangeChange?: (range: '7d' | '30d' | '90d') => void
  className?: string
}

export function TokenHeatmap({ 
  data = [], 
  timeRange = '30d',
  onTimeRangeChange,
  className 
}: TokenHeatmapProps) {
  const [heatmapData, setHeatmapData] = useState<TokenUsageData[]>([])
  const [selectedCell, setSelectedCell] = useState<TokenUsageData | null>(null)

  useEffect(() => {
    // Generate mock data if none provided
    if (data.length === 0) {
      generateMockData()
    } else {
      setHeatmapData(data)
    }
  }, [data, timeRange])

  const generateMockData = () => {
    const days = timeRange === '7d' ? 7 : timeRange === '30d' ? 30 : 90
    const mockData: TokenUsageData[] = []
    
    for (let i = days - 1; i >= 0; i--) {
      const date = new Date()
      date.setDate(date.getDate() - i)
      
      const baseTokens = Math.floor(Math.random() * 10000) + 1000
      const tokens = baseTokens + Math.floor(Math.sin(i * 0.1) * 2000)
      
      mockData.push({
        date: date.toISOString().split('T')[0],
        tokens,
        cost: tokens * 0.002, // $0.002 per 1K tokens
        requests: Math.floor(tokens / 500) + Math.floor(Math.random() * 20)
      })
    }
    
    setHeatmapData(mockData)
  }

  const getIntensityColor = (tokens: number, maxTokens: number) => {
    const intensity = tokens / maxTokens
    
    if (intensity === 0) return 'bg-muted/20'
    if (intensity < 0.2) return 'bg-green-200 dark:bg-green-900/30'
    if (intensity < 0.4) return 'bg-green-300 dark:bg-green-800/50'
    if (intensity < 0.6) return 'bg-yellow-300 dark:bg-yellow-800/50'
    if (intensity < 0.8) return 'bg-orange-300 dark:bg-orange-800/50'
    return 'bg-red-400 dark:bg-red-800/70'
  }

  const maxTokens = Math.max(...heatmapData.map(d => d.tokens))
  const totalTokens = heatmapData.reduce((sum, d) => sum + d.tokens, 0)
  const totalCost = heatmapData.reduce((sum, d) => sum + d.cost, 0)
  const avgDaily = totalTokens / heatmapData.length

  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr)
    return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' })
  }

  const getWeeksData = () => {
    const weeks: TokenUsageData[][] = []
    for (let i = 0; i < heatmapData.length; i += 7) {
      weeks.push(heatmapData.slice(i, i + 7))
    }
    return weeks
  }

  const weeks = getWeeksData()

  return (
    <Card className={cn("", className)}>
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <CardTitle className="text-base flex items-center gap-2">
            <Calendar className="h-4 w-4" />
            Token Usage Heatmap
          </CardTitle>
          <div className="flex gap-1">
            {(['7d', '30d', '90d'] as const).map((range) => (
              <Button
                key={range}
                variant={timeRange === range ? 'default' : 'outline'}
                size="sm"
                className="text-xs px-2 py-1"
                onClick={() => onTimeRangeChange?.(range)}
              >
                {range}
              </Button>
            ))}
          </div>
        </div>
        
        <div className="flex items-center gap-4 text-sm text-muted-foreground">
          <div className="flex items-center gap-1">
            <TrendingUp className="h-3 w-3" />
            <span>{totalTokens.toLocaleString()} tokens</span>
          </div>
          <div className="flex items-center gap-1">
            <DollarSign className="h-3 w-3" />
            <span>${totalCost.toFixed(2)}</span>
          </div>
          <div>
            <span>Avg: {Math.round(avgDaily).toLocaleString()}/day</span>
          </div>
        </div>
      </CardHeader>
      
      <CardContent className="pt-0">
        <div className="space-y-1">
          {weeks.map((week, weekIndex) => (
            <div key={weekIndex} className="flex gap-1">
              {week.map((day) => (
                <div
                  key={day.date}
                  className={cn(
                    "w-3 h-3 rounded-sm cursor-pointer transition-all hover:scale-110 border",
                    getIntensityColor(day.tokens, maxTokens),
                    selectedCell?.date === day.date && "ring-2 ring-primary"
                  )}
                  title={`${formatDate(day.date)}: ${day.tokens.toLocaleString()} tokens, $${day.cost.toFixed(2)}`}
                  onClick={() => setSelectedCell(selectedCell?.date === day.date ? null : day)}
                />
              ))}
            </div>
          ))}
        </div>

        {/* Legend */}
        <div className="flex items-center justify-between mt-3 pt-3 border-t">
          <div className="flex items-center gap-2 text-xs text-muted-foreground">
            <span>Less</span>
            <div className="flex gap-0.5">
              <div className="w-2 h-2 rounded-sm bg-muted/20" />
              <div className="w-2 h-2 rounded-sm bg-green-200 dark:bg-green-900/30" />
              <div className="w-2 h-2 rounded-sm bg-green-300 dark:bg-green-800/50" />
              <div className="w-2 h-2 rounded-sm bg-yellow-300 dark:bg-yellow-800/50" />
              <div className="w-2 h-2 rounded-sm bg-orange-300 dark:bg-orange-800/50" />
              <div className="w-2 h-2 rounded-sm bg-red-400 dark:bg-red-800/70" />
            </div>
            <span>More</span>
          </div>
          
          {selectedCell && (
            <div className="flex items-center gap-2">
              <Badge variant="outline" className="text-xs">
                {formatDate(selectedCell.date)}
              </Badge>
              <span className="text-xs text-muted-foreground">
                {selectedCell.tokens.toLocaleString()} tokens • ${selectedCell.cost.toFixed(2)}
              </span>
            </div>
          )}
        </div>
      </CardContent>
    </Card>
  )
}