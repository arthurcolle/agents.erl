import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Search, Download, Trash2 } from 'lucide-react'
import { cn } from '@/lib/utils'

interface LogEntry {
  id: string
  level: 'info' | 'warning' | 'error' | 'success'
  message: string
  timestamp: Date
}

export default function LogsViewer() {
  const [logs, setLogs] = useState<LogEntry[]>([])
  const [filter, setFilter] = useState<string>('all')
  const [search, setSearch] = useState('')
  const [autoScroll, setAutoScroll] = useState(true)
  const logsEndRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    // Simulate some initial logs
    const initialLogs: LogEntry[] = [
      {
        id: '1',
        level: 'info',
        message: 'System initialized',
        timestamp: new Date()
      },
      {
        id: '2',
        level: 'success',
        message: 'WebSocket connection established',
        timestamp: new Date()
      }
    ]
    setLogs(initialLogs)
  }, [])

  useEffect(() => {
    if (autoScroll) {
      logsEndRef.current?.scrollIntoView({ behavior: 'smooth' })
    }
  }, [logs, autoScroll])

  const clearLogs = () => {
    setLogs([])
  }

  const exportLogs = () => {
    const logsText = logs
      .map(log => `[${log.timestamp.toLocaleString()}] [${log.level.toUpperCase()}] ${log.message}`)
      .join('\n')
    
    const blob = new Blob([logsText], { type: 'text/plain' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `logs-${new Date().toISOString()}.txt`
    a.click()
    URL.revokeObjectURL(url)
  }

  const filteredLogs = logs.filter(log => {
    const matchesFilter = filter === 'all' || log.level === filter
    const matchesSearch = log.message.toLowerCase().includes(search.toLowerCase())
    return matchesFilter && matchesSearch
  })

  const levelColors = {
    info: 'text-blue-600 dark:text-blue-400',
    warning: 'text-yellow-600 dark:text-yellow-400',
    error: 'text-red-600 dark:text-red-400',
    success: 'text-green-600 dark:text-green-400'
  }

  const levelBadgeVariants = {
    info: 'secondary' as const,
    warning: 'outline' as const,
    error: 'destructive' as const,
    success: 'secondary' as const
  }

  return (
    <Card className="h-[600px] flex flex-col">
      <CardHeader>
        <CardTitle>System Logs</CardTitle>
      </CardHeader>
      <CardContent className="flex-1 flex flex-col">
        <div className="flex gap-2 mb-4">
          <div className="relative flex-1">
            <Search className="absolute left-2 top-2.5 h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Search logs..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              className="pl-8"
            />
          </div>
          <select
            className="px-3 py-2 border rounded-md bg-background"
            value={filter}
            onChange={(e) => setFilter(e.target.value)}
          >
            <option value="all">All Levels</option>
            <option value="info">Info</option>
            <option value="warning">Warnings</option>
            <option value="error">Errors</option>
            <option value="success">Success</option>
          </select>
          <Button variant="outline" onClick={clearLogs}>
            <Trash2 className="h-4 w-4" />
          </Button>
          <Button variant="outline" onClick={exportLogs}>
            <Download className="h-4 w-4" />
          </Button>
          <label className="flex items-center gap-2">
            <input
              type="checkbox"
              checked={autoScroll}
              onChange={(e) => setAutoScroll(e.target.checked)}
              className="rounded"
            />
            <span className="text-sm">Auto-scroll</span>
          </label>
        </div>

        <div className="flex-1 overflow-y-auto font-mono text-sm space-y-1 bg-muted/50 rounded-md p-4">
          {filteredLogs.length === 0 ? (
            <div className="text-center text-muted-foreground py-8">
              No logs to display
            </div>
          ) : (
            filteredLogs.map(log => (
              <div key={log.id} className="flex items-start gap-2">
                <span className="text-muted-foreground text-xs">
                  [{log.timestamp.toLocaleTimeString()}]
                </span>
                <Badge variant={levelBadgeVariants[log.level]} className="text-xs">
                  {log.level.toUpperCase()}
                </Badge>
                <span className={cn('flex-1', levelColors[log.level])}>
                  {log.message}
                </span>
              </div>
            ))
          )}
          <div ref={logsEndRef} />
        </div>
      </CardContent>
    </Card>
  )
}