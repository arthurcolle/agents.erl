import React, { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { ScrollArea } from '@/components/ui/scroll-area'
import { 
  Terminal, 
  Send, 
  X, 
  Copy, 
  Download, 
  History,
  HelpCircle,
  ChevronRight
} from 'lucide-react'

interface CLIEntry {
  id: string
  timestamp: Date
  command: string
  result?: any
  error?: string
  loading?: boolean
}

interface ClaudeCLIProps {
  className?: string
}

export default function ClaudeCLI({ className = '' }: ClaudeCLIProps) {
  const [entries, setEntries] = useState<CLIEntry[]>([])
  const [currentCommand, setCurrentCommand] = useState('')
  const [history, setHistory] = useState<string[]>([])
  const [historyIndex, setHistoryIndex] = useState(-1)
  const [isLoading, setIsLoading] = useState(false)
  const [suggestions, setSuggestions] = useState<string[]>([])
  const inputRef = useRef<HTMLInputElement>(null)
  const scrollRef = useRef<HTMLDivElement>(null)

  // Common Claude Code commands for autocomplete
  const commonCommands = [
    'claude -p "Your prompt here"',
    'claude --continue',
    'claude --resume',
    'claude --output-format json',
    'claude --help',
    'claude --version',
    'system status',
    'system metrics',
    'list agents',
    'help'
  ]

  useEffect(() => {
    // Auto-scroll to bottom when new entries are added
    if (scrollRef.current) {
      scrollRef.current.scrollTop = scrollRef.current.scrollHeight
    }
  }, [entries])

  useEffect(() => {
    // Focus input on component mount
    if (inputRef.current) {
      inputRef.current.focus()
    }
  }, [])

  const handleCommandSubmit = async (command?: string) => {
    const cmdToExecute = command || currentCommand.trim()
    if (!cmdToExecute) return

    const entryId = Date.now().toString()
    const newEntry: CLIEntry = {
      id: entryId,
      timestamp: new Date(),
      command: cmdToExecute,
      loading: true
    }

    setEntries(prev => [...prev, newEntry])
    setHistory(prev => {
      const newHistory = [cmdToExecute, ...prev.filter(cmd => cmd !== cmdToExecute)]
      return newHistory.slice(0, 50) // Keep last 50 commands
    })
    setCurrentCommand('')
    setHistoryIndex(-1)
    setIsLoading(true)

    try {
      const response = await fetch('/api/claude-cli', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          command: cmdToExecute,
          options: {
            output_format: 'json'
          }
        })
      })

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`)
      }

      const text = await response.text()
      
      // Try to parse as JSON, but handle plain text responses
      let result
      try {
        result = JSON.parse(text)
      } catch (parseError) {
        // If it's not JSON, treat as plain text output
        result = {
          success: true,
          result: {
            output: text,
            format: 'text'
          }
        }
      }

      setEntries(prev => prev.map(entry => 
        entry.id === entryId 
          ? {
              ...entry,
              loading: false,
              result: result.success ? result.result : undefined,
              error: result.success ? undefined : (result.error || result.message || 'Unknown error')
            }
          : entry
      ))
    } catch (error) {
      console.error('Claude CLI Error:', error)
      setEntries(prev => prev.map(entry => 
        entry.id === entryId 
          ? {
              ...entry,
              loading: false,
              error: `Network error: ${error instanceof Error ? error.message : 'Unknown error'}`
            }
          : entry
      ))
    } finally {
      setIsLoading(false)
    }
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter') {
      e.preventDefault()
      handleCommandSubmit()
    } else if (e.key === 'ArrowUp') {
      e.preventDefault()
      if (historyIndex < history.length - 1) {
        const newIndex = historyIndex + 1
        setHistoryIndex(newIndex)
        setCurrentCommand(history[newIndex])
      }
    } else if (e.key === 'ArrowDown') {
      e.preventDefault()
      if (historyIndex > 0) {
        const newIndex = historyIndex - 1
        setHistoryIndex(newIndex)
        setCurrentCommand(history[newIndex])
      } else if (historyIndex === 0) {
        setHistoryIndex(-1)
        setCurrentCommand('')
      }
    } else if (e.key === 'Tab') {
      e.preventDefault()
      // Auto-complete logic
      const matchingSuggestions = commonCommands.filter(cmd => 
        cmd.toLowerCase().startsWith(currentCommand.toLowerCase())
      )
      if (matchingSuggestions.length === 1) {
        setCurrentCommand(matchingSuggestions[0])
      } else if (matchingSuggestions.length > 1) {
        setSuggestions(matchingSuggestions)
      }
    }
  }

  const clearHistory = () => {
    setEntries([])
    setSuggestions([])
  }

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
  }

  const exportHistory = () => {
    const exportData = entries.map(entry => ({
      timestamp: entry.timestamp.toISOString(),
      command: entry.command,
      result: entry.result,
      error: entry.error
    }))
    
    const blob = new Blob([JSON.stringify(exportData, null, 2)], {
      type: 'application/json'
    })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `claude-cli-history-${new Date().toISOString().split('T')[0]}.json`
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    URL.revokeObjectURL(url)
  }

  const showHelp = () => {
    handleCommandSubmit('help')
  }

  const formatResult = (result: any) => {
    if (typeof result === 'string') {
      return result
    }
    return JSON.stringify(result, null, 2)
  }

  const renderEntry = (entry: CLIEntry) => (
    <div key={entry.id} className="mb-4 font-mono text-sm">
      {/* Command line */}
      <div className="flex items-center gap-2 mb-2">
        <span className="text-green-500">$</span>
        <span className="text-blue-400">{entry.command}</span>
        <span className="text-gray-500 text-xs ml-auto">
          {entry.timestamp.toLocaleTimeString()}
        </span>
        <Button
          variant="ghost"
          size="icon"
          className="h-6 w-6"
          onClick={() => copyToClipboard(entry.command)}
        >
          <Copy className="h-3 w-3" />
        </Button>
      </div>

      {/* Result or error */}
      {entry.loading && (
        <div className="text-yellow-500 ml-4">
          <div className="flex items-center gap-2">
            <div className="animate-spin h-4 w-4 border-2 border-yellow-500 border-t-transparent rounded-full" />
            Executing...
          </div>
        </div>
      )}

      {entry.error && (
        <div className="text-red-400 ml-4 whitespace-pre-wrap">
          Error: {entry.error}
        </div>
      )}

      {entry.result && (
        <div className="ml-4">
          <pre className="text-gray-300 whitespace-pre-wrap text-xs overflow-x-auto">
            {formatResult(entry.result)}
          </pre>
        </div>
      )}
    </div>
  )

  return (
    <Card className={`h-full flex flex-col ${className}`}>
      <CardHeader className="flex-shrink-0 pb-3">
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Terminal className="h-5 w-5" />
            Claude CLI
            <Badge variant="outline" className="text-xs">
              Interactive
            </Badge>
          </CardTitle>
          <div className="flex items-center gap-2">
            <Button
              variant="ghost"
              size="sm"
              onClick={showHelp}
              className="gap-1"
            >
              <HelpCircle className="h-4 w-4" />
              Help
            </Button>
            <Button
              variant="ghost"
              size="sm"
              onClick={exportHistory}
              className="gap-1"
              disabled={entries.length === 0}
            >
              <Download className="h-4 w-4" />
              Export
            </Button>
            <Button
              variant="ghost"
              size="sm"
              onClick={clearHistory}
              className="gap-1"
              disabled={entries.length === 0}
            >
              <X className="h-4 w-4" />
              Clear
            </Button>
          </div>
        </div>
      </CardHeader>

      <CardContent className="flex-1 flex flex-col p-4 overflow-hidden">
        {/* Terminal output */}
        <ScrollArea className="flex-1 mb-4" ref={scrollRef}>
          <div className="min-h-full bg-gray-900 rounded-lg p-4 text-gray-100">
            {entries.length === 0 && (
              <div className="text-gray-500 text-center py-8">
                <Terminal className="h-12 w-12 mx-auto mb-4 opacity-50" />
                <p>Welcome to Claude CLI</p>
                <p className="text-sm mt-2">
                  Type commands to interact with Claude Code directly from the web interface
                </p>
                <p className="text-xs mt-1 text-blue-400">
                  Try: claude -p "Hello, Claude!"
                </p>
              </div>
            )}
            
            {entries.map(renderEntry)}
            
            {/* Show suggestions */}
            {suggestions.length > 0 && (
              <div className="mt-4 p-2 bg-gray-800 rounded">
                <div className="text-xs text-gray-400 mb-2">Suggestions:</div>
                {suggestions.slice(0, 5).map((suggestion, index) => (
                  <button
                    key={index}
                    className="block w-full text-left text-xs text-blue-400 hover:text-blue-300 py-1"
                    onClick={() => {
                      setCurrentCommand(suggestion)
                      setSuggestions([])
                      inputRef.current?.focus()
                    }}
                  >
                    {suggestion}
                  </button>
                ))}
              </div>
            )}
          </div>
        </ScrollArea>

        {/* Input area */}
        <div className="flex-shrink-0">
          <div className="flex items-center gap-2 p-3 bg-gray-900 rounded-lg">
            <ChevronRight className="h-4 w-4 text-green-500" />
            <Input
              ref={inputRef}
              value={currentCommand}
              onChange={(e) => {
                setCurrentCommand(e.target.value)
                setSuggestions([])
              }}
              onKeyDown={handleKeyDown}
              placeholder="Enter Claude Code command..."
              className="flex-1 bg-transparent border-none focus:outline-none text-gray-100"
              disabled={isLoading}
            />
            <Button
              size="sm"
              onClick={() => handleCommandSubmit()}
              disabled={!currentCommand.trim() || isLoading}
              className="gap-1"
            >
              <Send className="h-4 w-4" />
              Execute
            </Button>
          </div>
          
          {/* Quick commands */}
          <div className="flex flex-wrap gap-2 mt-2">
            {['system status', 'list agents', 'claude --help'].map((cmd) => (
              <Button
                key={cmd}
                variant="outline"
                size="sm"
                className="text-xs"
                onClick={() => handleCommandSubmit(cmd)}
                disabled={isLoading}
              >
                {cmd}
              </Button>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  )
}