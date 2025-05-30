import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Send, Brain, Zap, BarChart3, Code, Terminal, Eye, Lightbulb } from 'lucide-react'
import { cn } from '@/lib/utils'
import ChainOfThoughtEngine from '../services/ChainOfThoughtEngine'
import ChainOfThoughtVisualization from './ChainOfThoughtVisualization'
import InteractiveReasoningControls from './InteractiveReasoningControls'

interface Message {
  id: string
  sender: 'user' | 'agent'
  content: string
  timestamp: Date
  type?: 'text' | 'code' | 'data' | 'math'
}

interface ChatInterfaceProps {
  selectedAgent: string | null
  agents: Map<string, any>
  ws: WebSocket | null
}

// Advanced Message Renderer
function AdvancedMessageContent({ content }: { content: string }) {
  const parseContent = (text: string) => {
    const parts = []
    let currentIndex = 0
    
    // Handle code blocks
    const codeBlockRegex = /```(\w+)?\n?([\s\S]*?)```/g
    let match
    
    while ((match = codeBlockRegex.exec(text)) !== null) {
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        parts.push(...parseInlineElements(beforeText, parts.length))
      }
      
      const language = match[1] || 'text'
      const code = match[2].trim()
      
      parts.push(
        <div key={parts.length} className="my-3">
          <div className="flex items-center justify-between bg-gray-800 text-white px-3 py-1 text-xs rounded-t-md">
            <span className="font-mono">{language}</span>
            <div className="flex gap-2">
              <button 
                className="hover:bg-gray-700 p-1 rounded"
                onClick={() => navigator.clipboard.writeText(code)}
                title="Copy code"
              >
                📋
              </button>
              <button 
                className="hover:bg-gray-700 p-1 rounded"
                title="Run code"
              >
                ▶️
              </button>
            </div>
          </div>
          <pre className="bg-gray-900 text-gray-100 p-4 rounded-b-md overflow-x-auto text-sm font-mono">
            <code>{code}</code>
          </pre>
        </div>
      )
      
      currentIndex = match.index + match[0].length
    }
    
    if (currentIndex < text.length) {
      const remainingText = text.slice(currentIndex)
      parts.push(...parseInlineElements(remainingText, parts.length))
    }
    
    return parts.length > 0 ? parts : [text]
  }
  
  const parseInlineElements = (text: string, startKey: number) => {
    const parts = []
    let currentIndex = 0
    
    // Handle inline code
    const inlineCodeRegex = /`([^`]+)`/g
    let match
    
    while ((match = inlineCodeRegex.exec(text)) !== null) {
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        parts.push(...parseMath(beforeText, startKey + parts.length))
      }
      
      parts.push(
        <code 
          key={startKey + parts.length} 
          className="bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded text-sm font-mono border"
        >
          {match[1]}
        </code>
      )
      
      currentIndex = match.index + match[0].length
    }
    
    if (currentIndex < text.length) {
      const remainingText = text.slice(currentIndex)
      parts.push(...parseMath(remainingText, startKey + parts.length))
    }
    
    return parts.length > 0 ? parts : [text]
  }
  
  const parseMath = (text: string, startKey: number) => {
    const parts = []
    let currentIndex = 0
    
    // Handle LaTeX math
    const mathRegex = /\$\$?(.*?)\$\$?/g
    let match
    
    while ((match = mathRegex.exec(text)) !== null) {
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        if (beforeText.trim()) {
          parts.push(<span key={startKey + parts.length}>{beforeText}</span>)
        }
      }
      
      const isBlock = match[0].startsWith('$$')
      parts.push(
        <span 
          key={startKey + parts.length}
          className={cn(
            "font-mono border rounded px-1 py-0.5",
            isBlock ? "bg-blue-50 border-blue-200 text-blue-800 block my-2 p-3" : "bg-blue-50 border-blue-200 text-blue-800"
          )}
        >
          {match[1]}
        </span>
      )
      
      currentIndex = match.index + match[0].length
    }
    
    if (currentIndex < text.length) {
      const remainingText = text.slice(currentIndex)
      if (remainingText.trim()) {
        parts.push(<span key={startKey + parts.length}>{remainingText}</span>)
      }
    }
    
    return parts.length > 0 ? parts : [text]
  }
  
  const parsedContent = parseContent(content)
  
  return (
    <div className="text-sm whitespace-pre-wrap">
      {parsedContent}
    </div>
  )
}

// Interactive Widget Panel
function InteractivePanel({ isVisible, onToggle }: { isVisible: boolean; onToggle: () => void }) {
  const [activeWidget, setActiveWidget] = useState<string | null>(null)
  
  const widgets = [
    { id: 'calculator', name: 'Calculator', icon: '🧮' },
    { id: 'chart', name: 'Chart', icon: '📊' },
    { id: 'terminal', name: 'Terminal', icon: '💻' },
    { id: 'code', name: 'Code Editor', icon: '📝' }
  ]
  
  if (!isVisible) return null
  
  return (
    <div className="w-80 bg-gray-50 dark:bg-gray-900 border-l border-gray-200 dark:border-gray-700 p-4">
      <div className="flex items-center justify-between mb-4">
        <h3 className="font-semibold">Interactive Tools</h3>
        <Button size="sm" variant="ghost" onClick={onToggle}>×</Button>
      </div>
      
      <div className="grid grid-cols-2 gap-2 mb-4">
        {widgets.map(widget => (
          <Button
            key={widget.id}
            variant={activeWidget === widget.id ? "default" : "outline"}
            size="sm"
            onClick={() => setActiveWidget(activeWidget === widget.id ? null : widget.id)}
            className="flex flex-col h-16 p-2"
          >
            <span className="text-lg">{widget.icon}</span>
            <span className="text-xs">{widget.name}</span>
          </Button>
        ))}
      </div>
      
      {activeWidget && (
        <Card className="p-3">
          <div className="text-center text-gray-500 text-sm">
            {activeWidget === 'calculator' && <SimpleCalculator />}
            {activeWidget === 'chart' && <div>Chart widget placeholder</div>}
            {activeWidget === 'terminal' && <div>Terminal widget placeholder</div>}
            {activeWidget === 'code' && <div>Code editor widget placeholder</div>}
          </div>
        </Card>
      )}
    </div>
  )
}

// Simple Calculator Widget
function SimpleCalculator() {
  const [expression, setExpression] = useState('')
  const [result, setResult] = useState('')
  
  const calculate = () => {
    try {
      const evaluated = eval(expression.replace(/\^/g, '**'))
      setResult(String(evaluated))
    } catch {
      setResult('Error')
    }
  }
  
  const buttons = [
    ['7', '8', '9', '/'],
    ['4', '5', '6', '*'],
    ['1', '2', '3', '-'],
    ['0', '.', '=', '+']
  ]
  
  return (
    <div className="space-y-2">
      <Input
        value={expression}
        onChange={(e) => setExpression(e.target.value)}
        placeholder="Enter expression..."
        className="font-mono text-xs"
      />
      {result && (
        <div className="bg-blue-50 border border-blue-200 rounded p-2 text-sm">
          Result: {result}
        </div>
      )}
      <div className="grid grid-cols-4 gap-1">
        {buttons.flat().map(btn => (
          <Button
            key={btn}
            size="sm"
            variant="outline"
            className="h-8 text-xs"
            onClick={() => btn === '=' ? calculate() : setExpression(prev => prev + btn)}
          >
            {btn}
          </Button>
        ))}
      </div>
    </div>
  )
}

// Content Analysis Badge
function ContentAnalysisBadge({ content }: { content: string }) {
  const hasCode = content.includes('```') || content.includes('function')
  const hasMath = content.includes('$') || content.includes('equation')
  const hasData = content.includes('chart') || content.includes('data')
  
  if (!hasCode && !hasMath && !hasData) return null
  
  return (
    <div className="flex gap-1 mb-2">
      {hasCode && (
        <span className="text-xs px-2 py-1 rounded-full bg-green-100 text-green-800 flex items-center gap-1">
          <Code className="h-3 w-3" />
          Code
        </span>
      )}
      {hasMath && (
        <span className="text-xs px-2 py-1 rounded-full bg-blue-100 text-blue-800">
          Math
        </span>
      )}
      {hasData && (
        <span className="text-xs px-2 py-1 rounded-full bg-purple-100 text-purple-800 flex items-center gap-1">
          <BarChart3 className="h-3 w-3" />
          Data
        </span>
      )}
    </div>
  )
}

// Main Chat Interface
export default function SimpleAdvancedChat({ selectedAgent, agents, ws }: ChatInterfaceProps) {
  const [messages, setMessages] = useState<Message[]>([])
  const [input, setInput] = useState('')
  const [isStreaming, setIsStreaming] = useState(false)
  const [isAdvancedMode, setIsAdvancedMode] = useState(true)
  const [showInteractivePanel, setShowInteractivePanel] = useState(false)
  const [showChainOfThought, setShowChainOfThought] = useState(false)
  const [reasoningEngine] = useState(() => new ChainOfThoughtEngine())
  const [activeReasoningChain, setActiveReasoningChain] = useState<string | null>(null)
  const messagesEndRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    scrollToBottom()
  }, [messages])

  useEffect(() => {
    if (selectedAgent && ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        type: 'create_stream',
        agent_id: selectedAgent
      }))
    }
  }, [selectedAgent, ws])

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }

  const detectMessageType = (content: string): Message['type'] => {
    if (content.includes('```')) return 'code'
    if (content.includes('$') || content.includes('equation')) return 'math'
    if (content.includes('chart') || content.includes('data')) return 'data'
    return 'text'
  }

  const sendMessage = async () => {
    if (!input.trim() || !selectedAgent || !ws) return

    const userMessage: Message = {
      id: Date.now().toString(),
      sender: 'user',
      content: input,
      timestamp: new Date(),
      type: detectMessageType(input)
    }

    setMessages(prev => [...prev, userMessage])
    
    // 🧠 CHAIN OF THOUGHT: Start reasoning for complex queries
    if (shouldStartReasoning(input) && !activeReasoningChain) {
      try {
        const chainId = await reasoningEngine.startReasoningChain(
          `Respond to user query: ${input}`,
          `User context: Agent conversation about ${input}`,
          [`User asked: ${input}`, 'Need to provide helpful and accurate response']
        )
        setActiveReasoningChain(chainId)
        setShowChainOfThought(true)
        
        // 📝 Add reasoning steps for response generation
        await reasoningEngine.addThoughtStep(chainId, {
          type: 'analysis',
          content: `Analyzing user query for intent and complexity`,
          reasoning: 'Understanding what the user needs helps generate better responses',
          confidence: 0.8,
          source: 'ai'
        })
      } catch (error) {
        console.error('Failed to start reasoning chain:', error)
      }
    }
    
    setInput('')

    const agent = agents.get(selectedAgent)
    if (agent?.type === 'ai') {
      setIsStreaming(true)
      ws.send(JSON.stringify({
        type: 'stream_chat',
        message: input
      }))
    } else {
      fetch(`/api/agents/${selectedAgent}/execute`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'chat', params: { message: input } })
      })
        .then(res => res.json())
        .then(async data => {
          const agentMessage: Message = {
            id: Date.now().toString(),
            sender: 'agent',
            content: data.result,
            timestamp: new Date(),
            type: detectMessageType(data.result)
          }
          setMessages(prev => [...prev, agentMessage])
          
          // 🎯 ADD CONCLUSION TO REASONING CHAIN
          if (activeReasoningChain) {
            await reasoningEngine.addThoughtStep(activeReasoningChain, {
              type: 'conclusion',
              content: `Generated response: ${data.result.substring(0, 100)}...`,
              reasoning: 'Completed analysis and provided response to user',
              confidence: 0.9,
              source: 'ai'
            })
          }
        })
    }
  }

  // 🤔 REASONING TRIGGER: Determine when to start chain of thought
  const shouldStartReasoning = (message: string): boolean => {
    const complexityIndicators = [
      'how', 'why', 'explain', 'analyze', 'compare', 'evaluate', 
      'solve', 'optimize', 'recommend', 'strategy', 'approach'
    ]
    const wordCount = message.split(' ').length
    const hasComplexityKeywords = complexityIndicators.some(keyword => 
      message.toLowerCase().includes(keyword)
    )
    
    return wordCount > 10 || hasComplexityKeywords || message.includes('?')
  }

  // 🎯 REASONING CHAIN HANDLERS
  const handleChainStart = (chainId: string) => {
    setActiveReasoningChain(chainId)
    setShowChainOfThought(true)
  }

  const handleStepAdded = (stepId: string) => {
    console.log('New reasoning step added:', stepId)
  }

  if (!selectedAgent) {
    return (
      <Card>
        <CardContent className="flex items-center justify-center h-96">
          <p className="text-muted-foreground">
            Please select an AI agent from the sidebar to start chatting.
          </p>
        </CardContent>
      </Card>
    )
  }

  const agent = agents.get(selectedAgent)

  return (
    <div className="flex h-[600px]">
      <Card className="flex-1 flex flex-col">
        <CardHeader className="flex-row items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Brain className="h-5 w-5" />
            Chat with {agent?.type || 'Agent'}
          </CardTitle>
          <div className="flex gap-2">
            <Button
              variant={isAdvancedMode ? "default" : "outline"}
              size="sm"
              onClick={() => setIsAdvancedMode(!isAdvancedMode)}
              className="flex items-center gap-1"
            >
              <Zap className="h-3 w-3" />
              Advanced
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowInteractivePanel(!showInteractivePanel)}
              className="flex items-center gap-1"
            >
              <Terminal className="h-3 w-3" />
              Tools
            </Button>
            <Button
              variant={showChainOfThought ? "default" : "outline"}
              size="sm"
              onClick={() => setShowChainOfThought(!showChainOfThought)}
              className="flex items-center gap-1"
            >
              <Eye className="h-3 w-3" />
              Thinking
            </Button>
          </div>
        </CardHeader>
        <CardContent className="flex-1 flex flex-col">
          <div className="flex-1 overflow-y-auto space-y-4 mb-4">
            {messages.map(message => (
              <div
                key={message.id}
                className={cn(
                  "flex",
                  message.sender === 'user' ? "justify-end" : "justify-start"
                )}
              >
                <div
                  className={cn(
                    "max-w-[85%] transition-all duration-300",
                    message.sender === 'user'
                      ? "bg-primary text-primary-foreground rounded-lg p-4"
                      : "bg-muted rounded-lg p-4"
                  )}
                >
                  {isAdvancedMode && <ContentAnalysisBadge content={message.content} />}
                  
                  {isAdvancedMode ? (
                    <AdvancedMessageContent content={message.content} />
                  ) : (
                    <p className="text-sm whitespace-pre-wrap">{message.content}</p>
                  )}
                  
                  <p className="text-xs opacity-70 mt-2 flex items-center gap-2">
                    {message.timestamp.toLocaleTimeString()}
                    {message.type && message.type !== 'text' && (
                      <span className="bg-opacity-50 bg-white rounded px-1">
                        {message.type}
                      </span>
                    )}
                  </p>
                </div>
              </div>
            ))}
            <div ref={messagesEndRef} />
          </div>
          
          <div className="flex gap-2">
            <Input
              value={input}
              onChange={(e) => setInput(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && sendMessage()}
              placeholder="Type your message..."
              disabled={isStreaming}
            />
            <Button onClick={sendMessage} disabled={isStreaming || !input.trim()}>
              <Send className="h-4 w-4" />
            </Button>
          </div>
        </CardContent>
      </Card>
      
      <InteractivePanel 
        isVisible={showInteractivePanel}
        onToggle={() => setShowInteractivePanel(!showInteractivePanel)}
      />
      
      {/* 🧠 CHAIN OF THOUGHT PANEL */}
      {showChainOfThought && (
        <div className="w-96 bg-gray-50 dark:bg-gray-900 border-l border-gray-200 dark:border-gray-700 overflow-y-auto">
          <div className="p-4 space-y-4">
            <InteractiveReasoningControls
              engine={reasoningEngine}
              chainId={activeReasoningChain}
              onChainStart={handleChainStart}
              onStepAdded={handleStepAdded}
              collaborative={false}
            />
            
            {activeReasoningChain && (
              <ChainOfThoughtVisualization
                engine={reasoningEngine}
                chainId={activeReasoningChain}
                onStepClick={(stepId) => console.log('Step clicked:', stepId)}
                onBranchCreate={(parentStepId, approach) => 
                  reasoningEngine.createReasoningBranch(activeReasoningChain, parentStepId, approach)
                }
                collaborative={false}
              />
            )}
          </div>
        </div>
      )}
    </div>
  )
}