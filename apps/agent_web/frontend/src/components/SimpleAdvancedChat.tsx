import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Send, Brain, Zap, BarChart3, Code, Terminal, Eye, Lightbulb, FileText, Calculator, Image, Globe, Paperclip, X, Download, Save, Trash2 } from 'lucide-react'
import { cn } from '@/lib/utils'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import remarkMath from 'remark-math'
import rehypeHighlight from 'rehype-highlight'
import rehypeKatex from 'rehype-katex'
import 'katex/dist/katex.min.css'
import 'highlight.js/styles/github-dark.css'
import ChainOfThoughtEngine from '../services/ChainOfThoughtEngine'
import ChainOfThoughtVisualization from './ChainOfThoughtVisualization'
import InteractiveReasoningControls from './InteractiveReasoningControls'
import { sanitizeHtml } from '@/utils/sanitize'

interface Message {
  id: string
  sender: 'user' | 'agent'
  content: string
  timestamp: Date
  type?: 'text' | 'markdown' | 'code' | 'data' | 'math' | 'html' | 'latex' | 'json'
  format?: 'auto' | 'plain' | 'markdown' | 'html' | 'latex' | 'json'
  images?: string[]
  attachments?: Array<{
    name: string
    url: string
    type: string
    size: number
  }>
  metadata?: {
    edited?: boolean
    editedAt?: Date
    conversationId?: string
  }
}

interface ChatInterfaceProps {
  selectedAgent: string | null
  agents: Map<string, any>
  ws: WebSocket | null
}

// Content Format Detection
function detectContentFormat(content: string): Message['format'] {
  // HTML detection
  if (/<\/?[a-z][\s\S]*>/i.test(content)) {
    return 'html'
  }
  
  // LaTeX detection
  if (/\\(begin|end)\{|\\[a-zA-Z]+\{|\$\$.*\$\$/.test(content)) {
    return 'latex'
  }
  
  // JSON detection
  if (/^\s*[\[\{]/.test(content) && /[\]\}]\s*$/.test(content)) {
    try {
      JSON.parse(content)
      return 'json'
    } catch {
      // Not valid JSON
    }
  }
  
  // Markdown detection
  if (/^#{1,6}\s|```|\*\*|__|\[.*\]\(.*\)|^\s*[-*+]\s|^\s*\d+\.\s/m.test(content)) {
    return 'markdown'
  }
  
  return 'plain'
}

// Advanced Message Renderer
function AdvancedMessageContent({ content, format, isUser = false }: { content: string; format?: Message['format']; isUser?: boolean }) {
  const detectedFormat = format || detectContentFormat(content)
  
  const renderPlainText = () => (
    <div className="text-sm whitespace-pre-wrap">
      {content}
    </div>
  )
  
  const renderJSON = () => {
    try {
      const parsed = JSON.parse(content)
      return (
        <div className="text-sm">
          <div className="flex items-center gap-2 mb-2">
            <Code className={cn("h-4 w-4", isUser ? "text-blue-200" : "text-green-600")} />
            <span className={cn("text-xs font-semibold", isUser ? "text-blue-200" : "text-green-600")}>JSON</span>
          </div>
          <pre className={cn(
            "p-4 rounded overflow-x-auto text-sm font-mono",
            isUser ? "bg-blue-800 text-blue-100" : "bg-gray-900 text-gray-100"
          )}>
            <code>{JSON.stringify(parsed, null, 2)}</code>
          </pre>
        </div>
      )
    } catch {
      return renderPlainText()
    }
  }
  
  const renderHTML = () => (
    <div className="text-sm">
      <div className="flex items-center gap-2 mb-2">
        <Globe className={cn("h-4 w-4", isUser ? "text-blue-200" : "text-orange-600")} />
        <span className={cn("text-xs font-semibold", isUser ? "text-blue-200" : "text-orange-600")}>HTML</span>
      </div>
      <div 
        className={cn(
          "prose prose-sm max-w-none",
          isUser ? "prose-invert" : "prose-slate"
        )}
        dangerouslySetInnerHTML={{ __html: sanitizeHtml(content) }}
      />
    </div>
  )
  
  const renderLatex = () => (
    <div className="text-sm">
      <div className="flex items-center gap-2 mb-2">
        <Calculator className={cn("h-4 w-4", isUser ? "text-blue-200" : "text-blue-600")} />
        <span className={cn("text-xs font-semibold", isUser ? "text-blue-200" : "text-blue-600")}>LaTeX</span>
      </div>
      <div className={cn(
        "prose prose-sm max-w-none",
        isUser ? "prose-invert" : "prose-slate"
      )}>
        <ReactMarkdown
          remarkPlugins={[remarkMath]}
          rehypePlugins={[rehypeKatex]}
        >
          {content}
        </ReactMarkdown>
      </div>
    </div>
  )
  
  const renderMarkdown = () => (
    <div className="text-sm">
      <div className={cn(
        "prose prose-sm max-w-none",
        isUser ? "prose-invert" : "prose-slate"
      )}>
        <ReactMarkdown
          remarkPlugins={[remarkGfm, remarkMath]}
          rehypePlugins={[rehypeHighlight, rehypeKatex]}
          components={{
            code(props) {
              const {children, className, ...rest} = props
              const match = /language-(\w+)/.exec(className || '')
              const isInline = !className || !match
              
              return isInline ? (
                <code className={cn(className, isUser && "bg-blue-700 text-blue-100")} {...rest}>
                  {children}
                </code>
              ) : (
                <div className="relative">
                  <div className={cn(
                    "flex items-center justify-between px-3 py-1 text-xs rounded-t-md",
                    isUser ? "bg-blue-800 text-white" : "bg-gray-800 text-white"
                  )}>
                    <span className="font-mono">{match[1]}</span>
                    <button 
                      className={cn("p-1 rounded", isUser ? "hover:bg-blue-700" : "hover:bg-gray-700")}
                      onClick={() => navigator.clipboard.writeText(String(children))}
                      title="Copy code"
                    >
                      📋
                    </button>
                  </div>
                  <code className={cn(className, isUser && "bg-blue-900 text-blue-100")} {...rest}>
                    {children}
                  </code>
                </div>
              )
            }
          }}
        >
          {content}
        </ReactMarkdown>
      </div>
    </div>
  )
  
  switch (detectedFormat) {
    case 'html':
      return renderHTML()
    case 'latex':
      return renderLatex()
    case 'json':
      return renderJSON()
    case 'markdown':
      return renderMarkdown()
    case 'plain':
      return renderPlainText()
    default:
      return renderMarkdown() // Default to markdown for best compatibility
  }
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
      // Safe math expression evaluation without eval()
      const safeExpression = expression.replace(/[^0-9+\-*/().\s]/g, '')
      if (safeExpression !== expression) {
        setResult('Invalid expression')
        return
      }
      
      // Use Function constructor as safer alternative (still sandboxed)
      const mathFunction = new Function('return ' + safeExpression.replace(/\^/g, '**'))
      const evaluated = mathFunction()
      
      if (!isFinite(evaluated)) {
        setResult('Invalid result')
      } else {
        setResult(String(evaluated))
      }
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
function ContentAnalysisBadge({ content, format }: { content: string; format?: Message['format'] }) {
  const detectedFormat = format || detectContentFormat(content)
  const hasCode = content.includes('```') || content.includes('function')
  const hasMath = content.includes('$') || content.includes('equation') || content.includes('\\[')
  const hasData = content.includes('chart') || content.includes('data')
  const hasHTML = detectedFormat === 'html'
  const hasJSON = detectedFormat === 'json'
  const hasLatex = detectedFormat === 'latex'
  const hasMarkdown = detectedFormat === 'markdown'
  
  if (!hasCode && !hasMath && !hasData && !hasHTML && !hasJSON && !hasLatex && !hasMarkdown) return null
  
  return (
    <div className="flex gap-1 mb-2 flex-wrap">
      {hasMarkdown && (
        <span className="text-xs px-2 py-1 rounded-full bg-indigo-100 text-indigo-800 flex items-center gap-1">
          <FileText className="h-3 w-3" />
          Markdown
        </span>
      )}
      {hasHTML && (
        <span className="text-xs px-2 py-1 rounded-full bg-orange-100 text-orange-800 flex items-center gap-1">
          <Globe className="h-3 w-3" />
          HTML
        </span>
      )}
      {hasJSON && (
        <span className="text-xs px-2 py-1 rounded-full bg-green-100 text-green-800 flex items-center gap-1">
          <Code className="h-3 w-3" />
          JSON
        </span>
      )}
      {hasLatex && (
        <span className="text-xs px-2 py-1 rounded-full bg-blue-100 text-blue-800 flex items-center gap-1">
          <Calculator className="h-3 w-3" />
          LaTeX
        </span>
      )}
      {hasCode && (
        <span className="text-xs px-2 py-1 rounded-full bg-green-100 text-green-800 flex items-center gap-1">
          <Code className="h-3 w-3" />
          Code
        </span>
      )}
      {hasMath && (
        <span className="text-xs px-2 py-1 rounded-full bg-blue-100 text-blue-800 flex items-center gap-1">
          <Calculator className="h-3 w-3" />
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
  const [currentStreamingMessage, setCurrentStreamingMessage] = useState<string>('')
  const [streamingMessageId, setStreamingMessageId] = useState<string | null>(null)
  const [images, setImages] = useState<File[]>([])
  const [isUploading, setIsUploading] = useState(false)
  const [currentConversationId, setCurrentConversationId] = useState<string | null>(null)
  const [autoSaveTimer, setAutoSaveTimer] = useState<number | null>(null)
  const messagesEndRef = useRef<HTMLDivElement>(null)
  const fileInputRef = useRef<HTMLInputElement>(null)
  const chatContainerRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    scrollToBottom()
  }, [messages, currentStreamingMessage])

  // Load conversation when agent changes
  useEffect(() => {
    if (selectedAgent) {
      loadConversationForAgent(selectedAgent)
    }
  }, [selectedAgent])
  
  // Subscribe to agent stream when WebSocket is ready
  useEffect(() => {
    if (selectedAgent && ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        type: 'create_stream',
        agent_id: selectedAgent
      }))
    }
  }, [selectedAgent, ws])

  // Auto-save conversation on message changes
  useEffect(() => {
    if (messages.length > 0 && currentConversationId) {
      // Clear existing timer
      if (autoSaveTimer) {
        clearTimeout(autoSaveTimer)
      }
      
      // Set new timer for auto-save (2 seconds after last change)
      const timer = window.setTimeout(() => {
        autoSaveConversation()
      }, 2000)
      
      setAutoSaveTimer(timer)
      
      return () => {
        if (timer) clearTimeout(timer)
      }
    }
  }, [messages])

  // Load conversation for specific agent
  const loadConversationForAgent = async (agentId: string) => {
    try {
      const response = await fetch('/api/conversations')
      if (response.ok) {
        const data = await response.json()
        const agentConversations = data.conversations.filter((conv: any) => 
          conv.agent_id === agentId
        )
        
        if (agentConversations.length > 0) {
          // Load the most recent conversation
          const latestConversation = agentConversations.sort((a: any, b: any) => 
            b.updated_at - a.updated_at
          )[0]
          
          setCurrentConversationId(latestConversation.id)
          setMessages(latestConversation.messages.map((msg: any) => ({
            ...msg,
            timestamp: new Date(msg.timestamp)
          })))
        } else {
          // Create new conversation for this agent
          createNewConversation(agentId)
        }
      }
    } catch (error) {
      console.error('Failed to load conversations:', error)
      createNewConversation(agentId)
    }
  }

  // Create new conversation
  const createNewConversation = async (agentId: string) => {
    try {
      const response = await fetch('/api/conversations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: `Chat with ${agents.get(agentId)?.name || 'Agent'} - ${new Date().toLocaleString()}`,
          agent_id: agentId,
          messages: []
        })
      })
      
      if (response.ok) {
        const result = await response.json()
        setCurrentConversationId(result.id)
        // Only clear messages if we don't have any existing messages
        if (messages.length === 0) {
          setMessages([])
        }
      }
    } catch (error) {
      console.error('Failed to create conversation:', error)
    }
  }

  // Auto-save current conversation
  const autoSaveConversation = async () => {
    if (!currentConversationId || messages.length === 0) return
    
    try {
      await fetch('/api/conversations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          id: currentConversationId,
          name: `Chat with ${agents.get(selectedAgent!)?.name || 'Agent'} - ${new Date().toLocaleString()}`,
          agent_id: selectedAgent,
          messages: messages
        })
      })
    } catch (error) {
      console.error('Failed to auto-save conversation:', error)
    }
  }

  // WebSocket streaming event handler
  useEffect(() => {
    if (!ws) return

    const handleWebSocketMessage = (event: MessageEvent) => {
      try {
        const data = JSON.parse(event.data)
        
        switch (data.type) {
          case 'chat_response_start':
            setIsStreaming(true)
            setCurrentStreamingMessage('')
            setStreamingMessageId(Date.now().toString())
            break
            
          case 'chat_response_token':
            if (data.token) {
              setCurrentStreamingMessage(prev => prev + data.token)
            }
            break
            
          case 'chat_response_complete':
            if (currentStreamingMessage || data.content) {
              const finalContent = currentStreamingMessage || data.content || ''
              const agentMessage: Message = {
                id: streamingMessageId || Date.now().toString(),
                sender: 'agent',
                content: finalContent,
                timestamp: new Date(),
                type: detectMessageType(finalContent),
                format: detectContentFormat(finalContent)
              }
              setMessages(prev => [...prev, agentMessage])
              
              // Send to timeline
              sendTimelineEvent({
                type: 'message',
                source: 'agent',
                content: finalContent,
                agentId: selectedAgent,
                agentName: agents.get(selectedAgent)?.name,
                conversationId: currentConversationId,
                metadata: {
                  format: agentMessage.format,
                  model: data.model,
                  tokens: data.tokens
                }
              })
            }
            setIsStreaming(false)
            setCurrentStreamingMessage('')
            setStreamingMessageId(null)
            break
            
          case 'chat_error':
            const errorMessage: Message = {
              id: Date.now().toString(),
              sender: 'agent',
              content: `Error: ${data.error || 'Failed to get response'}`,
              timestamp: new Date(),
              type: 'text',
              format: 'plain'
            }
            setMessages(prev => [...prev, errorMessage])
            setIsStreaming(false)
            break
            
          default:
            console.log('Unhandled WebSocket message:', data)
        }
      } catch (error) {
        console.error('Failed to parse WebSocket message:', error)
      }
    }

    const handleCustomStreamEvent = (event: CustomEvent) => {
      const data = event.detail
      handleWebSocketMessage({ data: JSON.stringify(data) } as MessageEvent)
    }

    ws.addEventListener('message', handleWebSocketMessage)
    window.addEventListener('agent_stream', handleCustomStreamEvent as EventListener)
    
    return () => {
      ws.removeEventListener('message', handleWebSocketMessage)
      window.removeEventListener('agent_stream', handleCustomStreamEvent as EventListener)
    }
  }, [ws])

  const scrollToBottom = () => {
    if (chatContainerRef.current) {
      // Use setTimeout to ensure DOM has updated
      setTimeout(() => {
        const container = chatContainerRef.current
        if (container) {
          container.scrollTop = container.scrollHeight
        }
      }, 0)
    }
  }

  // File and image upload functions
  const handleFileUpload = async (files: FileList) => {
    if (!files || files.length === 0) return []
    
    setIsUploading(true)
    const allFiles = Array.from(files)
    const imageFiles = allFiles.filter(file => file.type.startsWith('image/'))
    const documentFiles = allFiles.filter(file => !file.type.startsWith('image/'))
    
    try {
      // Handle image files
      const imageUrls: string[] = []
      for (const file of imageFiles) {
        // For now, create object URLs for preview (can be uploaded to server later)
        const objectUrl = URL.createObjectURL(file)
        imageUrls.push(objectUrl)
      }
      
      // Add images to state for preview
      setImages(prev => [...prev, ...imageFiles])
      
      // Handle document files (if any)
      if (documentFiles.length > 0) {
        console.log('Document files detected:', documentFiles.map(f => f.name))
        // Could implement document upload here
      }
      
      return imageUrls
    } catch (error) {
      console.error('File upload failed:', error)
      return []
    } finally {
      setIsUploading(false)
    }
  }

  const handleImageUpload = async (files: FileList) => {
    return handleFileUpload(files)
  }

  const removeImage = (index: number) => {
    setImages(prev => prev.filter((_, i) => i !== index))
  }

  // Conversation management functions
  const saveConversation = async (name?: string) => {
    try {
      const response = await fetch('/api/conversations', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          id: currentConversationId,
          name: name || `Conversation ${new Date().toLocaleString()}`,
          agent_id: selectedAgent,
          messages: messages
        })
      })
      
      if (!response.ok) throw new Error('Save failed')
      const result = await response.json()
      console.log('Conversation saved:', result.id)
      
      // Update current conversation ID if this was a new conversation
      if (!currentConversationId) {
        setCurrentConversationId(result.id)
      }
      
      return result.id
    } catch (error) {
      console.error('Failed to save conversation:', error)
    }
  }

  const loadConversation = async (conversationId: string) => {
    try {
      const response = await fetch(`/api/conversations/${conversationId}`)
      if (!response.ok) throw new Error('Load failed')
      
      const conversation = await response.json()
      setMessages(conversation.messages || [])
    } catch (error) {
      console.error('Failed to load conversation:', error)
    }
  }

  const deleteMessage = (messageId: string) => {
    setMessages(prev => prev.filter(msg => msg.id !== messageId))
  }

  const editMessage = (messageId: string, newContent: string) => {
    setMessages(prev => prev.map(msg => 
      msg.id === messageId 
        ? { 
            ...msg, 
            content: newContent, 
            metadata: { 
              ...msg.metadata, 
              edited: true, 
              editedAt: new Date() 
            } 
          }
        : msg
    ))
  }

  const detectMessageType = (content: string): Message['type'] => {
    const format = detectContentFormat(content)
    if (format === 'html') return 'html'
    if (format === 'latex') return 'latex'
    if (format === 'json') return 'json'
    if (format === 'markdown') return 'markdown'
    if (content.includes('```')) return 'code'
    if (content.includes('$') || content.includes('equation')) return 'math'
    if (content.includes('chart') || content.includes('data')) return 'data'
    return 'text'
  }

  // Send event to timeline
  const sendTimelineEvent = async (event: any) => {
    try {
      await fetch('/api/timeline/events', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          id: `event_${Date.now()}_${Math.random()}`,
          timestamp: new Date().toISOString(),
          ...event
        })
      })
    } catch (error) {
      console.error('Failed to send timeline event:', error)
    }
  }

  const sendMessage = async () => {
    if (!input.trim() || !selectedAgent) return

    // Create user message
    const userMessage: Message = {
      id: Date.now().toString(),
      sender: 'user',
      content: input,
      timestamp: new Date(),
      type: detectMessageType(input),
      format: detectContentFormat(input),
      images: images.length > 0 ? images.map(img => URL.createObjectURL(img)) : undefined
    }

    // Add user message to conversation
    setMessages(prev => [...prev, userMessage])
    const messageContent = input
    setInput('')
    setImages([])

    // Send to timeline
    sendTimelineEvent({
      type: 'message',
      source: 'user',
      content: messageContent,
      agentId: selectedAgent,
      agentName: agents.get(selectedAgent)?.name,
      conversationId: currentConversationId,
      metadata: {
        format: userMessage.format,
        images: userMessage.images
      }
    })

    try {
      // Try WebSocket first for streaming
      if (ws && ws.readyState === WebSocket.OPEN) {
        setIsStreaming(true)
        setCurrentStreamingMessage('')
        setStreamingMessageId(Date.now().toString())
        
        ws.send(JSON.stringify({
          type: 'stream_chat',
          message: messageContent,
          conversation_id: currentConversationId
        }))
      } else {
        // Fallback to HTTP API
        setIsStreaming(true)
        const response = await fetch(`/api/agents/${selectedAgent}/chat`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ 
            message: messageContent,
            conversation_id: currentConversationId,
            images: userMessage.images 
          })
        })

        if (response.ok) {
          const data = await response.json()
          const agentMessage: Message = {
            id: Date.now().toString(),
            sender: 'agent',
            content: data.response || data.result || 'Agent response received',
            timestamp: new Date(),
            type: detectMessageType(data.response || data.result || ''),
            format: detectContentFormat(data.response || data.result || '')
          }
          
          setMessages(prev => [...prev, agentMessage])
          
          // Send to timeline
          sendTimelineEvent({
            type: 'message',
            source: 'agent',
            content: agentMessage.content,
            agentId: selectedAgent,
            agentName: agents.get(selectedAgent)?.name,
            conversationId: currentConversationId,
            metadata: {
              format: agentMessage.format,
              model: data.model
            }
          })
        } else {
          // Show error message
          const errorMessage: Message = {
            id: Date.now().toString(),
            sender: 'agent',
            content: `Error: Failed to get response from agent (${response.status})`,
            timestamp: new Date(),
            type: 'text',
            format: 'plain'
          }
          setMessages(prev => [...prev, errorMessage])
        }
        setIsStreaming(false)
      }
    } catch (error) {
      console.error('Error sending message:', error)
      const errorMessage: Message = {
        id: Date.now().toString(),
        sender: 'agent',
        content: `Error: ${error.message || 'Failed to send message'}`,
        timestamp: new Date(),
        type: 'text',
        format: 'plain'
      }
      setMessages(prev => [...prev, errorMessage])
      setIsStreaming(false)
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
    <div className="flex h-full w-full">
      <div className="flex-1 flex flex-col bg-white border rounded-lg shadow-sm">
        <div className="flex items-center justify-between p-3 border-b bg-gray-50 rounded-t-lg">
          <div className="flex items-center gap-2">
            <Brain className="h-4 w-4" />
            <h3 className="font-semibold text-sm">
              Chat with {agent?.name || agent?.type || 'Agent'}
            </h3>
            {agent?.model && (
              <span className="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">
                {agent.model}
              </span>
            )}
          </div>
          <div className="flex gap-1">
            <Button
              variant="ghost"
              size="sm"
              onClick={() => saveConversation()}
              className="h-7 w-7 p-0"
              title="Save conversation"
            >
              <Save className="h-3 w-3" />
            </Button>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setMessages([])}
              className="h-7 w-7 p-0"
              title="Clear conversation"
            >
              <Trash2 className="h-3 w-3" />
            </Button>
            <Button
              variant={isAdvancedMode ? "default" : "ghost"}
              size="sm"
              onClick={() => setIsAdvancedMode(!isAdvancedMode)}
              className="h-7 px-2"
            >
              <Zap className="h-3 w-3" />
            </Button>
            <Button
              variant={showInteractivePanel ? "default" : "ghost"}
              size="sm"
              onClick={() => setShowInteractivePanel(!showInteractivePanel)}
              className="h-7 px-2"
            >
              <Terminal className="h-3 w-3" />
            </Button>
            <Button
              variant={showChainOfThought ? "default" : "ghost"}
              size="sm"
              onClick={() => setShowChainOfThought(!showChainOfThought)}
              className="h-7 px-2"
            >
              <Eye className="h-3 w-3" />
            </Button>
          </div>
        </div>
        <div className="flex-1 flex flex-col min-h-0">
          <div 
            ref={chatContainerRef}
            className="flex-1 overflow-y-auto space-y-3 p-4 scroll-smooth"
            style={{ minHeight: 0 }}
          >
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
                    "max-w-[85%] transition-all duration-300 group",
                    message.sender === 'user'
                      ? "bg-blue-600 text-white rounded-lg p-4"
                      : "bg-gray-100 text-gray-900 rounded-lg p-4"
                  )}
                >
                  <div className="flex justify-between items-start mb-2">
                    <div className="flex-1">
                      {isAdvancedMode && <ContentAnalysisBadge content={message.content} format={message.format} />}
                    </div>
                    <div className="flex gap-1 opacity-0 group-hover:opacity-100 transition-opacity">
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => editMessage(message.id, message.content)}
                        className="h-6 w-6 p-0"
                      >
                        <FileText className="h-3 w-3" />
                      </Button>
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => deleteMessage(message.id)}
                        className="h-6 w-6 p-0 text-red-500 hover:text-red-700"
                      >
                        <Trash2 className="h-3 w-3" />
                      </Button>
                    </div>
                  </div>
                  
                  {/* Render images if present */}
                  {message.images && message.images.length > 0 && (
                    <div className="grid grid-cols-2 gap-2 mb-3">
                      {message.images.map((imageUrl, index) => (
                        <img
                          key={index}
                          src={imageUrl}
                          alt={`Attachment ${index + 1}`}
                          className="rounded-md max-w-full h-auto cursor-pointer hover:opacity-90"
                          onClick={() => window.open(imageUrl, '_blank')}
                        />
                      ))}
                    </div>
                  )}
                  
                  {isAdvancedMode ? (
                    <AdvancedMessageContent content={message.content} format={message.format} isUser={message.sender === 'user'} />
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
            
            {/* Show current streaming message */}
            {isStreaming && (
              <div className="flex justify-start">
                <div className="max-w-[85%] bg-gray-100 text-gray-900 rounded-lg p-4 relative overflow-hidden">
                  <div className="flex items-center gap-2 mb-2">
                    <div className="w-2 h-2 bg-blue-500 rounded-full animate-pulse"></div>
                    <span className="text-xs text-muted-foreground">AI is thinking...</span>
                  </div>
                  {currentStreamingMessage ? (
                    isAdvancedMode ? (
                      <AdvancedMessageContent content={currentStreamingMessage} format={detectContentFormat(currentStreamingMessage)} isUser={false} />
                    ) : (
                      <p className="text-sm whitespace-pre-wrap">
                        {currentStreamingMessage}
                        <span className="inline-block w-2 h-4 bg-gray-400 animate-pulse ml-1"></span>
                      </p>
                    )
                  ) : (
                    <div className="flex items-center gap-2">
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '0ms' }}></div>
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '150ms' }}></div>
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '300ms' }}></div>
                    </div>
                  )}
                </div>
              </div>
            )}
            <div ref={messagesEndRef} />
          </div>
          
          {/* Image Preview Area */}
          {images.length > 0 && (
            <div className="border-t pt-2 mb-2 mx-4">
              <div className="flex flex-wrap gap-2">
                {images.map((image, index) => (
                  <div key={index} className="relative">
                    <img
                      src={URL.createObjectURL(image)}
                      alt={`Upload ${index + 1}`}
                      className="w-12 h-12 object-cover rounded border"
                    />
                    <Button
                      variant="destructive"
                      size="sm"
                      onClick={() => removeImage(index)}
                      className="absolute -top-1 -right-1 w-5 h-5 p-0"
                    >
                      <X className="h-2 w-2" />
                    </Button>
                  </div>
                ))}
              </div>
            </div>
          )}

          <div 
            className="flex gap-2 p-4 border-t bg-gray-50"
            onDrop={(e) => {
              e.preventDefault()
              if (e.dataTransfer.files) {
                handleFileUpload(e.dataTransfer.files)
              }
            }}
            onDragOver={(e) => e.preventDefault()}
            onDragEnter={(e) => e.preventDefault()}
          >
            <input
              ref={fileInputRef}
              type="file"
              multiple
              accept="image/*,application/pdf,.txt,.doc,.docx,.md"
              onChange={(e) => {
                if (e.target.files) {
                  handleFileUpload(e.target.files)
                }
              }}
              className="hidden"
            />
            <Button
              variant="ghost"
              size="sm"
              onClick={() => fileInputRef.current?.click()}
              disabled={isUploading}
              className="h-9 w-9 p-0"
            >
              <Paperclip className="h-4 w-4" />
            </Button>
            <Input
              value={input}
              onChange={(e) => setInput(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && sendMessage()}
              placeholder="Type your message..."
              disabled={isStreaming}
              className="flex-1"
            />
            <Button 
              onClick={sendMessage} 
              disabled={isStreaming || (!input.trim() && images.length === 0)}
              className="h-9"
            >
              <Send className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>
      
      {showInteractivePanel && (
        <div className="w-80 bg-gray-50 border-l border-gray-200 p-3">
          <InteractivePanel 
            isVisible={showInteractivePanel}
            onToggle={() => setShowInteractivePanel(!showInteractivePanel)}
          />
        </div>
      )}
      
      {/* 🧠 CHAIN OF THOUGHT PANEL */}
      {showChainOfThought && (
        <div className="w-80 bg-gray-50 border-l border-gray-200 overflow-y-auto">
          <div className="p-3 space-y-3">
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