import { useState, useEffect, useMemo, useCallback } from 'react'
import { cn } from '@/lib/utils'
import AdvancedMessageRenderer from './AdvancedMessageRenderer'
import ContextualRetrievalSystem from '../services/ContextualRetrieval'

interface DynamicUIManagerProps {
  messages: Message[]
  onLayoutChange?: (layout: UILayout) => void
  onInteraction?: (action: string, data: any) => void
  contextualRetrieval: ContextualRetrievalSystem
  ws: WebSocket | null
}

interface Message {
  id: string
  sender: 'user' | 'agent'
  content: string
  timestamp: Date
  metadata?: {
    type?: 'code' | 'data' | 'math' | 'text' | 'mixed'
    complexity?: 'simple' | 'medium' | 'complex'
    hasInteractiveElements?: boolean
    suggestedActions?: string[]
  }
}

interface UILayout {
  type: 'standard' | 'split' | 'dashboard' | 'focused' | 'collaborative'
  panels: UIPanel[]
  contextSidebar: boolean
  suggestionBar: boolean
  interactiveElements: InteractiveElement[]
}

interface UIPanel {
  id: string
  type: 'chat' | 'code' | 'visualization' | 'documentation' | 'context' | 'tools'
  position: 'main' | 'sidebar' | 'bottom' | 'floating'
  size: 'small' | 'medium' | 'large' | 'auto'
  content?: any
  adaptable: boolean
}

interface InteractiveElement {
  id: string
  type: 'button' | 'slider' | 'input' | 'chart' | 'code-editor' | 'widget'
  position: { x: number; y: number }
  data: any
  context: string[]
}

interface ContextualInsight {
  type: 'suggestion' | 'warning' | 'insight' | 'opportunity'
  content: string
  actionable: boolean
  priority: 'low' | 'medium' | 'high'
  relatedMessages: string[]
}

export default function DynamicUIManager({
  messages,
  onLayoutChange,
  onInteraction,
  contextualRetrieval,
  ws
}: DynamicUIManagerProps) {
  const [currentLayout, setCurrentLayout] = useState<UILayout>({
    type: 'standard',
    panels: [],
    contextSidebar: false,
    suggestionBar: false,
    interactiveElements: []
  })
  
  const [contextualInsights, setContextualInsights] = useState<ContextualInsight[]>([])
  const [adaptiveState, setAdaptiveState] = useState<Map<string, any>>(new Map())
  const [userPreferences, setUserPreferences] = useState<any>({})
  const [collaborativeState, setCollaborativeState] = useState<any>({})

  // Analyze content and adapt UI layout
  const analyzeAndAdapt = useCallback(async (newMessages: Message[]) => {
    if (newMessages.length === 0) return

    const latestMessage = newMessages[newMessages.length - 1]
    const conversationHistory = newMessages.slice(-10).map(m => m.content)

    // Get contextual suggestions
    const suggestions = await contextualRetrieval.generateSuggestions(
      latestMessage.content,
      conversationHistory,
      userPreferences
    )

    setContextualInsights(prev => [
      ...suggestions.contextualInsights.map(insight => ({
        type: 'insight' as const,
        content: insight,
        actionable: true,
        priority: 'medium' as const,
        relatedMessages: [latestMessage.id]
      })),
      ...prev.slice(0, 5) // Keep last 5 insights
    ])

    // Determine optimal layout based on content analysis
    const optimalLayout = determineOptimalLayout(newMessages, suggestions)
    
    if (optimalLayout.type !== currentLayout.type) {
      setCurrentLayout(optimalLayout)
      onLayoutChange?.(optimalLayout)
    }
  }, [contextualRetrieval, userPreferences, currentLayout, onLayoutChange])

  // Effect to analyze new messages
  useEffect(() => {
    analyzeAndAdapt(messages)
  }, [messages, analyzeAndAdapt])

  // Determine optimal layout based on content
  const determineOptimalLayout = (messages: Message[], suggestions: any): UILayout => {
    const recentMessages = messages.slice(-5)
    const hasCode = recentMessages.some(m => m.content.includes('```') || m.content.includes('function'))
    const hasData = recentMessages.some(m => m.content.includes('data') || m.content.includes('chart'))
    const hasMath = recentMessages.some(m => m.content.includes('$') || m.content.includes('equation'))
    const isComplex = recentMessages.some(m => m.metadata?.complexity === 'complex')

    let layoutType: UILayout['type'] = 'standard'
    const panels: UIPanel[] = [
      {
        id: 'main-chat',
        type: 'chat',
        position: 'main',
        size: 'large',
        adaptable: true
      }
    ]

    // Add code panel for code-heavy conversations
    if (hasCode) {
      panels.push({
        id: 'code-panel',
        type: 'code',
        position: 'sidebar',
        size: 'medium',
        adaptable: true
      })
      layoutType = 'split'
    }

    // Add visualization panel for data-heavy content
    if (hasData) {
      panels.push({
        id: 'viz-panel',
        type: 'visualization',
        position: hasCode ? 'bottom' : 'sidebar',
        size: 'medium',
        adaptable: true
      })
      layoutType = hasCode ? 'dashboard' : 'split'
    }

    // Add context sidebar for complex conversations
    const contextSidebar = isComplex || suggestions.relatedContent.length > 3

    // Add suggestion bar if there are actionable suggestions
    const suggestionBar = suggestions.suggestedActions.length > 0

    return {
      type: layoutType,
      panels,
      contextSidebar,
      suggestionBar,
      interactiveElements: generateInteractiveElements(recentMessages, suggestions)
    }
  }

  // Generate interactive elements based on content
  const generateInteractiveElements = (messages: Message[], suggestions: any): InteractiveElement[] => {
    const elements: InteractiveElement[] = []

    messages.forEach((message, index) => {
      if (message.content.includes('```')) {
        elements.push({
          id: `code-${message.id}`,
          type: 'code-editor',
          position: { x: 0, y: index * 100 },
          data: { code: extractCodeFromMessage(message.content) },
          context: ['code', 'development']
        })
      }

      if (message.content.includes('chart') || message.content.includes('graph')) {
        elements.push({
          id: `chart-${message.id}`,
          type: 'chart',
          position: { x: 300, y: index * 100 },
          data: { type: 'dynamic' },
          context: ['visualization', 'data']
        })
      }
    })

    return elements
  }

  // Render adaptive chat interface
  const renderChatInterface = () => (
    <div className={cn(
      "flex-1 space-y-4 overflow-y-auto",
      currentLayout.type === 'focused' && "max-w-4xl mx-auto",
      currentLayout.type === 'collaborative' && "border-2 border-blue-200"
    )}>
      {messages.map((message) => (
        <AdaptiveMessageContainer
          key={message.id}
          message={message}
          layout={currentLayout}
          onAnalysis={(analysis) => handleContentAnalysis(message.id, analysis)}
          onInteraction={(action, data) => handleInteraction(action, data, message.id)}
        />
      ))}
    </div>
  )

  // Render context sidebar
  const renderContextSidebar = () => (
    <div className="w-80 bg-gray-50 dark:bg-gray-900 border-l border-gray-200 dark:border-gray-700 p-4 overflow-y-auto">
      <h3 className="font-semibold text-lg mb-4">Context & Insights</h3>
      
      {/* Contextual Insights */}
      <div className="space-y-3 mb-6">
        <h4 className="font-medium text-sm text-gray-600 dark:text-gray-400">Recent Insights</h4>
        {contextualInsights.map((insight, index) => (
          <div
            key={index}
            className={cn(
              "p-3 rounded-lg text-sm",
              insight.type === 'suggestion' && "bg-blue-50 border border-blue-200",
              insight.type === 'warning' && "bg-yellow-50 border border-yellow-200",
              insight.type === 'insight' && "bg-green-50 border border-green-200",
              insight.type === 'opportunity' && "bg-purple-50 border border-purple-200"
            )}
          >
            <div className="flex items-start gap-2">
              <span className="text-lg">
                {insight.type === 'suggestion' && '💡'}
                {insight.type === 'warning' && '⚠️'}
                {insight.type === 'insight' && '🔍'}
                {insight.type === 'opportunity' && '🚀'}
              </span>
              <div>
                <p>{insight.content}</p>
                {insight.actionable && (
                  <button
                    className="mt-2 text-xs bg-white px-2 py-1 rounded border hover:bg-gray-50"
                    onClick={() => handleInsightAction(insight)}
                  >
                    Take Action
                  </button>
                )}
              </div>
            </div>
          </div>
        ))}
      </div>

      {/* Related Content */}
      <ContextualContentPanel contextualRetrieval={contextualRetrieval} />
    </div>
  )

  // Render suggestion bar
  const renderSuggestionBar = () => (
    <div className="bg-white dark:bg-gray-800 border-t border-gray-200 dark:border-gray-700 p-3">
      <div className="flex flex-wrap gap-2">
        <span className="text-sm text-gray-600 dark:text-gray-400 mr-2">Quick Actions:</span>
        {contextualInsights
          .filter(insight => insight.actionable)
          .slice(0, 4)
          .map((insight, index) => (
            <button
              key={index}
              className="text-xs bg-blue-100 hover:bg-blue-200 text-blue-800 px-3 py-1 rounded-full transition-colors"
              onClick={() => handleInsightAction(insight)}
            >
              {insight.content.split(' ').slice(0, 3).join(' ')}
            </button>
          ))}
      </div>
    </div>
  )

  // Handle content analysis from messages
  const handleContentAnalysis = (messageId: string, analysis: any) => {
    const updatedMessages = messages.map(msg =>
      msg.id === messageId ? { ...msg, metadata: { ...msg.metadata, ...analysis } } : msg
    )
    
    // Trigger re-analysis with updated metadata
    analyzeAndAdapt(updatedMessages)
  }

  // Handle user interactions
  const handleInteraction = (action: string, data: any, messageId?: string) => {
    console.log('UI Interaction:', { action, data, messageId })
    
    // Store interaction in contextual retrieval system
    if (messageId) {
      contextualRetrieval.storeContent(
        `User action: ${action}`,
        'message',
        [action, 'interaction'],
        { originalMessageId: messageId, actionData: data }
      )
    }

    onInteraction?.(action, data)
  }

  // Handle insight actions
  const handleInsightAction = (insight: ContextualInsight) => {
    console.log('Insight action:', insight)
    // Implement specific actions based on insight type
  }

  // Extract code from message content
  const extractCodeFromMessage = (content: string): string => {
    const codeMatch = content.match(/```[\w]*\n?([\s\S]*?)```/)
    return codeMatch ? codeMatch[1].trim() : ''
  }

  // Main render method
  return (
    <div className={cn(
      "h-full flex",
      currentLayout.type === 'dashboard' && "flex-col",
      currentLayout.type === 'collaborative' && "relative"
    )}>
      {/* Main content area */}
      <div className="flex-1 flex flex-col">
        {renderChatInterface()}
        {currentLayout.suggestionBar && renderSuggestionBar()}
      </div>

      {/* Context sidebar */}
      {currentLayout.contextSidebar && renderContextSidebar()}

      {/* Interactive overlays for collaborative mode */}
      {currentLayout.type === 'collaborative' && (
        <CollaborativeOverlay
          ws={ws}
          onStateChange={setCollaborativeState}
        />
      )}

      {/* Floating interactive elements */}
      {currentLayout.interactiveElements.map(element => (
        <FloatingElement
          key={element.id}
          element={element}
          onInteraction={handleInteraction}
        />
      ))}
    </div>
  )
}

// Adaptive message container
function AdaptiveMessageContainer({
  message,
  layout,
  onAnalysis,
  onInteraction
}: {
  message: Message
  layout: UILayout
  onAnalysis: (analysis: any) => void
  onInteraction: (action: string, data: any) => void
}) {
  const [isExpanded, setIsExpanded] = useState(false)
  const [analysisState, setAnalysisState] = useState<any>(null)

  const shouldShowExpanded = useMemo(() => {
    return layout.type === 'focused' || 
           message.metadata?.complexity === 'complex' ||
           layout.type === 'dashboard'
  }, [layout, message.metadata?.complexity])

  return (
    <div className={cn(
      "transition-all duration-300",
      shouldShowExpanded && "bg-gray-50 dark:bg-gray-900 rounded-lg p-4 border",
      message.sender === 'user' ? "justify-end" : "justify-start",
      "flex"
    )}>
      <div className={cn(
        "max-w-[85%] transition-all duration-300",
        shouldShowExpanded && "max-w-full",
        message.sender === 'user'
          ? "bg-primary text-primary-foreground rounded-lg p-4"
          : "bg-muted rounded-lg p-4"
      )}>
        <AdvancedMessageRenderer
          content={message.content}
          sender={message.sender}
          messageId={message.id}
          onContentAnalysis={onAnalysis}
        />
        
        {shouldShowExpanded && message.metadata?.hasInteractiveElements && (
          <div className="mt-3 flex gap-2">
            <button
              className="text-xs bg-blue-100 hover:bg-blue-200 text-blue-800 px-2 py-1 rounded"
              onClick={() => setIsExpanded(!isExpanded)}
            >
              {isExpanded ? 'Collapse' : 'Expand'}
            </button>
            {message.metadata.suggestedActions?.map((action, index) => (
              <button
                key={index}
                className="text-xs bg-green-100 hover:bg-green-200 text-green-800 px-2 py-1 rounded"
                onClick={() => onInteraction(action, { messageId: message.id })}
              >
                {action}
              </button>
            ))}
          </div>
        )}
      </div>
    </div>
  )
}

// Contextual content panel
function ContextualContentPanel({ contextualRetrieval }: { contextualRetrieval: ContextualRetrievalSystem }) {
  const [relatedContent, setRelatedContent] = useState<any[]>([])

  useEffect(() => {
    // Load related content based on current conversation
    const loadRelatedContent = async () => {
      // This would be triggered by conversation changes
      const results = await contextualRetrieval.semanticSearch(
        "recent conversation topics",
        []
      )
      setRelatedContent(results)
    }

    loadRelatedContent()
  }, [contextualRetrieval])

  return (
    <div className="space-y-3">
      <h4 className="font-medium text-sm text-gray-600 dark:text-gray-400">Related Content</h4>
      {relatedContent.map((content, index) => (
        <div
          key={index}
          className="p-3 bg-white dark:bg-gray-800 rounded-lg border text-sm"
        >
          <div className="font-medium text-xs text-gray-500 mb-1">{content.type}</div>
          <p className="text-sm">{content.content.slice(0, 100)}...</p>
          <div className="text-xs text-blue-600 mt-2">
            Relevance: {Math.round(content.similarity * 100)}%
          </div>
        </div>
      ))}
    </div>
  )
}

// Collaborative overlay for real-time collaboration
function CollaborativeOverlay({
  ws,
  onStateChange
}: {
  ws: WebSocket | null
  onStateChange: (state: any) => void
}) {
  const [collaborators, setCollaborators] = useState<any[]>([])

  useEffect(() => {
    if (!ws) return

    const handleCollaborativeMessage = (event: MessageEvent) => {
      try {
        const data = JSON.parse(event.data)
        if (data.type === 'collaborative_update') {
          setCollaborators(data.collaborators)
          onStateChange(data.state)
        }
      } catch (error) {
        console.warn('Failed to handle collaborative message:', error)
      }
    }

    ws.addEventListener('message', handleCollaborativeMessage)

    return () => {
      ws.removeEventListener('message', handleCollaborativeMessage)
    }
  }, [ws, onStateChange])

  return (
    <div className="absolute top-4 right-4 flex gap-2">
      {collaborators.map((collaborator, index) => (
        <div
          key={index}
          className="w-8 h-8 bg-blue-500 rounded-full flex items-center justify-center text-white text-xs"
          title={collaborator.name}
        >
          {collaborator.name[0]?.toUpperCase()}
        </div>
      ))}
    </div>
  )
}

// Floating interactive element
function FloatingElement({
  element,
  onInteraction
}: {
  element: InteractiveElement
  onInteraction: (action: string, data: any) => void
}) {
  const [isDragging, setIsDragging] = useState(false)
  const [position, setPosition] = useState(element.position)

  const handleMouseDown = () => setIsDragging(true)
  const handleMouseUp = () => setIsDragging(false)

  return (
    <div
      className={cn(
        "absolute z-50 bg-white shadow-lg rounded-lg border p-3",
        isDragging && "cursor-grabbing"
      )}
      style={{
        left: position.x,
        top: position.y,
        cursor: isDragging ? 'grabbing' : 'grab'
      }}
      onMouseDown={handleMouseDown}
      onMouseUp={handleMouseUp}
    >
      <div className="text-sm font-medium mb-2">{element.type}</div>
      {element.type === 'button' && (
        <button
          className="bg-blue-500 text-white px-3 py-1 rounded text-sm"
          onClick={() => onInteraction('floating_action', element.data)}
        >
          Action
        </button>
      )}
      {element.type === 'chart' && (
        <div className="w-32 h-20 bg-gray-100 rounded flex items-center justify-center text-xs">
          Chart Placeholder
        </div>
      )}
    </div>
  )
}