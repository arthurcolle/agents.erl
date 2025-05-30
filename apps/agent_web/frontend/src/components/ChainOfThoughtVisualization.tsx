import { useState, useEffect, useRef, useCallback } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { cn } from '@/lib/utils'
import { 
  Brain, 
  Eye, 
  Lightbulb, 
  Search, 
  CheckCircle, 
  AlertCircle, 
  HelpCircle,
  GitBranch,
  Clock,
  Target,
  TrendingUp,
  Edit3,
  Play,
  Pause,
  SkipForward,
  Rewind
} from 'lucide-react'
import ChainOfThoughtEngine from '../services/ChainOfThoughtEngine'

interface ChainOfThoughtVisualizationProps {
  engine: ChainOfThoughtEngine
  chainId: string | null
  onStepClick?: (stepId: string) => void
  onBranchCreate?: (parentStepId: string, approach: string) => void
  collaborative?: boolean
}

interface ThoughtStepDisplay {
  id: string
  type: 'observation' | 'hypothesis' | 'analysis' | 'conclusion' | 'question' | 'assumption'
  content: string
  confidence: number
  reasoning: string
  evidence: any[]
  timestamp: number
  source: 'ai' | 'human' | 'hybrid'
  validated?: boolean
  alternatives?: string[]
  position: { x: number, y: number }
  color: string
  size: number
}

// Main Chain of Thought Visualization Component
export default function ChainOfThoughtVisualization({
  engine,
  chainId,
  onStepClick,
  onBranchCreate,
  collaborative = false
}: ChainOfThoughtVisualizationProps) {
  const [visualization, setVisualization] = useState<any>(null)
  const [selectedStep, setSelectedStep] = useState<string | null>(null)
  const [isPlaying, setIsPlaying] = useState(false)
  const [playbackSpeed, setPlaybackSpeed] = useState(1)
  const [currentStep, setCurrentStep] = useState(0)
  const [showReasoningDetails, setShowReasoningDetails] = useState(true)
  const [filterBy, setFilterBy] = useState<string>('all')
  const [qualityMetrics, setQualityMetrics] = useState<any>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)

  // 🎯 THOUGHT: Why do we need visualization controls?
  // REASONING: Complex reasoning chains need interactive exploration
  // SOLUTION: Provide multiple views and playback controls

  useEffect(() => {
    if (!chainId) return

    // 🔄 REAL-TIME UPDATES: Fetch visualization data
    const updateVisualization = async () => {
      try {
        const viz = engine.getReasoningVisualization(chainId)
        setVisualization(viz)
        
        // 📊 QUALITY ASSESSMENT: Get reasoning quality metrics
        const quality = await engine.evaluateReasoningQuality(chainId)
        setQualityMetrics(quality)
      } catch (error) {
        console.error('Failed to update visualization:', error)
      }
    }

    updateVisualization()
    
    // 🔄 AUTO-REFRESH: Update every 2 seconds during active reasoning
    const interval = setInterval(updateVisualization, 2000)
    return () => clearInterval(interval)
  }, [chainId, engine])

  // 🎮 PLAYBACK CONTROL: Step through reasoning process
  const handlePlayback = useCallback(() => {
    if (!visualization) return

    if (isPlaying) {
      // ⏸️ PAUSE: Stop automatic progression
      setIsPlaying(false)
    } else {
      // ▶️ PLAY: Start stepping through reasoning
      setIsPlaying(true)
      
      const interval = setInterval(() => {
        setCurrentStep(prev => {
          const next = prev + 1
          if (next >= visualization.nodes.length) {
            setIsPlaying(false)
            return prev
          }
          return next
        })
      }, 1000 / playbackSpeed)

      // Store interval for cleanup
      setTimeout(() => clearInterval(interval), 10000)
    }
  }, [isPlaying, visualization, playbackSpeed])

  // 🎨 RENDERING: Draw reasoning flow on canvas
  useEffect(() => {
    if (!visualization || !canvasRef.current) return

    const canvas = canvasRef.current
    const ctx = canvas.getContext('2d')
    if (!ctx) return

    // 🧹 CLEAR CANVAS
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    // 🎯 DRAW REASONING STEPS AS NODES
    visualization.nodes.forEach((node: ThoughtStepDisplay, index: number) => {
      const isCurrentStep = index <= currentStep
      const isSelected = node.id === selectedStep
      
      // 🎨 NODE STYLING: Visual encoding of reasoning state
      ctx.save()
      
      // 🌟 HIGHLIGHT LOGIC: Show current focus
      if (isSelected) {
        ctx.shadowColor = '#3B82F6'
        ctx.shadowBlur = 15
      }
      
      // 🎯 VISIBILITY: Fade future steps during playback
      ctx.globalAlpha = isCurrentStep ? 1.0 : 0.3
      
      // 🔵 DRAW NODE: Circle representing thought step
      ctx.beginPath()
      ctx.arc(node.position.x, node.position.y, node.size, 0, 2 * Math.PI)
      ctx.fillStyle = node.color
      ctx.fill()
      
      // 🔲 CONFIDENCE RING: Visual confidence indicator
      ctx.beginPath()
      ctx.arc(node.position.x, node.position.y, node.size + 5, 0, 2 * Math.PI * node.confidence)
      ctx.strokeStyle = node.confidence > 0.7 ? '#10B981' : node.confidence > 0.4 ? '#F59E0B' : '#EF4444'
      ctx.lineWidth = 3
      ctx.stroke()
      
      // ✅ VALIDATION INDICATOR: Show if step is validated
      if (node.validated) {
        ctx.beginPath()
        ctx.arc(node.position.x + node.size * 0.7, node.position.y - node.size * 0.7, 8, 0, 2 * Math.PI)
        ctx.fillStyle = '#10B981'
        ctx.fill()
        
        // ✓ CHECK MARK
        ctx.strokeStyle = 'white'
        ctx.lineWidth = 2
        ctx.beginPath()
        ctx.moveTo(node.position.x + node.size * 0.7 - 3, node.position.y - node.size * 0.7)
        ctx.lineTo(node.position.x + node.size * 0.7, node.position.y - node.size * 0.7 + 3)
        ctx.lineTo(node.position.x + node.size * 0.7 + 4, node.position.y - node.size * 0.7 - 4)
        ctx.stroke()
      }
      
      ctx.restore()
    })

    // 🔗 DRAW REASONING CONNECTIONS
    visualization.edges.forEach((edge: any) => {
      const sourceNode = visualization.nodes.find((n: any) => n.id === edge.source)
      const targetNode = visualization.nodes.find((n: any) => n.id === edge.target)
      
      if (sourceNode && targetNode) {
        ctx.save()
        
        // 🌊 FLOW DIRECTION: Animate reasoning flow
        const gradient = ctx.createLinearGradient(
          sourceNode.position.x, sourceNode.position.y,
          targetNode.position.x, targetNode.position.y
        )
        gradient.addColorStop(0, 'rgba(59, 130, 246, 0.8)')
        gradient.addColorStop(1, 'rgba(59, 130, 246, 0.3)')
        
        ctx.strokeStyle = gradient
        ctx.lineWidth = edge.strength * 5
        ctx.lineCap = 'round'
        
        // 📏 DRAW CONNECTION LINE
        ctx.beginPath()
        ctx.moveTo(sourceNode.position.x, sourceNode.position.y)
        ctx.lineTo(targetNode.position.x, targetNode.position.y)
        ctx.stroke()
        
        // ➡️ ARROW HEAD: Show reasoning direction
        const angle = Math.atan2(
          targetNode.position.y - sourceNode.position.y,
          targetNode.position.x - sourceNode.position.x
        )
        
        ctx.save()
        ctx.translate(targetNode.position.x, targetNode.position.y)
        ctx.rotate(angle)
        ctx.beginPath()
        ctx.moveTo(-15, -5)
        ctx.lineTo(-15, 5)
        ctx.lineTo(0, 0)
        ctx.closePath()
        ctx.fillStyle = '#3B82F6'
        ctx.fill()
        ctx.restore()
        
        ctx.restore()
      }
    })
  }, [visualization, currentStep, selectedStep])

  // 🎨 STEP TYPE ICONS: Visual representation of reasoning types
  const getStepIcon = (type: string) => {
    switch (type) {
      case 'observation': return <Eye className="h-4 w-4" />
      case 'hypothesis': return <Lightbulb className="h-4 w-4" />
      case 'analysis': return <Search className="h-4 w-4" />
      case 'conclusion': return <CheckCircle className="h-4 w-4" />
      case 'question': return <HelpCircle className="h-4 w-4" />
      case 'assumption': return <AlertCircle className="h-4 w-4" />
      default: return <Brain className="h-4 w-4" />
    }
  }

  // 🎨 CONFIDENCE COLORS: Visual encoding of certainty
  const getConfidenceColor = (confidence: number) => {
    if (confidence > 0.8) return 'bg-green-100 text-green-800 border-green-200'
    if (confidence > 0.6) return 'bg-blue-100 text-blue-800 border-blue-200'
    if (confidence > 0.4) return 'bg-yellow-100 text-yellow-800 border-yellow-200'
    return 'bg-red-100 text-red-800 border-red-200'
  }

  if (!chainId) {
    return (
      <Card className="h-96">
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <Brain className="h-12 w-12 mx-auto mb-4 opacity-50" />
            <p>Start a reasoning process to see the chain of thought</p>
          </div>
        </CardContent>
      </Card>
    )
  }

  return (
    <div className="space-y-4">
      {/* 🎛️ REASONING CONTROLS */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <Brain className="h-5 w-5" />
              Chain of Thought
              {qualityMetrics && (
                <Badge className={cn(
                  "ml-2",
                  qualityMetrics.overall_score > 0.8 ? "bg-green-100 text-green-800" :
                  qualityMetrics.overall_score > 0.6 ? "bg-blue-100 text-blue-800" :
                  "bg-yellow-100 text-yellow-800"
                )}>
                  Quality: {Math.round(qualityMetrics.overall_score * 100)}%
                </Badge>
              )}
            </CardTitle>
            
            <div className="flex items-center gap-2">
              {/* 🎮 PLAYBACK CONTROLS */}
              <Button size="sm" variant="outline" onClick={() => setCurrentStep(0)}>
                <Rewind className="h-4 w-4" />
              </Button>
              <Button size="sm" variant="outline" onClick={handlePlayback}>
                {isPlaying ? <Pause className="h-4 w-4" /> : <Play className="h-4 w-4" />}
              </Button>
              <Button size="sm" variant="outline" onClick={() => setCurrentStep(visualization?.nodes.length || 0)}>
                <SkipForward className="h-4 w-4" />
              </Button>
              
              {/* ⚡ SPEED CONTROL */}
              <select 
                value={playbackSpeed} 
                onChange={(e) => setPlaybackSpeed(Number(e.target.value))}
                className="text-xs border rounded px-2 py-1"
              >
                <option value={0.5}>0.5x</option>
                <option value={1}>1x</option>
                <option value={2}>2x</option>
                <option value={4}>4x</option>
              </select>

              {/* 👁️ VIEW CONTROLS */}
              <Button 
                size="sm" 
                variant={showReasoningDetails ? "default" : "outline"}
                onClick={() => setShowReasoningDetails(!showReasoningDetails)}
              >
                <Eye className="h-4 w-4" />
              </Button>
            </div>
          </div>
        </CardHeader>
      </Card>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
        {/* 🎨 MAIN VISUALIZATION CANVAS */}
        <Card className="lg:col-span-2">
          <CardContent className="p-4">
            <div className="relative">
              <canvas
                ref={canvasRef}
                width={600}
                height={400}
                className="border rounded-lg w-full cursor-pointer"
                onClick={(e) => {
                  // 🎯 CLICK HANDLING: Select reasoning steps
                  const rect = canvasRef.current?.getBoundingClientRect()
                  if (!rect || !visualization) return
                  
                  const x = e.clientX - rect.left
                  const y = e.clientY - rect.top
                  
                  // 🔍 FIND CLICKED STEP
                  const clickedNode = visualization.nodes.find((node: any) => {
                    const dx = x - node.position.x
                    const dy = y - node.position.y
                    return Math.sqrt(dx * dx + dy * dy) <= node.size
                  })
                  
                  if (clickedNode) {
                    setSelectedStep(clickedNode.id)
                    onStepClick?.(clickedNode.id)
                  }
                }}
              />
              
              {/* 📊 VISUALIZATION METADATA */}
              {visualization && (
                <div className="absolute top-2 left-2 bg-white bg-opacity-90 rounded px-2 py-1 text-xs">
                  <div className="flex items-center gap-4">
                    <span className="flex items-center gap-1">
                      <Target className="h-3 w-3" />
                      {visualization.metadata.step_count} steps
                    </span>
                    <span className="flex items-center gap-1">
                      <GitBranch className="h-3 w-3" />
                      {visualization.metadata.branch_count} branches
                    </span>
                    <span className="flex items-center gap-1">
                      <TrendingUp className="h-3 w-3" />
                      {Math.round(visualization.metadata.avg_confidence * 100)}% avg confidence
                    </span>
                  </div>
                </div>
              )}
            </div>
          </CardContent>
        </Card>

        {/* 📋 REASONING DETAILS PANEL */}
        <Card>
          <CardHeader>
            <CardTitle className="text-sm">Reasoning Details</CardTitle>
          </CardHeader>
          <CardContent className="p-4 space-y-4">
            {/* 🎯 SELECTED STEP DETAILS */}
            {selectedStep && visualization && (() => {
              const step = visualization.nodes.find((n: any) => n.id === selectedStep)
              if (!step) return null
              
              return (
                <div className="space-y-3">
                  <div className="flex items-center gap-2">
                    {getStepIcon(step.type)}
                    <span className="font-medium capitalize">{step.type}</span>
                    <Badge className={getConfidenceColor(step.confidence)}>
                      {Math.round(step.confidence * 100)}%
                    </Badge>
                  </div>
                  
                  <div>
                    <div className="text-sm font-medium text-gray-700 mb-1">Content:</div>
                    <div className="text-sm bg-gray-50 p-2 rounded">{step.content}</div>
                  </div>
                  
                  {showReasoningDetails && (
                    <div>
                      <div className="text-sm font-medium text-gray-700 mb-1">Reasoning:</div>
                      <div className="text-sm bg-blue-50 p-2 rounded">{step.reasoning}</div>
                    </div>
                  )}
                  
                  <div>
                    <div className="text-sm font-medium text-gray-700 mb-1">Evidence:</div>
                    <div className="text-xs text-gray-600">
                      {step.evidence_count} pieces of evidence
                    </div>
                  </div>
                  
                  {collaborative && (
                    <div className="flex gap-2">
                      <Button size="sm" variant="outline" className="flex-1">
                        <Edit3 className="h-3 w-3 mr-1" />
                        Edit
                      </Button>
                      <Button size="sm" variant="outline" className="flex-1">
                        <GitBranch className="h-3 w-3 mr-1" />
                        Branch
                      </Button>
                    </div>
                  )}
                </div>
              )
            })()}
            
            {/* 📊 QUALITY METRICS */}
            {qualityMetrics && (
              <div className="border-t pt-4">
                <div className="text-sm font-medium text-gray-700 mb-2">Quality Analysis:</div>
                
                <div className="space-y-2">
                  <div className="flex justify-between text-xs">
                    <span>Logical Consistency:</span>
                    <span className="font-medium">
                      {Math.round(qualityMetrics.logical_consistency * 100)}%
                    </span>
                  </div>
                  
                  <div className="text-xs">
                    <div className="font-medium text-green-700 mb-1">Strengths:</div>
                    <ul className="list-disc list-inside text-gray-600">
                      {qualityMetrics.strengths.map((strength: string, i: number) => (
                        <li key={i}>{strength}</li>
                      ))}
                    </ul>
                  </div>
                  
                  <div className="text-xs">
                    <div className="font-medium text-yellow-700 mb-1">Suggestions:</div>
                    <ul className="list-disc list-inside text-gray-600">
                      {qualityMetrics.suggestions.map((suggestion: string, i: number) => (
                        <li key={i}>{suggestion}</li>
                      ))}
                    </ul>
                  </div>
                </div>
              </div>
            )}
          </CardContent>
        </Card>
      </div>

      {/* 📝 REASONING TIMELINE */}
      {visualization && (
        <ReasoningTimeline 
          steps={visualization.nodes}
          currentStep={currentStep}
          onStepSelect={setSelectedStep}
          selectedStep={selectedStep}
        />
      )}
    </div>
  )
}

// 📅 REASONING TIMELINE: Linear view of thought progression
function ReasoningTimeline({ 
  steps, 
  currentStep, 
  onStepSelect, 
  selectedStep 
}: {
  steps: any[]
  currentStep: number
  onStepSelect: (stepId: string) => void
  selectedStep: string | null
}) {
  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-sm flex items-center gap-2">
          <Clock className="h-4 w-4" />
          Reasoning Timeline
        </CardTitle>
      </CardHeader>
      <CardContent className="p-4">
        <div className="flex gap-2 overflow-x-auto pb-2">
          {steps.map((step, index) => {
            const isCurrentStep = index <= currentStep
            const isSelected = step.id === selectedStep
            const isPast = index < currentStep
            
            return (
              <div
                key={step.id}
                className={cn(
                  "flex-shrink-0 w-32 p-2 rounded-lg border cursor-pointer transition-all",
                  isSelected && "ring-2 ring-blue-500",
                  isCurrentStep ? "opacity-100" : "opacity-30",
                  isPast ? "bg-green-50 border-green-200" : "bg-gray-50 border-gray-200"
                )}
                onClick={() => onStepSelect(step.id)}
              >
                <div className="flex items-center gap-1 mb-1">
                  {step.type === 'observation' && <Eye className="h-3 w-3" />}
                  {step.type === 'hypothesis' && <Lightbulb className="h-3 w-3" />}
                  {step.type === 'analysis' && <Search className="h-3 w-3" />}
                  {step.type === 'conclusion' && <CheckCircle className="h-3 w-3" />}
                  {step.type === 'question' && <HelpCircle className="h-3 w-3" />}
                  {step.type === 'assumption' && <AlertCircle className="h-3 w-3" />}
                  <span className="text-xs font-medium capitalize">{step.type}</span>
                </div>
                <div className="text-xs text-gray-600 truncate">
                  {step.content}
                </div>
                <div className="text-xs text-gray-500 mt-1">
                  {Math.round(step.confidence * 100)}% confidence
                </div>
              </div>
            )
          })}
        </div>
      </CardContent>
    </Card>
  )
}