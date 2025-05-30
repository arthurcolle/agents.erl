import { useState, useCallback } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Textarea } from '@/components/ui/textarea'
import { Badge } from '@/components/ui/badge'
import { cn } from '@/lib/utils'
import { 
  Brain,
  Plus,
  GitBranch,
  Eye,
  Lightbulb,
  Search,
  CheckCircle,
  HelpCircle,
  AlertCircle,
  Edit3,
  MessageSquare,
  ThumbsUp,
  ThumbsDown,
  Zap,
  Target,
  BookOpen,
  Users,
  TrendingUp
} from 'lucide-react'
import ChainOfThoughtEngine from '../services/ChainOfThoughtEngine'

interface InteractiveReasoningControlsProps {
  engine: ChainOfThoughtEngine
  chainId: string | null
  onChainStart?: (chainId: string) => void
  onStepAdded?: (stepId: string) => void
  collaborative?: boolean
}

interface NewStepForm {
  type: 'observation' | 'hypothesis' | 'analysis' | 'conclusion' | 'question' | 'assumption'
  content: string
  reasoning: string
  confidence: number
  evidence: string
}

interface HumanFeedback {
  type: 'correction' | 'validation' | 'suggestion' | 'question'
  content: string
  targetStepId?: string
}

// 🎛️ MAIN INTERACTIVE REASONING CONTROLS
export default function InteractiveReasoningControls({
  engine,
  chainId,
  onChainStart,
  onStepAdded,
  collaborative = false
}: InteractiveReasoningControlsProps) {
  const [newChainGoal, setNewChainGoal] = useState('')
  const [newChainContext, setNewChainContext] = useState('')
  const [showNewStepForm, setShowNewStepForm] = useState(false)
  const [newStep, setNewStep] = useState<NewStepForm>({
    type: 'observation',
    content: '',
    reasoning: '',
    confidence: 0.7,
    evidence: ''
  })
  const [humanFeedback, setHumanFeedback] = useState<HumanFeedback>({
    type: 'suggestion',
    content: '',
    targetStepId: undefined
  })
  const [showFeedbackForm, setShowFeedbackForm] = useState(false)
  const [isProcessing, setIsProcessing] = useState(false)

  // 🚀 START NEW REASONING CHAIN
  const handleStartChain = useCallback(async () => {
    if (!newChainGoal.trim()) return

    setIsProcessing(true)
    
    try {
      // 🎯 THOUGHT: Why structure the chain creation this way?
      // REASONING: Clear goal + context provides foundation for quality reasoning
      // APPROACH: Use initial observations to seed the thinking process
      
      const initialObservations = [
        `Goal: ${newChainGoal}`,
        `Context: ${newChainContext}`,
        'Beginning systematic analysis of the problem'
      ]

      const chainId = await engine.startReasoningChain(
        newChainGoal,
        newChainContext,
        initialObservations
      )

      onChainStart?.(chainId)
      setNewChainGoal('')
      setNewChainContext('')
    } catch (error) {
      console.error('Failed to start reasoning chain:', error)
    } finally {
      setIsProcessing(false)
    }
  }, [newChainGoal, newChainContext, engine, onChainStart])

  // ➕ ADD NEW REASONING STEP
  const handleAddStep = useCallback(async () => {
    if (!chainId || !newStep.content.trim()) return

    setIsProcessing(true)

    try {
      // 📝 THOUGHT: How to structure the new step?
      // REASONING: Each step needs clear content, reasoning, and evidence
      // QUALITY: Confidence should reflect actual certainty level
      
      const stepData = {
        type: newStep.type,
        content: newStep.content,
        reasoning: newStep.reasoning || `${newStep.type} step added by user`,
        confidence: newStep.confidence,
        evidence: newStep.evidence ? [{
          type: 'reference' as const,
          content: newStep.evidence,
          source: 'user_input',
          reliability: 0.8,
          relevance: 0.9
        }] : [],
        source: 'human' as const
      }

      const stepId = await engine.addThoughtStep(chainId, stepData)
      
      onStepAdded?.(stepId)
      
      // 🧹 RESET FORM
      setNewStep({
        type: 'observation',
        content: '',
        reasoning: '',
        confidence: 0.7,
        evidence: ''
      })
      setShowNewStepForm(false)
    } catch (error) {
      console.error('Failed to add reasoning step:', error)
    } finally {
      setIsProcessing(false)
    }
  }, [chainId, newStep, engine, onStepAdded])

  // 💬 SUBMIT HUMAN FEEDBACK
  const handleSubmitFeedback = useCallback(async () => {
    if (!chainId || !humanFeedback.content.trim()) return

    setIsProcessing(true)

    try {
      // 🤝 HUMAN-AI COLLABORATION: How to integrate human insights?
      // APPROACH: Treat human input as high-value reasoning steps
      // BENEFIT: Improves AI reasoning through human domain knowledge
      
      await engine.addHumanInput(
        chainId,
        humanFeedback.content,
        humanFeedback.type === 'question' ? 'question' : 'hypothesis',
        humanFeedback.targetStepId ? [{
          stepId: humanFeedback.targetStepId,
          newContent: humanFeedback.content,
          reasoning: `Human ${humanFeedback.type}: ${humanFeedback.content}`
        }] : undefined
      )

      setHumanFeedback({
        type: 'suggestion',
        content: '',
        targetStepId: undefined
      })
      setShowFeedbackForm(false)
    } catch (error) {
      console.error('Failed to submit feedback:', error)
    } finally {
      setIsProcessing(false)
    }
  }, [chainId, humanFeedback, engine])

  // 🌳 CREATE REASONING BRANCH
  const handleCreateBranch = useCallback(async (parentStepId: string) => {
    if (!chainId) return

    const approach = prompt('Enter alternative approach:')
    if (!approach) return

    try {
      // 🔀 BRANCHING LOGIC: Why allow alternative paths?
      // REASONING: Complex problems often have multiple valid solutions
      // VALUE: Prevents tunnel vision and improves solution robustness
      
      await engine.createReasoningBranch(chainId, parentStepId, approach, 0.6)
    } catch (error) {
      console.error('Failed to create branch:', error)
    }
  }, [chainId, engine])

  // 🎨 STEP TYPE CONFIGURATION
  const stepTypes = [
    { value: 'observation', label: 'Observation', icon: Eye, color: 'blue' },
    { value: 'hypothesis', label: 'Hypothesis', icon: Lightbulb, color: 'purple' },
    { value: 'analysis', label: 'Analysis', icon: Search, color: 'orange' },
    { value: 'conclusion', label: 'Conclusion', icon: CheckCircle, color: 'green' },
    { value: 'question', label: 'Question', icon: HelpCircle, color: 'red' },
    { value: 'assumption', label: 'Assumption', icon: AlertCircle, color: 'gray' }
  ]

  const feedbackTypes = [
    { value: 'correction', label: 'Correction', icon: Edit3 },
    { value: 'validation', label: 'Validation', icon: ThumbsUp },
    { value: 'suggestion', label: 'Suggestion', icon: Lightbulb },
    { value: 'question', label: 'Question', icon: HelpCircle }
  ]

  return (
    <div className="space-y-4">
      {/* 🎯 NEW REASONING CHAIN */}
      {!chainId && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Target className="h-5 w-5" />
              Start New Reasoning Chain
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <div>
              <label className="text-sm font-medium text-gray-700 mb-1 block">
                What are you trying to figure out or solve?
              </label>
              <Input
                value={newChainGoal}
                onChange={(e) => setNewChainGoal(e.target.value)}
                placeholder="e.g., How to optimize database performance for our application"
                className="w-full"
              />
            </div>
            
            <div>
              <label className="text-sm font-medium text-gray-700 mb-1 block">
                Relevant context and background information:
              </label>
              <Textarea
                value={newChainContext}
                onChange={(e) => setNewChainContext(e.target.value)}
                placeholder="Provide any relevant background, constraints, or context..."
                className="w-full h-20"
              />
            </div>
            
            <Button 
              onClick={handleStartChain}
              disabled={!newChainGoal.trim() || isProcessing}
              className="w-full"
            >
              <Brain className="h-4 w-4 mr-2" />
              {isProcessing ? 'Starting...' : 'Start Reasoning Process'}
            </Button>
          </CardContent>
        </Card>
      )}

      {/* 🎛️ ACTIVE REASONING CONTROLS */}
      {chainId && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center justify-between">
              <span className="flex items-center gap-2">
                <Brain className="h-5 w-5" />
                Active Reasoning Chain
              </span>
              <Badge variant="outline">{chainId.slice(-8)}</Badge>
            </CardTitle>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="grid grid-cols-2 gap-2">
              <Button
                variant="outline"
                onClick={() => setShowNewStepForm(!showNewStepForm)}
                className="flex items-center gap-2"
              >
                <Plus className="h-4 w-4" />
                Add Step
              </Button>
              <Button
                variant="outline"
                onClick={() => setShowFeedbackForm(!showFeedbackForm)}
                className="flex items-center gap-2"
              >
                <MessageSquare className="h-4 w-4" />
                Give Feedback
              </Button>
            </div>

            {/* ➕ NEW STEP FORM */}
            {showNewStepForm && (
              <Card className="border-dashed">
                <CardHeader>
                  <CardTitle className="text-sm">Add Reasoning Step</CardTitle>
                </CardHeader>
                <CardContent className="space-y-3">
                  <div>
                    <label className="text-sm font-medium text-gray-700 mb-1 block">
                      Step Type:
                    </label>
                    <div className="grid grid-cols-3 gap-2">
                      {stepTypes.map(type => {
                        const IconComponent = type.icon
                        return (
                          <Button
                            key={type.value}
                            variant={newStep.type === type.value ? "default" : "outline"}
                            size="sm"
                            onClick={() => setNewStep(prev => ({ ...prev, type: type.value as any }))}
                            className="flex flex-col h-16 p-2"
                          >
                            <IconComponent className="h-4 w-4 mb-1" />
                            <span className="text-xs">{type.label}</span>
                          </Button>
                        )
                      })}
                    </div>
                  </div>

                  <div>
                    <label className="text-sm font-medium text-gray-700 mb-1 block">
                      Content:
                    </label>
                    <Textarea
                      value={newStep.content}
                      onChange={(e) => setNewStep(prev => ({ ...prev, content: e.target.value }))}
                      placeholder="Describe your observation, hypothesis, analysis, etc."
                      className="w-full h-20"
                    />
                  </div>

                  <div>
                    <label className="text-sm font-medium text-gray-700 mb-1 block">
                      Reasoning (Why is this step important?):
                    </label>
                    <Textarea
                      value={newStep.reasoning}
                      onChange={(e) => setNewStep(prev => ({ ...prev, reasoning: e.target.value }))}
                      placeholder="Explain your reasoning behind this step..."
                      className="w-full h-16"
                    />
                  </div>

                  <div className="grid grid-cols-2 gap-3">
                    <div>
                      <label className="text-sm font-medium text-gray-700 mb-1 block">
                        Confidence:
                      </label>
                      <div className="flex items-center gap-2">
                        <input
                          type="range"
                          min="0"
                          max="1"
                          step="0.1"
                          value={newStep.confidence}
                          onChange={(e) => setNewStep(prev => ({ ...prev, confidence: Number(e.target.value) }))}
                          className="flex-1"
                        />
                        <span className="text-sm font-medium w-12">
                          {Math.round(newStep.confidence * 100)}%
                        </span>
                      </div>
                    </div>

                    <div>
                      <label className="text-sm font-medium text-gray-700 mb-1 block">
                        Evidence:
                      </label>
                      <Input
                        value={newStep.evidence}
                        onChange={(e) => setNewStep(prev => ({ ...prev, evidence: e.target.value }))}
                        placeholder="Supporting evidence..."
                        className="w-full"
                      />
                    </div>
                  </div>

                  <div className="flex gap-2">
                    <Button
                      onClick={handleAddStep}
                      disabled={!newStep.content.trim() || isProcessing}
                      className="flex-1"
                    >
                      {isProcessing ? 'Adding...' : 'Add Step'}
                    </Button>
                    <Button
                      variant="outline"
                      onClick={() => setShowNewStepForm(false)}
                    >
                      Cancel
                    </Button>
                  </div>
                </CardContent>
              </Card>
            )}

            {/* 💬 FEEDBACK FORM */}
            {showFeedbackForm && (
              <Card className="border-dashed">
                <CardHeader>
                  <CardTitle className="text-sm">Provide Human Insight</CardTitle>
                </CardHeader>
                <CardContent className="space-y-3">
                  <div>
                    <label className="text-sm font-medium text-gray-700 mb-1 block">
                      Feedback Type:
                    </label>
                    <div className="grid grid-cols-4 gap-2">
                      {feedbackTypes.map(type => {
                        const IconComponent = type.icon
                        return (
                          <Button
                            key={type.value}
                            variant={humanFeedback.type === type.value ? "default" : "outline"}
                            size="sm"
                            onClick={() => setHumanFeedback(prev => ({ ...prev, type: type.value as any }))}
                            className="flex flex-col h-14 p-1"
                          >
                            <IconComponent className="h-3 w-3 mb-1" />
                            <span className="text-xs">{type.label}</span>
                          </Button>
                        )
                      })}
                    </div>
                  </div>

                  <div>
                    <label className="text-sm font-medium text-gray-700 mb-1 block">
                      Your insight:
                    </label>
                    <Textarea
                      value={humanFeedback.content}
                      onChange={(e) => setHumanFeedback(prev => ({ ...prev, content: e.target.value }))}
                      placeholder="Share your domain knowledge, intuition, or corrections..."
                      className="w-full h-20"
                    />
                  </div>

                  <div className="flex gap-2">
                    <Button
                      onClick={handleSubmitFeedback}
                      disabled={!humanFeedback.content.trim() || isProcessing}
                      className="flex-1"
                    >
                      {isProcessing ? 'Submitting...' : 'Submit Feedback'}
                    </Button>
                    <Button
                      variant="outline"
                      onClick={() => setShowFeedbackForm(false)}
                    >
                      Cancel
                    </Button>
                  </div>
                </CardContent>
              </Card>
            )}
          </CardContent>
        </Card>
      )}

      {/* 🎓 REASONING PATTERNS & TIPS */}
      <ReasoningGuidance />

      {/* 🤝 COLLABORATION FEATURES */}
      {collaborative && chainId && (
        <CollaborativeReasoningPanel chainId={chainId} engine={engine} />
      )}
    </div>
  )
}

// 🎓 REASONING GUIDANCE COMPONENT
function ReasoningGuidance() {
  const [showGuidance, setShowGuidance] = useState(false)

  const reasoningTips = [
    {
      pattern: "Problem Decomposition",
      description: "Break complex problems into smaller, manageable parts",
      when: "Use when facing multi-faceted or overwhelming problems",
      steps: ["Identify overall scope", "Break into subproblems", "Solve each part", "Synthesize solutions"]
    },
    {
      pattern: "Evidence-Based Analysis", 
      description: "Build conclusions systematically from evidence",
      when: "Use for factual questions requiring verification",
      steps: ["Gather evidence", "Evaluate quality", "Form hypothesis", "Draw conclusions"]
    },
    {
      pattern: "Creative Exploration",
      description: "Generate and evaluate multiple alternatives",
      when: "Use for open-ended or creative challenges",
      steps: ["Understand constraints", "Generate alternatives", "Evaluate options", "Refine solution"]
    }
  ]

  return (
    <Card>
      <CardHeader>
        <CardTitle 
          className="flex items-center justify-between cursor-pointer"
          onClick={() => setShowGuidance(!showGuidance)}
        >
          <span className="flex items-center gap-2">
            <BookOpen className="h-5 w-5" />
            Reasoning Guidance
          </span>
          <Button variant="ghost" size="sm">
            {showGuidance ? '−' : '+'}
          </Button>
        </CardTitle>
      </CardHeader>
      
      {showGuidance && (
        <CardContent className="space-y-4">
          {reasoningTips.map((tip, index) => (
            <div key={index} className="border rounded-lg p-3">
              <div className="font-medium text-sm mb-1">{tip.pattern}</div>
              <div className="text-sm text-gray-600 mb-2">{tip.description}</div>
              <div className="text-xs text-blue-600 mb-2">
                <strong>When to use:</strong> {tip.when}
              </div>
              <div className="text-xs">
                <strong>Steps:</strong>
                <ol className="list-decimal list-inside text-gray-600 mt-1">
                  {tip.steps.map((step, i) => (
                    <li key={i}>{step}</li>
                  ))}
                </ol>
              </div>
            </div>
          ))}
        </CardContent>
      )}
    </Card>
  )
}

// 🤝 COLLABORATIVE REASONING PANEL
function CollaborativeReasoningPanel({ 
  chainId, 
  engine 
}: { 
  chainId: string
  engine: ChainOfThoughtEngine 
}) {
  const [collaborators, setCollaborators] = useState<string[]>([])
  const [inviteEmail, setInviteEmail] = useState('')

  const handleInviteCollaborator = async () => {
    if (!inviteEmail.trim()) return
    
    // Implementation would connect to collaboration system
    console.log('Inviting collaborator:', inviteEmail)
    setInviteEmail('')
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Users className="h-5 w-5" />
          Collaborative Reasoning
        </CardTitle>
      </CardHeader>
      <CardContent className="space-y-4">
        <div>
          <label className="text-sm font-medium text-gray-700 mb-1 block">
            Invite Collaborator:
          </label>
          <div className="flex gap-2">
            <Input
              value={inviteEmail}
              onChange={(e) => setInviteEmail(e.target.value)}
              placeholder="colleague@company.com"
              className="flex-1"
            />
            <Button onClick={handleInviteCollaborator} size="sm">
              Invite
            </Button>
          </div>
        </div>

        {collaborators.length > 0 && (
          <div>
            <div className="text-sm font-medium text-gray-700 mb-2">
              Active Collaborators:
            </div>
            <div className="flex flex-wrap gap-2">
              {collaborators.map((collaborator, index) => (
                <Badge key={index} variant="outline">
                  {collaborator}
                </Badge>
              ))}
            </div>
          </div>
        )}

        <div className="text-xs text-gray-500">
          💡 Tip: Collaborators can see your reasoning process in real-time and add their own insights
        </div>
      </CardContent>
    </Card>
  )
}