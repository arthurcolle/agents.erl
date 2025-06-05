import { useState, useEffect } from 'react'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Progress } from '@/components/ui/progress'
import { ScrollArea } from '@/components/ui/scroll-area'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { 
  Users, Vote, Clock, CheckCircle, XCircle, AlertCircle,
  GitBranch, Zap, Target, TrendingUp, Hash, Play, Bot
} from 'lucide-react'

interface QuorumDecision {
  id: string
  topic: string
  proposer: string
  participants: string[]
  votes: Map<string, { vote: 'yes' | 'no' | 'abstain', reason?: string }>
  status: 'proposed' | 'voting' | 'decided' | 'failed'
  result?: 'approved' | 'rejected'
  threshold: number
  deadline: number
  createdAt: number
}

interface ConsensusMetrics {
  totalDecisions: number
  approved: number
  rejected: number
  activeVotes: number
  avgParticipation: number
  avgConsensusTime: number
}

export default function AgentQuorumPanel({ agents, ws }: { agents: Map<string, any>, ws: WebSocket | null }) {
  const [decisions, setDecisions] = useState<Map<string, QuorumDecision>>(new Map())
  const [selectedAgents, setSelectedAgents] = useState<string[]>([])
  const [newTopic, setNewTopic] = useState('')
  const [threshold, setThreshold] = useState(66)
  const [metrics, setMetrics] = useState<ConsensusMetrics>({
    totalDecisions: 0,
    approved: 0,
    rejected: 0,
    activeVotes: 0,
    avgParticipation: 0,
    avgConsensusTime: 0
  })

  useEffect(() => {
    const handleQuorumUpdate = (event: any) => {
      const data = event.detail
      if (data.type === 'quorum_update') {
        const decision = data.decision as QuorumDecision
        setDecisions(prev => new Map(prev).set(decision.id, decision))
        updateMetrics()
      }
    }

    window.addEventListener('agent_stream', handleQuorumUpdate)
    return () => window.removeEventListener('agent_stream', handleQuorumUpdate)
  }, [])

  const updateMetrics = () => {
    const allDecisions = Array.from(decisions.values())
    const completed = allDecisions.filter(d => d.status === 'decided')
    const approved = completed.filter(d => d.result === 'approved')
    const active = allDecisions.filter(d => d.status === 'voting')

    setMetrics({
      totalDecisions: allDecisions.length,
      approved: approved.length,
      rejected: completed.length - approved.length,
      activeVotes: active.length,
      avgParticipation: allDecisions.length > 0
        ? allDecisions.reduce((acc, d) => acc + d.votes.size / d.participants.length, 0) / allDecisions.length * 100
        : 0,
      avgConsensusTime: completed.length > 0
        ? completed.reduce((acc, d) => acc + (d.deadline - d.createdAt), 0) / completed.length / 1000
        : 0
    })
  }

  const proposeDecision = async () => {
    if (!newTopic || selectedAgents.length < 2) return

    try {
      const response = await fetch('/api/agents/quorum/propose', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          topic: newTopic,
          proposer: selectedAgents[0],
          participants: selectedAgents,
          threshold: threshold / 100,
          deadline: Date.now() + 60000 // 1 minute deadline
        })
      })

      if (response.ok) {
        const decision = await response.json()
        setDecisions(prev => new Map(prev).set(decision.id, decision))
        setNewTopic('')
        updateMetrics()
      }
    } catch (error) {
      console.error('Failed to propose decision:', error)
    }
  }

  const castVote = async (decisionId: string, agentId: string, vote: 'yes' | 'no' | 'abstain') => {
    try {
      await fetch(`/api/agents/quorum/${decisionId}/vote`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ agentId, vote })
      })
    } catch (error) {
      console.error('Failed to cast vote:', error)
    }
  }

  const getAgentName = (id: string) => {
    const agent = agents.get(id)
    return agent?.name || agent?.type || id.substring(0, 8)
  }

  const getVoteProgress = (decision: QuorumDecision) => {
    const totalVotes = decision.votes.size
    const yesVotes = Array.from(decision.votes.values()).filter(v => v.vote === 'yes').length
    const progress = (yesVotes / decision.participants.length) * 100
    const thresholdMet = progress >= decision.threshold * 100
    
    return { progress, yesVotes, totalVotes, thresholdMet }
  }

  return (
    <div className="h-full flex flex-col space-y-2">
      {/* Metrics Bar */}
      <div className="grid grid-cols-6 gap-1 text-xs">
        <Badge variant="outline" className="py-0 px-1">
          <Hash className="h-3 w-3 mr-1" />
          {metrics.totalDecisions} total
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <CheckCircle className="h-3 w-3 mr-1 text-green-500" />
          {metrics.approved} approved
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <XCircle className="h-3 w-3 mr-1 text-red-500" />
          {metrics.rejected} rejected
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <Vote className="h-3 w-3 mr-1 text-yellow-500" />
          {metrics.activeVotes} active
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <Users className="h-3 w-3 mr-1" />
          {metrics.avgParticipation.toFixed(0)}% avg
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <Clock className="h-3 w-3 mr-1" />
          {metrics.avgConsensusTime.toFixed(1)}s
        </Badge>
      </div>

      {/* New Decision Form */}
      <Card className="p-2 space-y-2">
        <div className="flex gap-1">
          <Input
            value={newTopic}
            onChange={(e) => setNewTopic(e.target.value)}
            placeholder="Decision topic..."
            className="h-7 text-xs flex-1"
          />
          <div className="flex items-center gap-1">
            <Target className="h-3 w-3 text-gray-500" />
            <Input
              type="number"
              value={threshold}
              onChange={(e) => setThreshold(Number(e.target.value))}
              className="h-7 text-xs w-16"
              min={51}
              max={100}
            />
            <span className="text-xs text-gray-500">%</span>
          </div>
          <Button
            size="sm"
            onClick={proposeDecision}
            disabled={!newTopic || selectedAgents.length < 2}
            className="h-7 px-2"
          >
            <Play className="h-3 w-3 mr-1" />
            Propose
          </Button>
        </div>
        
        <div className="flex flex-wrap gap-1">
          {Array.from(agents.values()).map(agent => (
            <Button
              key={agent.id}
              size="sm"
              variant={selectedAgents.includes(agent.id) ? "default" : "outline"}
              onClick={() => {
                setSelectedAgents(prev =>
                  prev.includes(agent.id)
                    ? prev.filter(id => id !== agent.id)
                    : [...prev, agent.id]
                )
              }}
              className="h-6 px-2 text-xs"
            >
              {agent.name || agent.type}
            </Button>
          ))}
        </div>
      </Card>

      {/* Active Decisions */}
      <ScrollArea className="flex-1 border border-gray-800 rounded">
        <div className="p-2 space-y-2">
          {Array.from(decisions.values())
            .sort((a, b) => b.createdAt - a.createdAt)
            .map(decision => {
              const voteStats = getVoteProgress(decision)
              const timeLeft = Math.max(0, decision.deadline - Date.now())
              
              return (
                <Card key={decision.id} className="p-2 space-y-2">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-2">
                        <h4 className="text-sm font-medium">{decision.topic}</h4>
                        <Badge 
                          variant={
                            decision.status === 'decided' 
                              ? decision.result === 'approved' ? 'default' : 'destructive'
                              : decision.status === 'voting' ? 'secondary' : 'outline'
                          }
                          className="text-xs py-0"
                        >
                          {decision.status}
                        </Badge>
                      </div>
                      <div className="text-xs text-gray-500">
                        Proposed by {getAgentName(decision.proposer)}
                      </div>
                    </div>
                    {decision.status === 'voting' && (
                      <div className="flex items-center gap-1 text-xs text-gray-500">
                        <Clock className="h-3 w-3" />
                        {Math.floor(timeLeft / 1000)}s
                      </div>
                    )}
                  </div>

                  {/* Vote Progress */}
                  <div className="space-y-1">
                    <div className="flex items-center justify-between text-xs">
                      <span className="text-gray-500">
                        {voteStats.yesVotes}/{decision.participants.length} votes 
                        ({voteStats.progress.toFixed(0)}%)
                      </span>
                      <span className={`font-medium ${
                        voteStats.thresholdMet ? 'text-green-500' : 'text-gray-500'
                      }`}>
                        Threshold: {(decision.threshold * 100).toFixed(0)}%
                      </span>
                    </div>
                    <Progress 
                      value={voteStats.progress} 
                      className="h-2"
                    />
                  </div>

                  {/* Participant Votes */}
                  <div className="grid grid-cols-4 gap-1">
                    {decision.participants.map(agentId => {
                      const vote = decision.votes.get(agentId)
                      return (
                        <div
                          key={agentId}
                          className={`flex items-center gap-1 p-1 rounded text-xs ${
                            vote
                              ? vote.vote === 'yes' ? 'bg-green-900/20'
                              : vote.vote === 'no' ? 'bg-red-900/20'
                              : 'bg-gray-900/20'
                              : 'bg-gray-900/50'
                          }`}
                        >
                          <Bot className="h-3 w-3" />
                          <span className="truncate flex-1">
                            {getAgentName(agentId)}
                          </span>
                          {vote ? (
                            vote.vote === 'yes' ? <CheckCircle className="h-3 w-3 text-green-500" />
                            : vote.vote === 'no' ? <XCircle className="h-3 w-3 text-red-500" />
                            : <AlertCircle className="h-3 w-3 text-gray-500" />
                          ) : (
                            decision.status === 'voting' && (
                              <div className="flex gap-0.5">
                                <button
                                  onClick={() => castVote(decision.id, agentId, 'yes')}
                                  className="hover:bg-green-600 p-0.5 rounded"
                                >
                                  <CheckCircle className="h-3 w-3" />
                                </button>
                                <button
                                  onClick={() => castVote(decision.id, agentId, 'no')}
                                  className="hover:bg-red-600 p-0.5 rounded"
                                >
                                  <XCircle className="h-3 w-3" />
                                </button>
                              </div>
                            )
                          )}
                        </div>
                      )
                    })}
                  </div>
                </Card>
              )
            })}
        </div>
      </ScrollArea>
    </div>
  )
}