import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Play, Code, Database, Network } from 'lucide-react'

interface ExamplesPanelProps {
  ws: WebSocket | null
}

export default function ExamplesPanel({ ws }: ExamplesPanelProps) {
  const runExample = async (category: string, name: string) => {
    console.log(`Running example: ${category}/${name}`)
    
    if (category === 'streaming' && ws) {
      ws.send(JSON.stringify({
        type: 'run_example',
        category: category,
        name: name
      }))
    } else {
      try {
        const response = await fetch(`/api/examples/${category}`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ example: name })
        })
        
        const result = await response.json()
        console.log('Example result:', result)
      } catch (error) {
        console.error('Error running example:', error)
      }
    }
  }

  const examples = [
    {
      category: 'distributed',
      title: 'Distributed Examples',
      icon: Network,
      items: [
        {
          name: 'cluster',
          title: 'Distributed Cluster',
          description: 'Deploy agents across multiple Erlang nodes'
        },
        {
          name: 'research_team',
          title: 'Research Team',
          description: 'Collaborative AI agents working on research'
        },
        {
          name: 'data_pipeline',
          title: 'Data Pipeline',
          description: 'Distributed stream processing pipeline'
        },
        {
          name: 'swarm',
          title: 'Swarm Intelligence',
          description: 'Optimization using swarm algorithms'
        }
      ]
    },
    {
      category: 'streaming',
      title: 'Streaming Examples',
      icon: Database,
      items: [
        {
          name: 'pipeline',
          title: 'Stream Pipeline',
          description: 'Real-time data streaming with backpressure'
        },
        {
          name: 'realtime_chat',
          title: 'Real-time Chat',
          description: 'Streaming AI chat responses'
        },
        {
          name: 'batch_processor',
          title: 'Batch Processor',
          description: 'Parallel batch processing'
        },
        {
          name: 'event_system',
          title: 'Event System',
          description: 'Event-driven agent network'
        }
      ]
    },
    {
      category: 'composition',
      title: 'Tool Composition Examples',
      icon: Code,
      items: [
        {
          name: 'code_analysis',
          title: 'Code Analysis',
          description: 'Multi-stage code analysis pipeline'
        },
        {
          name: 'debugging',
          title: 'Auto Debugging',
          description: 'Autonomous debugging system'
        },
        {
          name: 'security_audit',
          title: 'Security Audit',
          description: 'Progressive security analysis'
        },
        {
          name: 'infrastructure',
          title: 'Infrastructure',
          description: 'Automated deployment orchestration'
        }
      ]
    }
  ]

  return (
    <div className="space-y-6">
      {examples.map(section => {
        const Icon = section.icon
        return (
          <div key={section.category}>
            <div className="flex items-center gap-2 mb-4">
              <Icon className="h-5 w-5" />
              <h3 className="text-lg font-semibold">{section.title}</h3>
            </div>
            <div className="grid gap-4 md:grid-cols-2">
              {section.items.map(example => (
                <Card key={example.name}>
                  <CardHeader>
                    <CardTitle className="text-base">{example.title}</CardTitle>
                    <CardDescription>{example.description}</CardDescription>
                  </CardHeader>
                  <CardContent>
                    <Button 
                      className="w-full" 
                      onClick={() => runExample(section.category, example.name)}
                    >
                      <Play className="h-4 w-4 mr-2" />
                      Run Example
                    </Button>
                  </CardContent>
                </Card>
              ))}
            </div>
          </div>
        )
      })}
    </div>
  )
}