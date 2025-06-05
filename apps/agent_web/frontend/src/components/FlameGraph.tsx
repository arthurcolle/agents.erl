import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Flame, Clock, Zap } from 'lucide-react'
import { cn } from '@/lib/utils'

interface FlameNode {
  name: string
  value: number
  children?: FlameNode[]
  depth: number
  start: number
  end: number
}

interface FlameGraphProps {
  data?: FlameNode[]
  height?: number
  className?: string
}

export function FlameGraph({ data = [], height = 400, className }: FlameGraphProps) {
  const [selectedNode, setSelectedNode] = useState<FlameNode | null>(null)
  const [hoveredNode, setHoveredNode] = useState<FlameNode | null>(null)
  const canvasRef = useRef<HTMLCanvasElement>(null)

  // Generate mock flame graph data if none provided
  const generateMockData = (): FlameNode[] => {
    return [
      {
        name: 'agent_main',
        value: 1000,
        depth: 0,
        start: 0,
        end: 1000,
        children: [
          {
            name: 'openai_call',
            value: 400,
            depth: 1,
            start: 0,
            end: 400,
            children: [
              { name: 'http_request', value: 300, depth: 2, start: 0, end: 300 },
              { name: 'json_parse', value: 100, depth: 2, start: 300, end: 400 }
            ]
          },
          {
            name: 'tool_execution',
            value: 300,
            depth: 1,
            start: 400,
            end: 700,
            children: [
              { name: 'file_read', value: 150, depth: 2, start: 400, end: 550 },
              { name: 'data_process', value: 150, depth: 2, start: 550, end: 700 }
            ]
          },
          {
            name: 'response_format',
            value: 300,
            depth: 1,
            start: 700,
            end: 1000,
            children: [
              { name: 'markdown_render', value: 200, depth: 2, start: 700, end: 900 },
              { name: 'websocket_send', value: 100, depth: 2, start: 900, end: 1000 }
            ]
          }
        ]
      }
    ]
  }

  const flameData = data.length > 0 ? data : generateMockData()

  const getColor = (depth: number, intensity: number) => {
    const colors = [
      '#dc2626', // red-600
      '#ea580c', // orange-600  
      '#ca8a04', // yellow-600
      '#16a34a', // green-600
      '#2563eb', // blue-600
      '#7c3aed', // violet-600
      '#c2410c', // orange-700
    ]
    return colors[depth % colors.length]
  }

  const flattenNodes = (nodes: FlameNode[]): FlameNode[] => {
    const result: FlameNode[] = []
    
    const traverse = (nodeList: FlameNode[]) => {
      nodeList.forEach(node => {
        result.push(node)
        if (node.children) {
          traverse(node.children)
        }
      })
    }
    
    traverse(nodes)
    return result
  }

  const allNodes = flattenNodes(flameData)
  const maxDepth = Math.max(...allNodes.map(n => n.depth))
  const maxValue = Math.max(...allNodes.map(n => n.end))

  const FlameRect = ({ node }: { node: FlameNode }) => {
    const width = ((node.end - node.start) / maxValue) * 100
    const left = (node.start / maxValue) * 100
    const rectHeight = height / (maxDepth + 1)
    const top = node.depth * rectHeight
    
    return (
      <div
        className={cn(
          "absolute border border-white/20 cursor-pointer transition-all",
          "hover:border-white/60 hover:brightness-110",
          selectedNode === node && "ring-2 ring-white/80"
        )}
        style={{
          left: `${left}%`,
          top: `${top}px`,
          width: `${width}%`,
          height: `${rectHeight - 1}px`,
          backgroundColor: getColor(node.depth, node.value),
        }}
        onClick={() => setSelectedNode(selectedNode === node ? null : node)}
        onMouseEnter={() => setHoveredNode(node)}
        onMouseLeave={() => setHoveredNode(null)}
        title={`${node.name}: ${node.value}ms`}
      >
        {width > 8 && (
          <div className="px-1 py-0.5 text-white text-xs font-medium truncate">
            {node.name}
          </div>
        )}
      </div>
    )
  }

  return (
    <Card className={cn("", className)}>
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <CardTitle className="text-base flex items-center gap-2">
            <Flame className="h-4 w-4" />
            Request Flame Graph
          </CardTitle>
          <div className="flex items-center gap-2">
            {selectedNode && (
              <Button
                variant="outline"
                size="sm"
                onClick={() => setSelectedNode(null)}
              >
                Clear Selection
              </Button>
            )}
          </div>
        </div>
      </CardHeader>
      
      <CardContent className="pt-0">
        <div className="space-y-4">
          {/* Flame Graph Visualization */}
          <div 
            className="relative bg-muted/20 rounded border"
            style={{ height: `${height}px` }}
          >
            {allNodes.map((node, index) => (
              <FlameRect key={`${node.name}-${index}`} node={node} />
            ))}
          </div>

          {/* Details Panel */}
          <div className="grid gap-4 md:grid-cols-2">
            {/* Hovered Node Info */}
            {hoveredNode && (
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm">Hovered Function</CardTitle>
                </CardHeader>
                <CardContent className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Function:</span>
                    <code className="text-xs">{hoveredNode.name}</code>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Duration:</span>
                    <span>{hoveredNode.value}ms</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Depth:</span>
                    <span>{hoveredNode.depth}</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Percentage:</span>
                    <span>{((hoveredNode.value / maxValue) * 100).toFixed(1)}%</span>
                  </div>
                </CardContent>
              </Card>
            )}

            {/* Selected Node Details */}
            {selectedNode && (
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle className="text-sm flex items-center gap-2">
                    <Zap className="h-3 w-3" />
                    Selected Function
                  </CardTitle>
                </CardHeader>
                <CardContent className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Function:</span>
                    <code className="text-xs font-bold">{selectedNode.name}</code>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Duration:</span>
                    <Badge variant="default">{selectedNode.value}ms</Badge>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Start Time:</span>
                    <span>{selectedNode.start}ms</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">End Time:</span>
                    <span>{selectedNode.end}ms</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Children:</span>
                    <span>{selectedNode.children?.length || 0}</span>
                  </div>
                </CardContent>
              </Card>
            )}
          </div>

          {/* Legend */}
          <div className="flex items-center justify-between text-sm text-muted-foreground">
            <div className="flex items-center gap-4">
              <div className="flex items-center gap-2">
                <Clock className="h-3 w-3" />
                <span>Total Time: {maxValue}ms</span>
              </div>
              <div>Max Depth: {maxDepth + 1}</div>
            </div>
            <div className="text-xs">
              Click to select • Hover for details
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}