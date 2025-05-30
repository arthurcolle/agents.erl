import { useState, useEffect, useRef } from 'react'
import { cn } from '@/lib/utils'
import { Button } from '@/components/ui/button'
import { Card } from '@/components/ui/card'
import { Input } from '@/components/ui/input'
import { 
  Play, 
  Download, 
  Maximize2, 
  Minimize2,
  Code,
  BarChart3,
  Terminal,
  Zap
} from 'lucide-react'

interface WidgetProps {
  id: string
  type: string
  data: any
  onInteraction: (action: string, data: any) => void
  onDataChange?: (newData: any) => void
}

// Code Editor Widget
export function CodeEditorWidget({ data, onInteraction }: WidgetProps) {
  const [code, setCode] = useState(data?.code || '')
  const [language, setLanguage] = useState(data?.language || 'javascript')
  const [isRunning, setIsRunning] = useState(false)
  const [output, setOutput] = useState('')
  const [isExpanded, setIsExpanded] = useState(false)

  const runCode = async () => {
    setIsRunning(true)
    setOutput('Running...')
    
    try {
      // Simulate code execution
      if (language === 'javascript') {
        const result = eval(code)
        setOutput(String(result))
      } else {
        setOutput(`Executed ${language} code:\n${code}`)
      }
    } catch (error: any) {
      setOutput(`Error: ${error.message}`)
    } finally {
      setIsRunning(false)
    }
    
    onInteraction('code_executed', { code, language, output })
  }

  return (
    <Card className={cn(
      "p-4 transition-all duration-300",
      isExpanded && "fixed inset-4 z-50 max-w-none max-h-none"
    )}>
      <div className="flex items-center justify-between mb-3">
        <div className="flex items-center gap-2">
          <Code className="h-4 w-4" />
          <span className="font-medium">Code Editor</span>
          <select 
            value={language} 
            onChange={(e) => setLanguage(e.target.value)}
            className="text-xs border rounded px-2 py-1"
          >
            <option value="javascript">JavaScript</option>
            <option value="python">Python</option>
            <option value="typescript">TypeScript</option>
            <option value="html">HTML</option>
            <option value="css">CSS</option>
          </select>
        </div>
        <div className="flex gap-2">
          <Button size="sm" onClick={runCode} disabled={isRunning}>
            <Play className="h-3 w-3" />
          </Button>
          <Button size="sm" variant="outline" onClick={() => setIsExpanded(!isExpanded)}>
            {isExpanded ? <Minimize2 className="h-3 w-3" /> : <Maximize2 className="h-3 w-3" />}
          </Button>
        </div>
      </div>
      
      <div className="space-y-3">
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          className="w-full h-32 font-mono text-sm border rounded-md p-3 bg-gray-900 text-green-400"
          placeholder="Enter your code here..."
        />
        
        {output && (
          <div className="bg-black text-white p-3 rounded-md font-mono text-sm">
            <div className="text-gray-400 text-xs mb-1">Output:</div>
            <pre>{output}</pre>
          </div>
        )}
      </div>
    </Card>
  )
}

// Data Visualization Widget
export function DataVisualizationWidget({ data, onInteraction }: WidgetProps) {
  const [chartType, setChartType] = useState(data?.chartType || 'bar')
  const [dataset, setDataset] = useState(data?.dataset || [
    { name: 'A', value: 20 },
    { name: 'B', value: 40 },
    { name: 'C', value: 60 },
    { name: 'D', value: 30 }
  ])
  const [isInteractive] = useState(true)

  const maxValue = Math.max(...dataset.map((d: any) => d.value))

  const renderChart = () => {
    switch (chartType) {
      case 'bar':
        return (
          <div className="space-y-2">
            {dataset.map((item, index) => (
              <div key={index} className="flex items-center gap-3">
                <span className="w-8 text-sm">{item.name}</span>
                <div className="flex-1 bg-gray-200 rounded-full h-6 relative">
                  <div
                    className="bg-blue-500 h-full rounded-full transition-all duration-500"
                    style={{ width: `${(item.value / maxValue) * 100}%` }}
                  />
                  <span className="absolute right-2 top-0 h-full flex items-center text-xs text-gray-700">
                    {item.value}
                  </span>
                </div>
              </div>
            ))}
          </div>
        )
      case 'line':
        return (
          <svg width="100%" height="200" className="border rounded">
            <polyline
              fill="none"
              stroke="blue"
              strokeWidth="2"
              points={dataset.map((d, i) => 
                `${(i / (dataset.length - 1)) * 300},${200 - (d.value / maxValue) * 180}`
              ).join(' ')}
            />
            {dataset.map((d, i) => (
              <circle
                key={i}
                cx={(i / (dataset.length - 1)) * 300}
                cy={200 - (d.value / maxValue) * 180}
                r="4"
                fill="blue"
                className="cursor-pointer hover:r-6"
                onClick={() => onInteraction('data_point_clicked', { point: d, index: i })}
              />
            ))}
          </svg>
        )
      case 'pie':
        const total = dataset.reduce((sum, d) => sum + d.value, 0)
        let currentAngle = 0
        return (
          <svg width="200" height="200" className="mx-auto">
            {dataset.map((d, i) => {
              const startAngle = currentAngle
              const endAngle = currentAngle + (d.value / total) * 360
              currentAngle = endAngle
              
              const x1 = 100 + 80 * Math.cos((startAngle - 90) * Math.PI / 180)
              const y1 = 100 + 80 * Math.sin((startAngle - 90) * Math.PI / 180)
              const x2 = 100 + 80 * Math.cos((endAngle - 90) * Math.PI / 180)
              const y2 = 100 + 80 * Math.sin((endAngle - 90) * Math.PI / 180)
              
              const largeArcFlag = endAngle - startAngle > 180 ? 1 : 0
              
              return (
                <path
                  key={i}
                  d={`M 100 100 L ${x1} ${y1} A 80 80 0 ${largeArcFlag} 1 ${x2} ${y2} Z`}
                  fill={`hsl(${i * 60}, 70%, 50%)`}
                  className="cursor-pointer hover:opacity-80"
                  onClick={() => onInteraction('segment_clicked', { segment: d, index: i })}
                />
              )
            })}
          </svg>
        )
      default:
        return <div>Unsupported chart type</div>
    }
  }

  return (
    <Card className="p-4">
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center gap-2">
          <BarChart3 className="h-4 w-4" />
          <span className="font-medium">Data Visualization</span>
        </div>
        <div className="flex gap-2">
          <select 
            value={chartType} 
            onChange={(e) => setChartType(e.target.value)}
            className="text-xs border rounded px-2 py-1"
          >
            <option value="bar">Bar Chart</option>
            <option value="line">Line Chart</option>
            <option value="pie">Pie Chart</option>
          </select>
          <Button 
            size="sm" 
            variant="outline"
            onClick={() => onInteraction('export_chart', { chartType, dataset })}
          >
            <Download className="h-3 w-3" />
          </Button>
        </div>
      </div>
      
      <div className="mb-4">
        {renderChart()}
      </div>
      
      {isInteractive && (
        <div className="space-y-2">
          <div className="text-sm font-medium">Dataset:</div>
          {dataset.map((item, index) => (
            <div key={index} className="flex gap-2">
              <Input
                value={item.name}
                onChange={(e) => {
                  const newDataset = [...dataset]
                  newDataset[index].name = e.target.value
                  setDataset(newDataset)
                }}
                className="w-20 h-8 text-xs"
              />
              <Input
                type="number"
                value={item.value}
                onChange={(e) => {
                  const newDataset = [...dataset]
                  newDataset[index].value = parseInt(e.target.value) || 0
                  setDataset(newDataset)
                }}
                className="w-20 h-8 text-xs"
              />
            </div>
          ))}
          <Button 
            size="sm" 
            onClick={() => setDataset([...dataset, { name: 'New', value: 10 }])}
          >
            Add Data Point
          </Button>
        </div>
      )}
    </Card>
  )
}

// Math Calculator Widget
export function MathCalculatorWidget({ id, data, onInteraction }: WidgetProps) {
  const [expression, setExpression] = useState(data?.expression || '')
  const [result, setResult] = useState('')
  const [history, setHistory] = useState<string[]>([])
  const [mode, setMode] = useState<'simple' | 'scientific'>('simple')

  const calculate = () => {
    try {
      // Simple math evaluation (in production, use a proper math parser)
      const calculatedResult = eval(expression.replace(/\^/g, '**'))
      const resultStr = String(calculatedResult)
      setResult(resultStr)
      setHistory(prev => [`${expression} = ${resultStr}`, ...prev.slice(0, 9)])
      onInteraction('calculation_performed', { expression, result: resultStr })
    } catch (error) {
      setResult('Error')
    }
  }

  const insertSymbol = (symbol: string) => {
    setExpression(prev => prev + symbol)
  }

  const scientificButtons = [
    'sin(', 'cos(', 'tan(', 'log(',
    'sqrt(', '^2', '^', 'π',
    'e', '(', ')', 'abs('
  ]

  return (
    <Card className="p-4">
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center gap-2">
          <Zap className="h-4 w-4" />
          <span className="font-medium">Math Calculator</span>
        </div>
        <div className="flex gap-2">
          <Button
            size="sm"
            variant={mode === 'simple' ? 'default' : 'outline'}
            onClick={() => setMode('simple')}
          >
            Simple
          </Button>
          <Button
            size="sm"
            variant={mode === 'scientific' ? 'default' : 'outline'}
            onClick={() => setMode('scientific')}
          >
            Scientific
          </Button>
        </div>
      </div>

      <div className="space-y-3">
        <Input
          value={expression}
          onChange={(e) => setExpression(e.target.value)}
          onKeyPress={(e) => e.key === 'Enter' && calculate()}
          placeholder="Enter mathematical expression..."
          className="font-mono"
        />
        
        {result && (
          <div className="bg-blue-50 border border-blue-200 rounded p-3">
            <div className="text-sm text-blue-600">Result:</div>
            <div className="text-lg font-mono">{result}</div>
          </div>
        )}

        <div className="grid grid-cols-4 gap-2">
          {['7', '8', '9', '/'].map(btn => (
            <Button key={btn} size="sm" onClick={() => insertSymbol(btn)}>
              {btn}
            </Button>
          ))}
          {['4', '5', '6', '*'].map(btn => (
            <Button key={btn} size="sm" onClick={() => insertSymbol(btn)}>
              {btn}
            </Button>
          ))}
          {['1', '2', '3', '-'].map(btn => (
            <Button key={btn} size="sm" onClick={() => insertSymbol(btn)}>
              {btn}
            </Button>
          ))}
          {['0', '.', '=', '+'].map(btn => (
            <Button 
              key={btn} 
              size="sm" 
              onClick={() => btn === '=' ? calculate() : insertSymbol(btn)}
              variant={btn === '=' ? 'default' : 'outline'}
            >
              {btn}
            </Button>
          ))}
        </div>

        {mode === 'scientific' && (
          <div className="grid grid-cols-4 gap-2 mt-3">
            {scientificButtons.map(btn => (
              <Button key={btn} size="sm" variant="outline" onClick={() => insertSymbol(btn)}>
                {btn}
              </Button>
            ))}
          </div>
        )}

        {history.length > 0 && (
          <div className="space-y-1">
            <div className="text-sm font-medium">History:</div>
            <div className="max-h-20 overflow-y-auto space-y-1">
              {history.map((entry, index) => (
                <div key={index} className="text-xs font-mono bg-gray-100 p-1 rounded">
                  {entry}
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </Card>
  )
}

// Terminal Widget
export function TerminalWidget({ id, data, onInteraction }: WidgetProps) {
  const [commands, setCommands] = useState<Array<{command: string, output: string}>>([])
  const [currentCommand, setCurrentCommand] = useState('')
  const [isProcessing, setIsProcessing] = useState(false)
  const terminalRef = useRef<HTMLDivElement>(null)

  const executeCommand = async () => {
    if (!currentCommand.trim()) return

    setIsProcessing(true)
    const command = currentCommand.trim()
    
    // Simulate command execution
    let output = ''
    switch (command.split(' ')[0]) {
      case 'help':
        output = 'Available commands: help, date, echo, ls, clear, pwd'
        break
      case 'date':
        output = new Date().toString()
        break
      case 'echo':
        output = command.slice(5)
        break
      case 'ls':
        output = 'file1.txt  file2.js  directory/  README.md'
        break
      case 'pwd':
        output = '/home/user/workspace'
        break
      case 'clear':
        setCommands([])
        setCurrentCommand('')
        setIsProcessing(false)
        return
      default:
        output = `Command not found: ${command}`
    }

    setCommands(prev => [...prev, { command, output }])
    setCurrentCommand('')
    setIsProcessing(false)
    
    onInteraction('terminal_command', { command, output })
  }

  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.scrollTop = terminalRef.current.scrollHeight
    }
  }, [commands])

  return (
    <Card className="p-4 bg-black text-green-400 font-mono">
      <div className="flex items-center justify-between mb-3">
        <div className="flex items-center gap-2">
          <Terminal className="h-4 w-4" />
          <span className="font-medium">Terminal</span>
        </div>
        <Button
          size="sm"
          variant="outline"
          onClick={() => setCommands([])}
          className="text-green-400 border-green-400"
        >
          Clear
        </Button>
      </div>

      <div 
        ref={terminalRef}
        className="h-60 overflow-y-auto bg-black border border-green-400 rounded p-3 space-y-1"
      >
        {commands.map((entry, index) => (
          <div key={index}>
            <div className="text-green-300">$ {entry.command}</div>
            <div className="text-green-100 whitespace-pre-wrap">{entry.output}</div>
          </div>
        ))}
        
        <div className="flex items-center">
          <span className="text-green-300">$ </span>
          <input
            value={currentCommand}
            onChange={(e) => setCurrentCommand(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && executeCommand()}
            className="flex-1 bg-transparent border-none outline-none text-green-400 ml-1"
            placeholder="Enter command..."
            disabled={isProcessing}
          />
          {isProcessing && <span className="animate-pulse">_</span>}
        </div>
      </div>
    </Card>
  )
}

// Function Plotter Widget
export function FunctionPlotterWidget({ id, data, onInteraction }: WidgetProps) {
  const [expression, setExpression] = useState(data?.expression || 'x^2')
  const [xMin, setXMin] = useState(-10)
  const [xMax, setXMax] = useState(10)
  const [points, setPoints] = useState<Array<{x: number, y: number}>>([])

  const plotFunction = () => {
    const newPoints = []
    const step = (xMax - xMin) / 200

    try {
      for (let x = xMin; x <= xMax; x += step) {
        const expr = expression
          .replace(/\^/g, '**')
          .replace(/x/g, x.toString())
        const y = eval(expr)
        
        if (isFinite(y)) {
          newPoints.push({ x, y })
        }
      }
      setPoints(newPoints)
      onInteraction('function_plotted', { expression, xMin, xMax, points: newPoints })
    } catch (error) {
      console.error('Error plotting function:', error)
    }
  }

  useEffect(() => {
    plotFunction()
  }, [expression, xMin, xMax])

  const yValues = points.map(p => p.y).filter(y => isFinite(y))
  const yMin = Math.min(...yValues)
  const yMax = Math.max(...yValues)

  return (
    <Card className="p-4">
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center gap-2">
          <BarChart3 className="h-4 w-4" />
          <span className="font-medium">Function Plotter</span>
        </div>
      </div>

      <div className="space-y-4">
        <div className="flex gap-2">
          <Input
            value={expression}
            onChange={(e) => setExpression(e.target.value)}
            placeholder="Enter function (e.g., x^2, sin(x))"
            className="flex-1"
          />
          <Button onClick={plotFunction}>Plot</Button>
        </div>

        <div className="flex gap-2">
          <Input
            type="number"
            value={xMin}
            onChange={(e) => setXMin(Number(e.target.value))}
            placeholder="X Min"
            className="w-20"
          />
          <Input
            type="number"
            value={xMax}
            onChange={(e) => setXMax(Number(e.target.value))}
            placeholder="X Max"
            className="w-20"
          />
        </div>

        <div className="bg-white border rounded">
          <svg width="100%" height="300" viewBox="0 0 400 300">
            {/* Grid */}
            <defs>
              <pattern id="grid" width="20" height="20" patternUnits="userSpaceOnUse">
                <path d="M 20 0 L 0 0 0 20" fill="none" stroke="#e5e5e5" strokeWidth="1"/>
              </pattern>
            </defs>
            <rect width="100%" height="100%" fill="url(#grid)" />
            
            {/* Axes */}
            <line x1="0" y1="150" x2="400" y2="150" stroke="#666" strokeWidth="2" />
            <line x1="200" y1="0" x2="200" y2="300" stroke="#666" strokeWidth="2" />
            
            {/* Function curve */}
            {points.length > 1 && (
              <polyline
                fill="none"
                stroke="blue"
                strokeWidth="2"
                points={points.map(p => {
                  const screenX = ((p.x - xMin) / (xMax - xMin)) * 400
                  const screenY = 300 - ((p.y - yMin) / (yMax - yMin)) * 300
                  return `${screenX},${screenY}`
                }).join(' ')}
              />
            )}
          </svg>
        </div>

        <div className="text-sm text-gray-600">
          Function: y = {expression} | Range: [{xMin}, {xMax}]
        </div>
      </div>
    </Card>
  )
}

// Widget Factory
export function createWidget(type: string, props: WidgetProps) {
  switch (type) {
    case 'code-editor':
      return <CodeEditorWidget {...props} />
    case 'data-visualization':
      return <DataVisualizationWidget {...props} />
    case 'math-calculator':
      return <MathCalculatorWidget {...props} />
    case 'terminal':
      return <TerminalWidget {...props} />
    case 'function-plotter':
      return <FunctionPlotterWidget {...props} />
    default:
      return (
        <Card className="p-4">
          <div className="text-center text-gray-500">
            Unknown widget type: {type}
          </div>
        </Card>
      )
  }
}