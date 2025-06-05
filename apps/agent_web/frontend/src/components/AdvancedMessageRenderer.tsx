import { useState, useEffect, useMemo, useRef } from 'react'
import { cn } from '@/lib/utils'

interface AdvancedMessageRendererProps {
  content: string
  sender: 'user' | 'agent'
  messageId: string
  onContentAnalysis?: (analysis: ContentAnalysis) => void
}

interface ContentAnalysis {
  type: 'code' | 'data' | 'math' | 'text' | 'mixed'
  language?: string
  complexity: 'simple' | 'medium' | 'complex'
  hasInteractiveElements: boolean
  suggestedActions: string[]
  extractedEntities: string[]
  topics: string[]
}


export default function AdvancedMessageRenderer({ 
  content, 
  messageId, 
  onContentAnalysis 
}: AdvancedMessageRendererProps) {
  const [analysis, setAnalysis] = useState<ContentAnalysis | null>(null)
  const containerRef = useRef<HTMLDivElement>(null)

  // Advanced content parsing and analysis
  const { parsedContent, contentAnalysis } = useMemo(() => {
    const analyze = analyzeContent(content)
    const parsed = parseAdvancedContent(content)
    return { parsedContent: parsed, contentAnalysis: analyze }
  }, [content])

  useEffect(() => {
    setAnalysis(contentAnalysis)
    onContentAnalysis?.(contentAnalysis)
  }, [contentAnalysis, onContentAnalysis])

  // Analyze content for type, complexity, and features
  function analyzeContent(text: string): ContentAnalysis {
    const codeBlocks = text.match(/```[\s\S]*?```/g) || []
    const inlineCode = text.match(/`[^`]+`/g) || []
    const mathBlocks = text.match(/\$\$[\s\S]*?\$\$/g) || []
    const inlineMath = text.match(/\$[^$]+\$/g) || []
    const jsonLike = text.match(/\{[\s\S]*\}/g) || []
    const csvLike = text.match(/[^,\n]+(?:,[^,\n]+)+/g) || []

    const hasCode = codeBlocks.length > 0 || inlineCode.length > 0
    const hasMath = mathBlocks.length > 0 || inlineMath.length > 0
    const hasData = jsonLike.length > 0 || csvLike.length > 0

    let type: ContentAnalysis['type'] = 'text'
    if (hasCode && hasMath && hasData) type = 'mixed'
    else if (hasCode) type = 'code'
    else if (hasMath) type = 'math'
    else if (hasData) type = 'data'

    const complexity = calculateComplexity(text, codeBlocks, mathBlocks)
    const entities = extractEntities(text)
    const topics = extractTopics(text)
    const suggestedActions = generateSuggestions(type, complexity, hasCode, hasMath, hasData)

    return {
      type,
      complexity,
      hasInteractiveElements: hasCode || hasMath || hasData,
      suggestedActions,
      extractedEntities: entities,
      topics,
      language: detectLanguage(codeBlocks[0])
    }
  }

  function calculateComplexity(text: string, codeBlocks: string[], mathBlocks: string[]): 'simple' | 'medium' | 'complex' {
    const wordCount = text.split(/\s+/).length
    const codeComplexity = codeBlocks.reduce((acc, block) => acc + block.split('\n').length, 0)
    const mathComplexity = mathBlocks.length * 10

    const totalComplexity = wordCount + codeComplexity + mathComplexity
    
    if (totalComplexity > 500) return 'complex'
    if (totalComplexity > 100) return 'medium'
    return 'simple'
  }

  function extractEntities(text: string): string[] {
    const entities = new Set<string>()
    
    // Extract URLs
    const urls = text.match(/https?:\/\/[^\s]+/g) || []
    urls.forEach(url => entities.add(url))

    // Extract file paths
    const filePaths = text.match(/[./~]?[\w-./]+\.(js|ts|py|cpp|java|html|css|json|md|txt|erl)/g) || []
    filePaths.forEach(path => entities.add(path))

    // Extract function names
    const functions = text.match(/\b\w+\(/g) || []
    functions.forEach((fn: string) => entities.add(fn.slice(0, -1)))

    return Array.from(entities)
  }

  function extractTopics(text: string): string[] {
    const topics = new Set<string>()
    
    // Programming languages
    const langKeywords = ['javascript', 'typescript', 'python', 'java', 'cpp', 'rust', 'go', 'erlang', 'react', 'vue', 'angular']
    langKeywords.forEach(lang => {
      if (text.toLowerCase().includes(lang)) topics.add(lang)
    })

    // Math topics
    const mathKeywords = ['algorithm', 'function', 'equation', 'matrix', 'vector', 'calculus', 'linear algebra']
    mathKeywords.forEach(topic => {
      if (text.toLowerCase().includes(topic)) topics.add(topic)
    })

    return Array.from(topics)
  }

  function generateSuggestions(type: string, complexity: string, hasCode: boolean, hasMath: boolean, hasData: boolean): string[] {
    const suggestions = []

    if (hasCode) {
      suggestions.push('Run Code', 'Explain Code', 'Optimize Code', 'Add Tests')
    }
    if (hasMath) {
      suggestions.push('Visualize Math', 'Solve Equation', 'Plot Function')
    }
    if (hasData) {
      suggestions.push('Visualize Data', 'Export Data', 'Analyze Trends')
    }
    if (complexity === 'complex') {
      suggestions.push('Summarize', 'Break Down', 'Create Outline')
    }

    return suggestions
  }

  function detectLanguage(codeBlock?: string): string | undefined {
    if (!codeBlock) return undefined
    
    const languageMap: Record<string, string[]> = {
      javascript: ['const', 'let', 'var', 'function', '=>', 'import', 'export'],
      typescript: ['interface', 'type', ': string', ': number', 'as ', 'extends'],
      python: ['def ', 'import ', 'from ', 'class ', '__init__', 'self.'],
      java: ['public class', 'private ', 'public static void main'],
      cpp: ['#include', 'std::', 'int main()', 'cout', 'cin'],
      rust: ['fn ', 'let mut', 'match ', '&str', 'Vec<'],
      erlang: ['-module(', 'fun(', 'receive', 'spawn', '->'],
      html: ['<html', '<div', '<span', 'class=', 'id='],
      css: ['{', '}', ':', ';', 'color:', 'background:'],
      json: ['{"', '"}', '":', ',"', '[{']
    }

    for (const [lang, keywords] of Object.entries(languageMap)) {
      if (keywords.some(keyword => codeBlock.includes(keyword))) {
        return lang
      }
    }

    return 'text'
  }

  // Advanced content parsing with multiple formats
  function parseAdvancedContent(text: string) {
    const elements = []
    let currentIndex = 0

    // Parse code blocks with syntax highlighting
    const codeBlockRegex = /```(\w+)?\n?([\s\S]*?)```/g
    let match

    while ((match = codeBlockRegex.exec(text)) !== null) {
      // Add text before code block
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        elements.push(...parseInlineElements(beforeText, elements.length))
      }

      const language = match[1] || 'text'
      const code = match[2].trim()

      elements.push(
        <CodeBlockRenderer
          key={elements.length}
          language={language}
          code={code}
          messageId={messageId}
          onInteraction={(action, data) => handleInteraction(action, data)}
        />
      )

      currentIndex = match.index + match[0].length
    }

    // Parse LaTeX math blocks
    const mathBlockRegex = /\$\$([\s\S]*?)\$\$/g
    let mathMatch
    while ((mathMatch = mathBlockRegex.exec(text)) !== null) {
      if (mathMatch.index > currentIndex) {
        const beforeText = text.slice(currentIndex, mathMatch.index)
        elements.push(...parseInlineElements(beforeText, elements.length))
      }

      elements.push(
        <MathRenderer
          key={elements.length}
          expression={mathMatch[1]}
          type="block"
          messageId={messageId}
        />
      )

      currentIndex = mathMatch.index + mathMatch[0].length
    }

    // Add remaining text
    if (currentIndex < text.length) {
      const remainingText = text.slice(currentIndex)
      elements.push(...parseInlineElements(remainingText, elements.length))
    }

    return elements.length > 0 ? elements : [text]
  }

  function parseInlineElements(text: string, startKey: number) {
    const parts: (JSX.Element | string)[] = []
    let currentIndex = 0

    // Parse inline code
    const inlineCodeRegex = /`([^`]+)`/g
    let match

    while ((match = inlineCodeRegex.exec(text)) !== null) {
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        parts.push(...parseInlineMath(beforeText, startKey + parts.length))
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
      parts.push(...parseInlineMath(remainingText, startKey + parts.length))
    }

    return parts.length > 0 ? parts : [text]
  }

  function parseInlineMath(text: string, startKey: number) {
    const parts: (JSX.Element | string)[] = []
    let currentIndex = 0

    const inlineMathRegex = /\$([^$]+)\$/g
    let match

    while ((match = inlineMathRegex.exec(text)) !== null) {
      if (match.index > currentIndex) {
        const beforeText = text.slice(currentIndex, match.index)
        if (beforeText.trim()) {
          parts.push(
            <span key={startKey + parts.length}>{beforeText}</span>
          )
        }
      }

      parts.push(
        <MathRenderer
          key={startKey + parts.length}
          expression={match[1]}
          type="inline"
          messageId={messageId}
        />
      )

      currentIndex = match.index + match[0].length
    }

    if (currentIndex < text.length) {
      const remainingText = text.slice(currentIndex)
      if (remainingText.trim()) {
        parts.push(
          <span key={startKey + parts.length}>{remainingText}</span>
        )
      }
    }

    return parts.length > 0 ? parts : [text]
  }

  function handleInteraction(action: string, data: any) {
    console.log('Interaction:', action, data)
    // Handle interactive elements
  }

  return (
    <div 
      ref={containerRef}
      className={cn(
        "relative transition-all duration-300",
        analysis?.complexity === 'complex' && "border-l-4 border-blue-500 pl-4"
      )}
    >
      {/* Content Analysis Badge */}
      {analysis && (
        <div className="flex flex-wrap gap-1 mb-2">
          <span className={cn(
            "text-xs px-2 py-1 rounded-full",
            analysis.type === 'code' && "bg-green-100 text-green-800",
            analysis.type === 'math' && "bg-blue-100 text-blue-800",
            analysis.type === 'data' && "bg-purple-100 text-purple-800",
            analysis.type === 'mixed' && "bg-orange-100 text-orange-800"
          )}>
            {analysis.type} • {analysis.complexity}
          </span>
          {analysis.language && (
            <span className="text-xs px-2 py-1 rounded-full bg-gray-100 text-gray-800">
              {analysis.language}
            </span>
          )}
        </div>
      )}

      {/* Main Content */}
      <div className="space-y-2">
        {parsedContent}
      </div>

      {/* Interactive Suggestions */}
      {analysis?.suggestedActions && analysis.suggestedActions.length > 0 && (
        <div className="mt-3 flex flex-wrap gap-1">
          {analysis.suggestedActions.slice(0, 3).map((action, index) => (
            <button
              key={index}
              className="text-xs px-2 py-1 bg-blue-50 hover:bg-blue-100 text-blue-700 rounded-md transition-colors"
              onClick={() => handleInteraction('suggestion', action)}
            >
              {action}
            </button>
          ))}
        </div>
      )}
    </div>
  )
}

// Code Block Renderer with Syntax Highlighting
function CodeBlockRenderer({ 
  language, 
  code, 
  messageId, 
  onInteraction 
}: { 
  language: string
  code: string
  messageId: string
  onInteraction: (action: string, data: any) => void
}) {
  const [copied, setCopied] = useState(false)
  const [expanded, setExpanded] = useState(false)

  const copyToClipboard = async () => {
    await navigator.clipboard.writeText(code)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const shouldTruncate = code.split('\n').length > 10

  return (
    <div className="relative group">
      <div className="flex items-center justify-between bg-gray-800 text-white px-3 py-1 text-xs rounded-t-md">
        <span className="font-mono">{language}</span>
        <div className="flex gap-2">
          <button
            onClick={copyToClipboard}
            className="hover:bg-gray-700 p-1 rounded"
            title="Copy code"
          >
            {copied ? '✓' : '📋'}
          </button>
          <button
            onClick={() => onInteraction('run', { language, code })}
            className="hover:bg-gray-700 p-1 rounded"
            title="Run code"
          >
            ▶️
          </button>
        </div>
      </div>
      
      <pre className={cn(
        "bg-gray-900 text-gray-100 p-4 rounded-b-md overflow-x-auto text-sm font-mono",
        shouldTruncate && !expanded && "max-h-40 overflow-hidden"
      )}>
        <code className={`language-${language}`}>
          {code}
        </code>
      </pre>

      {shouldTruncate && (
        <button
          onClick={() => setExpanded(!expanded)}
          className="absolute bottom-2 right-2 bg-gray-700 text-white text-xs px-2 py-1 rounded"
        >
          {expanded ? 'Collapse' : 'Expand'}
        </button>
      )}
    </div>
  )
}

// Math Renderer with LaTeX support
function MathRenderer({ 
  expression, 
  type, 
  messageId 
}: { 
  expression: string
  type: 'inline' | 'block'
  messageId: string
}) {
  const [rendered, setRendered] = useState<string>('')

  useEffect(() => {
    // Simple LaTeX-like rendering (would use KaTeX/MathJax in production)
    const simpleRender = expression
      .replace(/\\frac\{([^}]+)\}\{([^}]+)\}/g, '($1)/($2)')
      .replace(/\\sqrt\{([^}]+)\}/g, '√($1)')
      .replace(/\\sum/g, '∑')
      .replace(/\\int/g, '∫')
      .replace(/\\pi/g, 'π')
      .replace(/\\alpha/g, 'α')
      .replace(/\\beta/g, 'β')
      .replace(/\\gamma/g, 'γ')
      .replace(/\\theta/g, 'θ')
      .replace(/\\lambda/g, 'λ')
      .replace(/\\mu/g, 'μ')
      .replace(/\\sigma/g, 'σ')

    setRendered(simpleRender)
  }, [expression])

  if (type === 'inline') {
    return (
      <span className="font-mono bg-blue-50 px-1 py-0.5 rounded text-blue-800 border border-blue-200">
        {rendered}
      </span>
    )
  }

  return (
    <div className="bg-blue-50 border border-blue-200 rounded-md p-4 my-2 text-center">
      <div className="font-mono text-lg text-blue-900">
        {rendered}
      </div>
      <div className="text-xs text-blue-600 mt-2">
        LaTeX: {expression}
      </div>
    </div>
  )
}