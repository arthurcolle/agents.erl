import { useState, useEffect, useRef } from 'react'
import { Card, CardContent } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { MessageSquare, Bug, Star, Lightbulb, X } from 'lucide-react'

interface ContextMenuProps {
  x: number
  y: number
  target: string
  onClose: () => void
  onFeedback: (type: string, message: string, target: string) => void
}

export function ContextMenu({ x, y, target, onClose, onFeedback }: ContextMenuProps) {
  const [showFeedbackForm, setShowFeedbackForm] = useState(false)
  const [feedbackType, setFeedbackType] = useState('')
  const [feedbackMessage, setFeedbackMessage] = useState('')
  const menuRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        onClose()
      }
    }

    const handleEscape = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        onClose()
      }
    }

    document.addEventListener('mousedown', handleClickOutside)
    document.addEventListener('keydown', handleEscape)

    return () => {
      document.removeEventListener('mousedown', handleClickOutside)
      document.removeEventListener('keydown', handleEscape)
    }
  }, [onClose])

  const feedbackTypes = [
    { id: 'bug', label: 'Report Bug', icon: Bug, color: 'text-red-500' },
    { id: 'improvement', label: 'Suggest Improvement', icon: Lightbulb, color: 'text-yellow-500' },
    { id: 'feature', label: 'Request Feature', icon: Star, color: 'text-blue-500' },
    { id: 'general', label: 'General Feedback', icon: MessageSquare, color: 'text-gray-500' }
  ]

  const handleSubmitFeedback = () => {
    if (feedbackType && feedbackMessage.trim()) {
      onFeedback(feedbackType, feedbackMessage, target)
      onClose()
    }
  }

  // Adjust position if menu would go off screen
  const adjustedX = Math.min(x, window.innerWidth - 300)
  const adjustedY = Math.min(y, window.innerHeight - (showFeedbackForm ? 400 : 200))

  return (
    <div 
      ref={menuRef}
      className="fixed z-50 bg-white dark:bg-gray-800 shadow-lg border border-gray-200 dark:border-gray-700 rounded-lg min-w-64"
      style={{ left: adjustedX, top: adjustedY }}
    >
      {!showFeedbackForm ? (
        <div className="p-2">
          <div className="px-3 py-2 text-xs font-medium text-gray-500 dark:text-gray-400 border-b border-gray-100 dark:border-gray-700">
            Feedback for: {target}
          </div>
          
          <div className="space-y-1 mt-2">
            {feedbackTypes.map(({ id, label, icon: Icon, color }) => (
              <Button
                key={id}
                variant="ghost"
                size="sm"
                className="w-full justify-start gap-2 h-8"
                onClick={() => {
                  setFeedbackType(id)
                  setShowFeedbackForm(true)
                }}
              >
                <Icon className={`h-4 w-4 ${color}`} />
                {label}
              </Button>
            ))}
          </div>
        </div>
      ) : (
        <div className="p-4 w-80">
          <div className="flex items-center justify-between mb-3">
            <h3 className="font-medium text-sm">
              {feedbackTypes.find(t => t.id === feedbackType)?.label}
            </h3>
            <Button
              variant="ghost"
              size="icon"
              className="h-6 w-6"
              onClick={onClose}
            >
              <X className="h-4 w-4" />
            </Button>
          </div>
          
          <div className="text-xs text-gray-500 dark:text-gray-400 mb-3">
            Target: {target}
          </div>
          
          <textarea
            value={feedbackMessage}
            onChange={(e) => setFeedbackMessage(e.target.value)}
            placeholder="Describe your feedback..."
            className="w-full p-2 border border-gray-200 dark:border-gray-600 rounded text-sm resize-none"
            rows={4}
          />
          
          <div className="flex gap-2 mt-3">
            <Button
              size="sm"
              onClick={handleSubmitFeedback}
              disabled={!feedbackMessage.trim()}
              className="flex-1"
            >
              Submit
            </Button>
            <Button
              size="sm"
              variant="outline"
              onClick={() => setShowFeedbackForm(false)}
            >
              Back
            </Button>
          </div>
        </div>
      )}
    </div>
  )
}