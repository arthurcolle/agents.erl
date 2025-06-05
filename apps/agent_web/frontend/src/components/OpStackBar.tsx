import { useState } from 'react'
import { Button } from '@/components/ui/button'
import { 
  Settings, 
  Search, 
  Terminal, 
  RefreshCw,
  Zap,
  Eye,
  Database,
  Network
} from 'lucide-react'
import { cn } from '@/lib/utils'
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip'

interface OpStackBarProps {
  onOpenCommandPalette: () => void
  onOpenSettings: () => void
  onOpenTerminal: () => void
  onRefreshSystem: () => void
  className?: string
}

export function OpStackBar({
  onOpenCommandPalette,
  onOpenSettings,
  onOpenTerminal,
  onRefreshSystem,
  className
}: OpStackBarProps) {
  const [isHovered, setIsHovered] = useState(false)

  const operations = [
    {
      id: 'command-palette',
      icon: Search,
      label: 'Command Palette',
      shortcut: '⌘K',
      action: onOpenCommandPalette,
      variant: 'default' as const
    },
    {
      id: 'settings',
      icon: Settings,
      label: 'Settings',
      action: onOpenSettings,
      variant: 'ghost' as const
    },
    {
      id: 'terminal',
      icon: Terminal,
      label: 'Terminal',
      action: onOpenTerminal,
      variant: 'ghost' as const
    },
    {
      id: 'refresh',
      icon: RefreshCw,
      label: 'Refresh System',
      action: onRefreshSystem,
      variant: 'ghost' as const
    },
    {
      id: 'monitor',
      icon: Eye,
      label: 'Monitor',
      action: () => {},
      variant: 'ghost' as const
    },
    {
      id: 'network',
      icon: Network,
      label: 'Network Status',
      action: () => {},
      variant: 'ghost' as const
    }
  ]

  return (
    <TooltipProvider>
      <div 
        className={cn(
          "fixed left-2 top-1/2 -translate-y-1/2 z-50",
          "flex flex-col gap-1 p-1 rounded-lg",
          "bg-background/95 backdrop-blur-sm border",
          "transition-all duration-200",
          isHovered ? "shadow-lg" : "shadow-sm",
          className
        )}
        onMouseEnter={() => setIsHovered(true)}
        onMouseLeave={() => setIsHovered(false)}
      >
        {operations.map((op) => (
          <Tooltip key={op.id}>
            <TooltipTrigger asChild>
              <Button
                variant={op.variant}
                size="icon"
                className={cn(
                  "h-8 w-8 transition-all",
                  isHovered ? "scale-105" : "scale-100"
                )}
                onClick={op.action}
              >
                <op.icon className="h-4 w-4" />
              </Button>
            </TooltipTrigger>
            <TooltipContent side="right" className="flex items-center gap-2">
              <span>{op.label}</span>
              {op.shortcut && (
                <kbd className="px-1 py-0.5 text-xs bg-muted rounded">
                  {op.shortcut}
                </kbd>
              )}
            </TooltipContent>
          </Tooltip>
        ))}

        {/* Quick Actions Separator */}
        <div className="h-px bg-border my-1" />
        
        {/* Quick Status Indicator */}
        <div className="flex flex-col items-center gap-1 p-1">
          <div className="h-2 w-2 rounded-full bg-green-500 animate-pulse" 
               title="System Online" />
          <span className="text-xs text-muted-foreground font-mono">
            99%
          </span>
        </div>
      </div>
    </TooltipProvider>
  )
}