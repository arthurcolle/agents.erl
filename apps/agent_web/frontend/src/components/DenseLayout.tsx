import { useState, useRef, useEffect } from 'react'
import { Panel, PanelGroup, PanelResizeHandle } from 'react-resizable-panels'
import { cn } from '@/lib/utils'
import { ChevronLeft, ChevronRight } from 'lucide-react'
import { Button } from '@/components/ui/button'

interface DenseLayoutProps {
  leftDock: React.ReactNode
  coreWorkspace: React.ReactNode
  rightDrawer?: React.ReactNode
  className?: string
}

export function DenseLayout({ leftDock, coreWorkspace, rightDrawer, className }: DenseLayoutProps) {
  const [isLeftCollapsed, setIsLeftCollapsed] = useState(false)
  const [isRightCollapsed, setIsRightCollapsed] = useState(true)
  
  return (
    <div className={cn("h-full w-full flex", className)}>
      <PanelGroup direction="horizontal" className="h-full">
        {/* Left Dock */}
        <Panel 
          defaultSize={20}
          minSize={isLeftCollapsed ? 3 : 15}
          maxSize={isLeftCollapsed ? 3 : 25}
          collapsible
          onCollapse={() => setIsLeftCollapsed(true)}
          onExpand={() => setIsLeftCollapsed(false)}
          className="relative"
        >
          <div className={cn(
            "h-full border-r bg-card transition-all duration-200",
            isLeftCollapsed && "overflow-hidden"
          )}>
            {isLeftCollapsed ? (
              <div className="flex flex-col items-center p-1 space-y-2">
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-6 w-6"
                  onClick={() => setIsLeftCollapsed(false)}
                >
                  <ChevronRight className="h-3 w-3" />
                </Button>
              </div>
            ) : (
              leftDock
            )}
          </div>
        </Panel>

        <PanelResizeHandle className="w-1 bg-border hover:bg-accent transition-colors" />

        {/* Core Workspace */}
        <Panel defaultSize={rightDrawer && !isRightCollapsed ? 60 : 80}>
          <div className="h-full">
            {coreWorkspace}
          </div>
        </Panel>

        {/* Right Drawer */}
        {rightDrawer && (
          <>
            <PanelResizeHandle className="w-1 bg-border hover:bg-accent transition-colors" />
            <Panel 
              defaultSize={20}
              minSize={isRightCollapsed ? 0 : 15}
              maxSize={isRightCollapsed ? 0 : 30}
              collapsible
              onCollapse={() => setIsRightCollapsed(true)}
              onExpand={() => setIsRightCollapsed(false)}
            >
              {!isRightCollapsed && (
                <div className="h-full border-l bg-card">
                  <div className="flex items-center justify-between p-2 border-b">
                    <span className="text-sm font-medium">Inspector</span>
                    <Button
                      variant="ghost"
                      size="icon"
                      className="h-6 w-6"
                      onClick={() => setIsRightCollapsed(true)}
                    >
                      <ChevronRight className="h-3 w-3" />
                    </Button>
                  </div>
                  <div className="p-2">
                    {rightDrawer}
                  </div>
                </div>
              )}
            </Panel>
          </>
        )}
      </PanelGroup>

      {/* Right drawer toggle when collapsed */}
      {rightDrawer && isRightCollapsed && (
        <div className="absolute right-0 top-1/2 -translate-y-1/2 z-10">
          <Button
            variant="ghost"
            size="icon"
            className="h-8 w-4 rounded-l-md rounded-r-none border-l border-y bg-card"
            onClick={() => setIsRightCollapsed(false)}
          >
            <ChevronLeft className="h-3 w-3" />
          </Button>
        </div>
      )}
    </div>
  )
}