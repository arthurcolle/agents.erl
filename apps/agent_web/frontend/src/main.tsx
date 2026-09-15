import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App.tsx'
import './index.css'
import AuthGate from './components/AuthGate'
import { bootstrapApiSession, SessionResult } from './lib/apiAuth'

function LoadingScreen() {
  return (
    <main className="min-h-screen bg-background text-foreground flex items-center justify-center">
      <p className="text-sm text-muted-foreground">Connecting to the control plane…</p>
    </main>
  )
}

function Root() {
  const [result, setResult] = React.useState<SessionResult | null>(null)

  React.useEffect(() => {
    let mounted = true
    bootstrapApiSession().then((nextResult) => {
      if (mounted) setResult(nextResult)
    })
    return () => {
      mounted = false
    }
  }, [])

  if (!result) return <LoadingScreen />
  if (!result.authenticated) {
    return <AuthGate initialResult={result} onAuthenticated={() => setResult({ authenticated: true, status: 204 })} />
  }
  return <App />
}

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <Root />
  </React.StrictMode>,
)
