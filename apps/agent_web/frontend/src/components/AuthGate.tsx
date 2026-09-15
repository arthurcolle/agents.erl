import { FormEvent, useState } from 'react'
import { establishApiSession, SessionResult } from '@/lib/apiAuth'

interface AuthGateProps {
  initialResult: SessionResult
  onAuthenticated: () => void
}

export default function AuthGate({ initialResult, onAuthenticated }: AuthGateProps) {
  const [token, setToken] = useState('')
  const [result, setResult] = useState(initialResult)
  const [submitting, setSubmitting] = useState(false)

  const submit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    if (!token.trim() || submitting) return

    setSubmitting(true)
    const nextResult = await establishApiSession(token.trim())
    setToken('')
    setResult(nextResult)
    setSubmitting(false)
    if (nextResult.authenticated) onAuthenticated()
  }

  const message = result.error === 'unavailable'
    ? 'The server is missing AGENTS_WEB_API_TOKEN.'
    : result.error === 'network'
      ? 'The API is unreachable. Check the service and try again.'
      : result.error === 'unauthorized'
        ? 'That token was rejected.'
        : 'Authenticate to open the control plane.'

  return (
    <main className="min-h-screen bg-background text-foreground flex items-center justify-center p-6">
      <section className="w-full max-w-md rounded-lg border bg-card p-6 shadow-lg">
        <h1 className="text-xl font-semibold">Agents control plane</h1>
        <p className="mt-2 text-sm text-muted-foreground">{message}</p>
        <form onSubmit={submit} className="mt-6 space-y-4">
          <label htmlFor="api-token" className="block text-sm font-medium">
            API token
          </label>
          <input
            id="api-token"
            type="password"
            autoComplete="current-password"
            value={token}
            onChange={(event) => setToken(event.target.value)}
            placeholder="Paste AGENTS_WEB_API_TOKEN"
            className="w-full rounded-md border bg-background px-3 py-2 text-sm outline-none focus:ring-2 focus:ring-ring"
            disabled={submitting || result.error === 'unavailable'}
          />
          <button
            type="submit"
            disabled={submitting || !token.trim() || result.error === 'unavailable'}
            className="w-full rounded-md bg-primary px-3 py-2 text-sm font-medium text-primary-foreground disabled:cursor-not-allowed disabled:opacity-50"
          >
            {submitting ? 'Authenticating…' : 'Continue'}
          </button>
        </form>
        <p className="mt-4 text-xs text-muted-foreground">
          The token is exchanged for an HttpOnly same-origin session and is not stored in browser storage.
        </p>
      </section>
    </main>
  )
}
