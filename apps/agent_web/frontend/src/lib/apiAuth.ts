export interface SessionResult {
  authenticated: boolean
  status: number
  error?: 'unauthorized' | 'unavailable' | 'network'
}

const SESSION_ENDPOINT = '/api/system/session'

function tokenFromFragment(): string | undefined {
  if (!window.location.hash.startsWith('#')) return undefined

  const params = new URLSearchParams(window.location.hash.slice(1))
  const token = params.get('token') || params.get('api_token')
  return token && token.length > 0 ? token : undefined
}

function clearTokenFragment(): void {
  if (!window.location.hash) return
  window.history.replaceState(
    null,
    document.title,
    `${window.location.pathname}${window.location.search}`,
  )
}

function resultFromStatus(status: number): SessionResult {
  if (status === 401) return { authenticated: false, status, error: 'unauthorized' }
  if (status === 503) return { authenticated: false, status, error: 'unavailable' }
  return { authenticated: false, status, error: 'network' }
}

export async function establishApiSession(token?: string): Promise<SessionResult> {
  const headers: Record<string, string> = { Accept: 'application/json' }
  if (token) headers.Authorization = `Bearer ${token}`

  try {
    const response = await fetch(SESSION_ENDPOINT, {
      method: 'POST',
      headers,
      credentials: 'same-origin',
      cache: 'no-store',
    })

    return response.ok
      ? { authenticated: true, status: response.status }
      : resultFromStatus(response.status)
  } catch {
    return { authenticated: false, status: 0, error: 'network' }
  }
}

export async function bootstrapApiSession(): Promise<SessionResult> {
  const token = tokenFromFragment()
  // Remove the bearer from the address bar before making the request.  URL
  // fragments are not sent to the server, but can still leak through screen
  // sharing, history, or copied links.
  if (token) clearTokenFragment()
  return establishApiSession(token)
}

export async function revokeApiSession(): Promise<void> {
  try {
    await fetch(SESSION_ENDPOINT, {
      method: 'DELETE',
      credentials: 'same-origin',
      cache: 'no-store',
    })
  } catch {
    // Logout is best-effort; the browser will discard the session on expiry.
  }
}
