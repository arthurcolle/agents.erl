import React, { useState, useEffect } from 'react';
import { Button } from './ui/button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from './ui/card';
import { Badge } from './ui/badge';
import { AlertCircle, CheckCircle, ExternalLink, RefreshCw, X } from 'lucide-react';
import { Alert, AlertDescription } from './ui/alert';

interface MCPOAuthManagerProps {
  serverId: string;
  serverName: string;
  authType: string;
  onAuthSuccess?: (serverId: string) => void;
  onAuthError?: (serverId: string, error: string) => void;
  onAuthChange?: (provider: any, authenticated: any) => void;
}

interface OAuthStatus {
  oauth_required: boolean;
  authenticated: boolean;
  server_id: string;
  expires_at?: string;
}

interface OAuthData {
  auth_url: string;
  popup_required: boolean;
  redirect_uri: string;
}

export const MCPOAuthManager: React.FC<MCPOAuthManagerProps> = ({
  serverId,
  serverName,
  authType,
  onAuthSuccess,
  onAuthError
}) => {
  const [oauthStatus, setOAuthStatus] = useState<OAuthStatus | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [authInProgress, setAuthInProgress] = useState(false);

  // Check OAuth status on component mount
  useEffect(() => {
    if (authType === 'oauth2') {
      checkOAuthStatus();
    }
  }, [serverId, authType]);

  // Listen for OAuth completion via WebSocket
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      try {
        const data = JSON.parse(event.data);
        
        if (data.type === 'mcp_oauth_success' && data.server_id === serverId) {
          setAuthInProgress(false);
          setError(null);
          checkOAuthStatus(); // Refresh status
          onAuthSuccess?.(serverId);
        } else if (data.type === 'mcp_oauth_error' && data.server_id === serverId) {
          setAuthInProgress(false);
          setError(data.error);
          onAuthError?.(serverId, data.error);
        }
      } catch (e) {
        // Ignore non-JSON messages
      }
    };

    // Connect to WebSocket for real-time updates
    const ws = new WebSocket(`ws://localhost:8080/ws`);
    ws.addEventListener('message', handleMessage);

    return () => {
      ws.removeEventListener('message', handleMessage);
      ws.close();
    };
  }, [serverId, onAuthSuccess, onAuthError]);

  // Listen for popup completion
  useEffect(() => {
    const handlePopupMessage = (event: MessageEvent) => {
      if (event.data.type === 'oauth_success') {
        setAuthInProgress(false);
        checkOAuthStatus();
      } else if (event.data.type === 'oauth_error') {
        setAuthInProgress(false);
        setError(event.data.error);
      }
    };

    window.addEventListener('message', handlePopupMessage);
    return () => window.removeEventListener('message', handlePopupMessage);
  }, []);

  const checkOAuthStatus = async () => {
    try {
      setLoading(true);
      const response = await fetch(`/api/mcp/oauth/status/${serverId}`);
      const data = await response.json();
      setOAuthStatus(data);
    } catch (err) {
      setError('Failed to check OAuth status');
    } finally {
      setLoading(false);
    }
  };

  const startOAuthFlow = async () => {
    try {
      setLoading(true);
      setError(null);
      setAuthInProgress(true);

      const response = await fetch(`/api/mcp/oauth/start/${serverId}`);
      const data = await response.json();

      if (!data.success) {
        throw new Error(data.error);
      }

      const oauthData: OAuthData = data;
      
      // Open popup for OAuth flow
      const popup = window.open(
        oauthData.auth_url,
        'oauth_popup',
        'width=600,height=700,scrollbars=yes,resizable=yes'
      );

      if (!popup) {
        throw new Error('Failed to open OAuth popup. Please allow popups for this site.');
      }

      // Monitor popup closure
      const checkClosed = setInterval(() => {
        if (popup.closed) {
          clearInterval(checkClosed);
          if (authInProgress) {
            setAuthInProgress(false);
            setError('OAuth flow was cancelled');
          }
        }
      }, 1000);

    } catch (err) {
      setAuthInProgress(false);
      setError(err instanceof Error ? err.message : 'OAuth flow failed');
    } finally {
      setLoading(false);
    }
  };

  const refreshToken = async () => {
    try {
      setLoading(true);
      setError(null);

      const response = await fetch(`/api/mcp/oauth/refresh/${serverId}`, {
        method: 'POST'
      });
      const data = await response.json();

      if (!data.success) {
        throw new Error(data.error);
      }

      checkOAuthStatus(); // Refresh status
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to refresh token');
    } finally {
      setLoading(false);
    }
  };

  const revokeToken = async () => {
    try {
      setLoading(true);
      setError(null);

      const response = await fetch(`/api/mcp/oauth/revoke/${serverId}`, {
        method: 'DELETE'
      });
      const data = await response.json();

      if (!data.success) {
        throw new Error(data.error);
      }

      checkOAuthStatus(); // Refresh status
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to revoke token');
    } finally {
      setLoading(false);
    }
  };

  if (authType !== 'oauth2') {
    return null; // Only show for OAuth2 servers
  }

  return (
    <Card className="w-full">
      <CardHeader className="pb-3">
        <CardTitle className="flex items-center gap-2 text-sm">
          <ExternalLink className="h-4 w-4" />
          OAuth Authentication
        </CardTitle>
        <CardDescription className="text-xs">
          {serverName} requires OAuth2 authentication
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-3">
        {error && (
          <Alert variant="destructive" className="py-2">
            <AlertCircle className="h-4 w-4" />
            <AlertDescription className="text-xs">{error}</AlertDescription>
          </Alert>
        )}

        {oauthStatus && (
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2">
              {oauthStatus.authenticated ? (
                <>
                  <CheckCircle className="h-4 w-4 text-green-500" />
                  <Badge variant="secondary" className="text-xs">Authenticated</Badge>
                </>
              ) : (
                <>
                  <AlertCircle className="h-4 w-4 text-yellow-500" />
                  <Badge variant="outline" className="text-xs">Not Authenticated</Badge>
                </>
              )}
            </div>
            
            {oauthStatus.expires_at && (
              <span className="text-xs text-muted-foreground">
                Expires: {new Date(oauthStatus.expires_at).toLocaleDateString()}
              </span>
            )}
          </div>
        )}

        <div className="flex gap-2">
          {!oauthStatus?.authenticated ? (
            <Button
              onClick={startOAuthFlow}
              disabled={loading || authInProgress}
              size="sm"
              className="flex-1"
            >
              {authInProgress ? (
                <>
                  <RefreshCw className="h-3 w-3 mr-1 animate-spin" />
                  Authenticating...
                </>
              ) : (
                <>
                  <ExternalLink className="h-3 w-3 mr-1" />
                  Authenticate
                </>
              )}
            </Button>
          ) : (
            <>
              <Button
                onClick={refreshToken}
                disabled={loading}
                size="sm"
                variant="outline"
                className="flex-1"
              >
                <RefreshCw className="h-3 w-3 mr-1" />
                Refresh
              </Button>
              <Button
                onClick={revokeToken}
                disabled={loading}
                size="sm"
                variant="destructive"
                className="flex-1"
              >
                <X className="h-3 w-3 mr-1" />
                Revoke
              </Button>
            </>
          )}
        </div>

        {authInProgress && (
          <div className="text-xs text-muted-foreground bg-muted p-2 rounded">
            OAuth popup opened. Complete authentication in the popup window.
          </div>
        )}
      </CardContent>
    </Card>
  );
};