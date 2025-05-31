import React, { useState, useCallback, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from './ui/card';
import { Button } from './ui/button';
import { Badge } from './ui/badge';
import { CheckCircle, XCircle, ExternalLink, AlertCircle } from 'lucide-react';

interface OAuthProvider {
  id: string;
  name: string;
  category: string;
  description: string;
  requiresAuth: boolean;
  authType: 'oauth2' | 'api_key' | 'open';
  scopes?: string[];
  official?: boolean;
}

interface OAuthStatus {
  authenticated: boolean;
  provider: string;
  user_id?: string;
  error?: string;
}

interface OAuthManagerProps {
  onAuthChange?: (provider: string, authenticated: boolean) => void;
}

const OAUTH_PROVIDERS: OAuthProvider[] = [
  {
    id: 'github',
    name: 'GitHub',
    category: 'Software Development',
    description: 'Repository management, issue tracking, and code collaboration',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['repo', 'user:email', 'read:org'],
    official: true
  },
  {
    id: 'slack',
    name: 'Slack',
    category: 'Communication',
    description: 'Team communication and workspace management',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['channels:read', 'chat:write', 'users:read', 'files:read'],
    official: true
  },
  {
    id: 'linear',
    name: 'Linear',
    category: 'Project Management',
    description: 'Issue tracking and project management for software teams',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['read', 'write'],
    official: true
  },
  {
    id: 'asana',
    name: 'Asana',
    category: 'Project Management',
    description: 'Interact with your Asana workspace through AI tools',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['default'],
    official: true
  },
  {
    id: 'intercom',
    name: 'Intercom',
    category: 'Customer Support',
    description: 'Access real-time customer conversations and tickets',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['read_conversations', 'write_conversations', 'read_users'],
    official: true
  },
  {
    id: 'zapier',
    name: 'Zapier',
    category: 'Automation',
    description: 'Connect to nearly 8,000 apps through automation platform',
    requiresAuth: true,
    authType: 'oauth2',
    scopes: ['read', 'write'],
    official: true
  }
];

export const OAuthManager: React.FC<OAuthManagerProps> = ({ onAuthChange }) => {
  const [authStatuses, setAuthStatuses] = useState<Map<string, OAuthStatus>>(new Map());
  const [loading, setLoading] = useState<Set<string>>(new Set());
  const [error, setError] = useState<string | null>(null);

  // Check authentication status for all providers on mount
  useEffect(() => {
    checkAllAuthStatuses();
    setupMessageListener();
  }, []);

  const checkAllAuthStatuses = async () => {
    for (const provider of OAUTH_PROVIDERS) {
      if (provider.requiresAuth) {
        await checkAuthStatus(provider.id);
      }
    }
  };

  const checkAuthStatus = async (providerId: string) => {
    try {
      const response = await fetch(`/api/oauth/status/${providerId}`, {
        credentials: 'include',
        headers: {
          'X-User-ID': getUserId()
        }
      });
      
      if (response.ok) {
        const status = await response.json();
        setAuthStatuses(prev => new Map(prev.set(providerId, status)));
        onAuthChange?.(providerId, status.authenticated);
      }
    } catch (err) {
      console.error(`Failed to check auth status for ${providerId}:`, err);
    }
  };

  const setupMessageListener = () => {
    const handleMessage = (event: MessageEvent) => {
      if (event.origin !== window.location.origin) {
        return; // Security: only accept messages from same origin
      }

      const { type, provider, data, error } = event.data;
      
      if (type === 'oauth_success') {
        console.log(`OAuth success for ${provider}:`, data);
        setAuthStatuses(prev => new Map(prev.set(provider, {
          authenticated: true,
          provider,
          user_id: data.user_id
        })));
        setLoading(prev => {
          const newSet = new Set(prev);
          newSet.delete(provider);
          return newSet;
        });
        onAuthChange?.(provider, true);
        setError(null);
      } else if (type === 'oauth_error') {
        console.error(`OAuth error for ${provider}:`, error);
        setAuthStatuses(prev => new Map(prev.set(provider, {
          authenticated: false,
          provider,
          error
        })));
        setLoading(prev => {
          const newSet = new Set(prev);
          newSet.delete(provider);
          return newSet;
        });
        onAuthChange?.(provider, false);
        setError(`Authentication failed for ${provider}: ${error}`);
      }
    };

    window.addEventListener('message', handleMessage);
    return () => window.removeEventListener('message', handleMessage);
  };

  const initiateOAuth = async (providerId: string) => {
    setLoading(prev => new Set(prev.add(providerId)));
    setError(null);

    try {
      const response = await fetch(`/api/oauth/authorize/${providerId}`, {
        credentials: 'include',
        headers: {
          'X-User-ID': getUserId()
        }
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      
      if (data.auth_url) {
        // Open popup window for OAuth flow
        const popup = window.open(
          data.auth_url,
          `oauth_${providerId}`,
          data.popup_features || 'width=600,height=700,scrollbars=yes,resizable=yes'
        );

        if (!popup) {
          throw new Error('Popup blocked. Please allow popups for OAuth authentication.');
        }

        // Monitor popup closure
        const checkClosed = setInterval(() => {
          if (popup.closed) {
            clearInterval(checkClosed);
            setLoading(prev => {
              const newSet = new Set(prev);
              newSet.delete(providerId);
              return newSet;
            });
          }
        }, 1000);

        // Set timeout for popup
        setTimeout(() => {
          if (!popup.closed) {
            popup.close();
            clearInterval(checkClosed);
            setLoading(prev => {
              const newSet = new Set(prev);
              newSet.delete(providerId);
              return newSet;
            });
            setError('Authentication timed out. Please try again.');
          }
        }, 5 * 60 * 1000); // 5 minute timeout
      } else {
        throw new Error('No authentication URL received');
      }
    } catch (err) {
      console.error(`OAuth initiation failed for ${providerId}:`, err);
      setError(err instanceof Error ? err.message : 'Authentication failed');
      setLoading(prev => {
        const newSet = new Set(prev);
        newSet.delete(providerId);
        return newSet;
      });
    }
  };

  const revokeAuth = async (providerId: string) => {
    try {
      const response = await fetch(`/api/oauth/revoke/${providerId}`, {
        method: 'POST',
        credentials: 'include',
        headers: {
          'X-User-ID': getUserId()
        }
      });

      if (response.ok) {
        setAuthStatuses(prev => new Map(prev.set(providerId, {
          authenticated: false,
          provider: providerId
        })));
        onAuthChange?.(providerId, false);
      } else {
        throw new Error('Failed to revoke authentication');
      }
    } catch (err) {
      console.error(`Failed to revoke auth for ${providerId}:`, err);
      setError(err instanceof Error ? err.message : 'Failed to revoke authentication');
    }
  };

  const exportClaudeConfig = async () => {
    try {
      const response = await fetch('/api/claude/export', {
        method: 'POST',
        credentials: 'include',
        headers: {
          'X-User-ID': getUserId()
        }
      });

      if (response.ok) {
        const result = await response.json();
        alert(`Configuration exported successfully to: ${result.path}`);
      } else {
        throw new Error('Export failed');
      }
    } catch (err) {
      console.error('Failed to export Claude configuration:', err);
      setError('Failed to export configuration to Claude Desktop');
    }
  };

  const downloadClaudeConfig = async () => {
    try {
      const response = await fetch('/api/claude/config', {
        credentials: 'include',
        headers: {
          'X-User-ID': getUserId()
        }
      });

      if (response.ok) {
        const config = await response.json();
        const blob = new Blob([JSON.stringify(config, null, 2)], { 
          type: 'application/json' 
        });
        
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'claude_desktop_config.json';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      } else {
        throw new Error('Download failed');
      }
    } catch (err) {
      console.error('Failed to download Claude configuration:', err);
      setError('Failed to download configuration file');
    }
  };

  const getUserId = (): string => {
    // Get or generate user ID for session management
    let userId = sessionStorage.getItem('oauth_user_id');
    if (!userId) {
      userId = `user_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
      sessionStorage.setItem('oauth_user_id', userId);
    }
    return userId;
  };

  const getAuthStatus = (providerId: string): OAuthStatus | null => {
    return authStatuses.get(providerId) || null;
  };

  const isLoading = (providerId: string): boolean => {
    return loading.has(providerId);
  };

  const renderAuthButton = (provider: OAuthProvider) => {
    const status = getAuthStatus(provider.id);
    const isLoadingProvider = isLoading(provider.id);

    if (!provider.requiresAuth) {
      return (
        <Badge variant="secondary" className="flex items-center gap-1">
          <CheckCircle className="w-3 h-3" />
          No Auth Required
        </Badge>
      );
    }

    if (isLoadingProvider) {
      return (
        <Button disabled size="sm">
          Authenticating...
        </Button>
      );
    }

    if (status?.authenticated) {
      return (
        <div className="flex items-center gap-2">
          <Badge variant="default" className="flex items-center gap-1">
            <CheckCircle className="w-3 h-3" />
            Connected
          </Badge>
          <Button
            variant="outline"
            size="sm"
            onClick={() => revokeAuth(provider.id)}
          >
            Disconnect
          </Button>
        </div>
      );
    }

    return (
      <Button
        variant="outline"
        size="sm"
        onClick={() => initiateOAuth(provider.id)}
        className="flex items-center gap-1"
      >
        <ExternalLink className="w-3 h-3" />
        Connect
      </Button>
    );
  };

  return (
    <div className="space-y-4">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <ExternalLink className="w-5 h-5" />
            OAuth Provider Authentication
          </CardTitle>
          <p className="text-sm text-muted-foreground">
            Connect to external services to enable full MCP functionality. 
            Claude-compatible OAuth flows with secure popup handling.
          </p>
        </CardHeader>
        <CardContent>
          {error && (
            <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded-md flex items-center gap-2 text-red-700">
              <AlertCircle className="w-4 h-4 flex-shrink-0" />
              <span className="text-sm">{error}</span>
            </div>
          )}
          
          <div className="grid gap-4">
            {OAUTH_PROVIDERS.map((provider) => (
              <Card key={provider.id} className="border border-gray-200">
                <CardContent className="p-4">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-2 mb-2">
                        <h3 className="font-medium">{provider.name}</h3>
                        {provider.official && (
                          <Badge variant="outline" className="text-xs">
                            Official
                          </Badge>
                        )}
                        <Badge variant="secondary" className="text-xs">
                          {provider.category}
                        </Badge>
                      </div>
                      <p className="text-sm text-muted-foreground mb-2">
                        {provider.description}
                      </p>
                      {provider.scopes && (
                        <div className="text-xs text-muted-foreground">
                          <strong>Scopes:</strong> {provider.scopes.join(', ')}
                        </div>
                      )}
                    </div>
                    <div className="ml-4 flex-shrink-0">
                      {renderAuthButton(provider)}
                    </div>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
          
          <div className="mt-6 space-y-4">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2 text-lg">
                  <ExternalLink className="w-5 h-5" />
                  Claude Desktop Integration
                </CardTitle>
                <p className="text-sm text-muted-foreground">
                  Export your authenticated MCP servers for use with Claude Desktop
                </p>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <Button
                    onClick={exportClaudeConfig}
                    className="flex items-center gap-2"
                    disabled={loading.size > 0}
                  >
                    <ExternalLink className="w-4 h-4" />
                    Export to Claude Desktop
                  </Button>
                  <Button
                    variant="outline"
                    onClick={downloadClaudeConfig}
                    className="flex items-center gap-2"
                    disabled={loading.size > 0}
                  >
                    <ExternalLink className="w-4 h-4" />
                    Download Config File
                  </Button>
                </div>
                
                <div className="bg-green-50 p-4 border border-green-200 rounded-md">
                  <h4 className="font-medium text-green-900 mb-2">Perfect Claude Compatibility</h4>
                  <div className="text-sm text-green-700 space-y-1">
                    <p>• OAuth tokens are automatically included in exported configuration</p>
                    <p>• All server definitions follow Claude Desktop format specifications</p>
                    <p>• Supports both stdio and SSE transport methods</p>
                    <p>• Environment variables are properly configured for each service</p>
                  </div>
                </div>
              </CardContent>
            </Card>
            
            <div className="p-4 bg-blue-50 border border-blue-200 rounded-md">
              <h4 className="font-medium text-blue-900 mb-2">Security Notice</h4>
              <p className="text-sm text-blue-700">
                All OAuth flows use secure popup windows and PKCE where supported. 
                Tokens are stored securely and can be revoked at any time. 
                Only the minimum required permissions are requested for each service.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
};