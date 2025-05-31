import React, { useState } from 'react';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle } from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Shield, Key, Globe, Plus, CheckCircle, XCircle } from 'lucide-react'

interface SeedServer {
  id: string;
  name: string;
  category: string;
  url: string;
  auth_type: 'oauth2' | 'api_key' | 'open';
  maintainer: string;
  description: string;
}

interface SeedServersDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onServerAdded: () => void;
}

const SEED_SERVERS: SeedServer[] = [
  {
    id: "atlassian",
    name: "Atlassian",
    category: "Software Development",
    url: "https://mcp.atlassian.com/v1/sse",
    auth_type: "oauth2",
    maintainer: "Atlassian",
    description: "Access Atlassian services including Jira and Confluence"
  },
  {
    id: "asana",
    name: "Asana",
    category: "Project Management",
    url: "https://mcp.asana.com/sse",
    auth_type: "oauth2",
    maintainer: "Asana",
    description: "Project management and task tracking"
  },
  {
    id: "linear",
    name: "Linear",
    category: "Project Management",
    url: "https://mcp.linear.app/sse",
    auth_type: "oauth2",
    maintainer: "Linear",
    description: "Issue tracking and project management for software teams"
  },
  {
    id: "stripe",
    name: "Stripe",
    category: "Payments",
    url: "https://mcp.stripe.com/",
    auth_type: "api_key",
    maintainer: "Stripe",
    description: "Payment processing and billing management"
  },
  {
    id: "cloudflare_workers",
    name: "Cloudflare Workers",
    category: "Software Development",
    url: "https://bindings.mcp.cloudflare.com/sse",
    auth_type: "oauth2",
    maintainer: "Cloudflare",
    description: "Deploy and manage Cloudflare Workers"
  },
  {
    id: "simplescraper",
    name: "Simplescraper",
    category: "Web Scraping",
    url: "https://mcp.simplescraper.com/mcp",
    auth_type: "oauth2",
    maintainer: "Simplescraper",
    description: "Web scraping and data extraction"
  },
  {
    id: "gitmcp",
    name: "GitMCP",
    category: "Software Development",
    url: "https://gitmcp.io/docs",
    auth_type: "open",
    maintainer: "GitMCP",
    description: "Git repository management and code analysis"
  },
  {
    id: "mcpoogle",
    name: "McPoogle",
    category: "MCP Server Search Engine",
    url: "https://mcp.mcpoogle.com/sse",
    auth_type: "open",
    maintainer: "McPoogle",
    description: "Search and discover MCP servers"
  },
  {
    id: "hubspot",
    name: "HubSpot",
    category: "CRM",
    url: "https://app.hubspot.com/mcp/v1/http",
    auth_type: "api_key",
    maintainer: "HubSpot",
    description: "Customer relationship management"
  },
  {
    id: "zapier",
    name: "Zapier",
    category: "Automation",
    url: "https://mcp.zapier.com/api/mcp/mcp",
    auth_type: "api_key",
    maintainer: "Zapier",
    description: "Workflow automation across apps"
  }
];

export const SeedServersDialog: React.FC<SeedServersDialogProps> = ({
  open,
  onOpenChange,
  onServerAdded
}) => {
  const [addingServers, setAddingServers] = useState<Set<string>>(new Set());
  const [addedServers, setAddedServers] = useState<Set<string>>(new Set());
  const [errors, setErrors] = useState<Map<string, string>>(new Map());

  const getAuthIcon = (authType: string) => {
    switch (authType) {
      case 'oauth2': return <Shield className="w-4 h-4" />;
      case 'api_key': return <Key className="w-4 h-4" />;
      case 'open': return <Globe className="w-4 h-4" />;
      default: return null;
    }
  };

  const getAuthColor = (authType: string) => {
    switch (authType) {
      case 'oauth2': return 'text-green-600 bg-green-100';
      case 'api_key': return 'text-yellow-600 bg-yellow-100';
      case 'open': return 'text-blue-600 bg-blue-100';
      default: return 'text-gray-600 bg-gray-100';
    }
  };

  const addServer = async (server: SeedServer) => {
    setAddingServers(prev => new Set(prev).add(server.id));
    setErrors(prev => {
      const next = new Map(prev);
      next.delete(server.id);
      return next;
    });

    try {
      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: server.name,
          url: server.url,
          type: 'remote',
          metadata: {
            category: server.category,
            auth_type: server.auth_type,
            maintainer: server.maintainer,
            description: server.description
          }
        })
      });

      if (response.ok) {
        setAddedServers(prev => new Set(prev).add(server.id));
        onServerAdded();
      } else {
        let errorMessage = 'Failed to add server';
        try {
          const errorData = await response.json();
          errorMessage = errorData.error || errorMessage;
        } catch {
          errorMessage = await response.text() || errorMessage;
        }
        console.error('Failed to add server:', server.name, errorMessage);
        setErrors(prev => new Map(prev).set(server.id, errorMessage));
      }
    } catch (error) {
      setErrors(prev => new Map(prev).set(server.id, 'Failed to add server'));
    } finally {
      setAddingServers(prev => {
        const next = new Set(prev);
        next.delete(server.id);
        return next;
      });
    }
  };

  const categories = [...new Set(SEED_SERVERS.map(s => s.category))].sort();

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="max-w-4xl max-h-[80vh] overflow-y-auto">
        <DialogHeader>
          <DialogTitle>Add MCP Servers from Curated List</DialogTitle>
          <DialogDescription>
            Select from our curated list of high-quality, production-ready MCP servers.
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-6 mt-4">
          {categories.map(category => (
            <div key={category}>
              <h3 className="text-sm font-semibold text-gray-700 mb-3">{category}</h3>
              <div className="space-y-2">
                {SEED_SERVERS
                  .filter(server => server.category === category)
                  .map(server => (
                    <div
                      key={server.id}
                      className="border rounded-lg p-4 flex items-center justify-between hover:bg-gray-50"
                    >
                      <div className="flex-1">
                        <div className="flex items-center gap-2">
                          <h4 className="font-medium">{server.name}</h4>
                          <Badge className={getAuthColor(server.auth_type)}>
                            {getAuthIcon(server.auth_type)}
                            <span className="ml-1">{server.auth_type.toUpperCase()}</span>
                          </Badge>
                        </div>
                        <p className="text-sm text-gray-600 mt-1">{server.description}</p>
                        <div className="flex items-center gap-4 mt-2 text-xs text-gray-500">
                          <span>Maintainer: {server.maintainer}</span>
                          <span className="font-mono">{server.url}</span>
                        </div>
                        {errors.has(server.id) && (
                          <p className="text-sm text-red-600 mt-2">{errors.get(server.id)}</p>
                        )}
                      </div>
                      <div className="ml-4">
                        {addedServers.has(server.id) ? (
                          <CheckCircle className="h-5 w-5 text-green-500" />
                        ) : errors.has(server.id) ? (
                          <XCircle className="h-5 w-5 text-red-500" />
                        ) : (
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={() => addServer(server)}
                            disabled={addingServers.has(server.id)}
                          >
                            {addingServers.has(server.id) ? (
                              "Adding..."
                            ) : (
                              <>
                                <Plus className="h-4 w-4 mr-1" />
                                Add
                              </>
                            )}
                          </Button>
                        )}
                      </div>
                    </div>
                  ))}
              </div>
            </div>
          ))}
        </div>
      </DialogContent>
    </Dialog>
  );
};