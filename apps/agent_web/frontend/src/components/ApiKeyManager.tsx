import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Badge } from "@/components/ui/badge";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { 
  Key, 
  AlertCircle, 
  CheckCircle, 
  ExternalLink,
  Eye,
  EyeOff,
  Save,
  RefreshCw,
  Shield,
  AlertTriangle
} from 'lucide-react';

interface ApiRequirement {
  name: string;
  required_fields: Array<{
    key: string;
    env: string;
    description: string;
    pattern?: string;
  }>;
  optional_fields?: Array<{
    key: string;
    env: string;
    description?: string;
    default?: string;
  }>;
  documentation_url: string;
}

interface ApiKeyStatus {
  service: string;
  missing_fields: string[];
}

interface ServiceStatus {
  name: string;
  configured: boolean;
  fields: Array<{
    field: string;
    configured: boolean;
    description: string;
  }>;
  documentation_url: string;
}

export function ApiKeyManager() {
  const [requirements, setRequirements] = useState<Record<string, ApiRequirement>>({});
  const [keyStatus, setKeyStatus] = useState<Record<string, ServiceStatus>>({});
  const [missingKeys, setMissingKeys] = useState<ApiKeyStatus[]>([]);
  const [selectedService, setSelectedService] = useState<string>('openai');
  const [serviceKeys, setServiceKeys] = useState<Record<string, any>>({});
  const [showKeys, setShowKeys] = useState<Record<string, boolean>>({});
  const [isSaving, setIsSaving] = useState(false);
  const [saveStatus, setSaveStatus] = useState<{type: 'success' | 'error', message: string} | null>(null);

  useEffect(() => {
    loadApiKeyStatus();
  }, []);

  const loadApiKeyStatus = async () => {
    try {
      const response = await fetch('/api/keys');
      if (response.ok) {
        const data = await response.json();
        setRequirements(data.requirements || {});
        setMissingKeys(data.missing || []);
        
        // Check detailed status
        const statusResponse = await fetch('/api/keys/check');
        if (statusResponse.ok) {
          const statusData = await statusResponse.json();
          setKeyStatus(statusData.details || {});
        }
      }
    } catch (error) {
      console.error('Failed to load API key status:', error);
    }
  };

  const loadServiceKeys = async (service: string) => {
    try {
      const response = await fetch(`/api/keys/${service}`);
      if (response.ok) {
        const data = await response.json();
        setServiceKeys(data.keys || {});
      }
    } catch (error) {
      console.error('Failed to load service keys:', error);
    }
  };

  useEffect(() => {
    if (selectedService) {
      loadServiceKeys(selectedService);
    }
  }, [selectedService]);

  const handleSaveKeys = async () => {
    setIsSaving(true);
    setSaveStatus(null);
    
    try {
      const response = await fetch(`/api/keys/${selectedService}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(serviceKeys)
      });
      
      if (response.ok) {
        setSaveStatus({ type: 'success', message: 'API keys saved successfully!' });
        await loadApiKeyStatus();
      } else {
        const error = await response.json();
        setSaveStatus({ type: 'error', message: error.error || 'Failed to save keys' });
      }
    } catch (error) {
      setSaveStatus({ type: 'error', message: 'Network error while saving keys' });
    } finally {
      setIsSaving(false);
    }
  };

  const handleLoadFromEnv = async () => {
    try {
      const response = await fetch('/api/keys/load-env', { method: 'POST' });
      if (response.ok) {
        await loadApiKeyStatus();
        await loadServiceKeys(selectedService);
        setSaveStatus({ type: 'success', message: 'Loaded from environment variables' });
      }
    } catch (error) {
      console.error('Failed to load from environment:', error);
    }
  };

  const toggleShowKey = (field: string) => {
    setShowKeys(prev => ({ ...prev, [field]: !prev[field] }));
  };

  const updateServiceKey = (field: string, value: string) => {
    setServiceKeys(prev => ({ ...prev, [field]: value }));
  };

  const getServiceIcon = (service: string) => {
    const icons: Record<string, string> = {
      openai: '🤖',
      anthropic: '🧠',
      slack: '💬',
      linear: '📋',
      plaid: '💳',
      graphlit: '🔗',
      github: '🐙',
      jina: '🔍'
    };
    return icons[service] || '🔑';
  };

  const hasMissingKeys = missingKeys.length > 0;

  return (
    <div className="p-6 space-y-6 max-w-6xl mx-auto">
      {/* Header Alert */}
      {hasMissingKeys && (
        <Alert variant="destructive">
          <AlertTriangle className="h-4 w-4" />
          <AlertTitle>API Keys Required</AlertTitle>
          <AlertDescription>
            Some services are missing required API keys. Please configure them below to enable all features.
          </AlertDescription>
        </Alert>
      )}

      {/* Overview Card */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Shield className="h-5 w-5" />
            API Key Configuration
          </CardTitle>
          <CardDescription>
            Manage API keys for external services. Keys are stored securely and never exposed in the UI.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {Object.entries(keyStatus).map(([service, status]) => (
              <div
                key={service}
                className={`p-4 rounded-lg border cursor-pointer transition-colors ${
                  selectedService === service ? 'border-primary bg-primary/5' : ''
                }`}
                onClick={() => setSelectedService(service)}
              >
                <div className="flex items-center justify-between mb-2">
                  <span className="text-2xl">{getServiceIcon(service)}</span>
                  {status.configured ? (
                    <CheckCircle className="h-5 w-5 text-green-500" />
                  ) : (
                    <AlertCircle className="h-5 w-5 text-yellow-500" />
                  )}
                </div>
                <h3 className="font-medium capitalize">{service}</h3>
                <p className="text-sm text-muted-foreground">
                  {status.configured ? 'Configured' : 'Setup required'}
                </p>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Service Configuration */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="flex items-center gap-2">
                <span className="text-2xl">{getServiceIcon(selectedService)}</span>
                {requirements[selectedService]?.name || selectedService} Configuration
              </CardTitle>
              <CardDescription>
                Configure API keys and settings for this service
              </CardDescription>
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={() => window.open(requirements[selectedService]?.documentation_url, '_blank')}
            >
              <ExternalLink className="h-4 w-4 mr-2" />
              Documentation
            </Button>
          </div>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Required Fields */}
          {requirements[selectedService]?.required_fields && (
            <div className="space-y-4">
              <h3 className="text-sm font-medium text-muted-foreground">Required Fields</h3>
              {requirements[selectedService].required_fields.map((field) => (
                <div key={field.key} className="space-y-2">
                  <Label htmlFor={field.key}>
                    {field.description || field.key}
                    <Badge variant="secondary" className="ml-2 text-xs">
                      {field.env}
                    </Badge>
                  </Label>
                  <div className="flex gap-2">
                    <div className="relative flex-1">
                      <Input
                        id={field.key}
                        type={showKeys[field.key] ? 'text' : 'password'}
                        value={serviceKeys[field.key] || ''}
                        onChange={(e) => updateServiceKey(field.key, e.target.value)}
                        placeholder={field.pattern ? `Format: ${field.pattern}...` : 'Enter key'}
                        className="pr-10"
                      />
                      <Button
                        variant="ghost"
                        size="icon"
                        className="absolute right-0 top-0 h-full px-3"
                        onClick={() => toggleShowKey(field.key)}
                      >
                        {showKeys[field.key] ? (
                          <EyeOff className="h-4 w-4" />
                        ) : (
                          <Eye className="h-4 w-4" />
                        )}
                      </Button>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}

          {/* Optional Fields */}
          {requirements[selectedService]?.optional_fields && 
           requirements[selectedService].optional_fields.length > 0 && (
            <div className="space-y-4">
              <h3 className="text-sm font-medium text-muted-foreground">Optional Fields</h3>
              {requirements[selectedService].optional_fields.map((field) => (
                <div key={field.key} className="space-y-2">
                  <Label htmlFor={field.key}>
                    {field.description || field.key}
                    <Badge variant="outline" className="ml-2 text-xs">
                      {field.env}
                    </Badge>
                  </Label>
                  <Input
                    id={field.key}
                    type="text"
                    value={serviceKeys[field.key] || ''}
                    onChange={(e) => updateServiceKey(field.key, e.target.value)}
                    placeholder={field.default || 'Optional'}
                  />
                </div>
              ))}
            </div>
          )}

          {/* Save Status */}
          {saveStatus && (
            <Alert variant={saveStatus.type === 'success' ? 'default' : 'destructive'}>
              {saveStatus.type === 'success' ? (
                <CheckCircle className="h-4 w-4" />
              ) : (
                <AlertCircle className="h-4 w-4" />
              )}
              <AlertDescription>{saveStatus.message}</AlertDescription>
            </Alert>
          )}

          {/* Action Buttons */}
          <div className="flex gap-2">
            <Button onClick={handleSaveKeys} disabled={isSaving}>
              {isSaving ? (
                <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
              ) : (
                <Save className="h-4 w-4 mr-2" />
              )}
              Save Configuration
            </Button>
            <Button variant="outline" onClick={handleLoadFromEnv}>
              <RefreshCw className="h-4 w-4 mr-2" />
              Load from Environment
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Security Notice */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Key className="h-5 w-5" />
            Security Information
          </CardTitle>
        </CardHeader>
        <CardContent>
          <ul className="space-y-2 text-sm text-muted-foreground">
            <li>• API keys are stored locally in <code>config/api_keys.config</code></li>
            <li>• This file is excluded from version control via .gitignore</li>
            <li>• Keys are never transmitted except to their respective services</li>
            <li>• Environment variables take precedence over config file values</li>
            <li>• Use dummy values for development/testing environments</li>
          </ul>
        </CardContent>
      </Card>
    </div>
  );
}