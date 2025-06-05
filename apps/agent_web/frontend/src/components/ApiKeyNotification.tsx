import React, { useState, useEffect } from 'react';
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { Button } from "@/components/ui/button";
import { X, Key, AlertTriangle } from 'lucide-react';

interface ApiKeyNotificationProps {
  onDismiss?: () => void;
  onConfigure?: () => void;
}

export function ApiKeyNotification({ onDismiss, onConfigure }: ApiKeyNotificationProps) {
  const [show, setShow] = useState(false);
  const [missingServices, setMissingServices] = useState<string[]>([]);

  useEffect(() => {
    checkApiKeys();
  }, []);

  const checkApiKeys = async () => {
    try {
      const response = await fetch('/api/keys/check');
      if (response.ok) {
        const data = await response.json();
        if (data.status === 'missing_keys' && data.missing) {
          const services = data.missing.map((m: any) => m.service);
          setMissingServices(services);
          setShow(true);
        }
      }
    } catch (error) {
      console.error('Failed to check API keys:', error);
    }
  };

  const handleDismiss = () => {
    setShow(false);
    if (onDismiss) onDismiss();
  };

  const handleConfigure = () => {
    setShow(false);
    if (onConfigure) onConfigure();
  };

  if (!show || missingServices.length === 0) {
    return null;
  }

  const serviceNames: Record<string, string> = {
    openai: 'OpenAI',
    anthropic: 'Anthropic',
    slack: 'Slack',
    linear: 'Linear',
    plaid: 'Plaid',
    graphlit: 'Graphlit',
    github: 'GitHub',
    jina: 'Jina AI'
  };

  return (
    <Alert className="mb-4 relative border-yellow-200 bg-yellow-50">
      <AlertTriangle className="h-4 w-4 text-yellow-600" />
      <AlertTitle className="text-yellow-800">API Keys Required</AlertTitle>
      <AlertDescription className="text-yellow-700">
        <div className="mt-2">
          <p>The following services need API keys to function properly:</p>
          <ul className="mt-2 space-y-1">
            {missingServices.map(service => (
              <li key={service} className="flex items-center gap-2">
                <Key className="h-3 w-3" />
                <span className="font-medium">
                  {serviceNames[service] || service}
                </span>
              </li>
            ))}
          </ul>
          <div className="mt-4 flex gap-2">
            <Button
              size="sm"
              onClick={handleConfigure}
              className="bg-yellow-600 hover:bg-yellow-700 text-white"
            >
              Configure API Keys
            </Button>
            <Button
              size="sm"
              variant="ghost"
              onClick={handleDismiss}
              className="text-yellow-700 hover:text-yellow-800"
            >
              Dismiss
            </Button>
          </div>
        </div>
      </AlertDescription>
      <button
        onClick={handleDismiss}
        className="absolute top-2 right-2 text-yellow-600 hover:text-yellow-800"
      >
        <X className="h-4 w-4" />
      </button>
    </Alert>
  );
}