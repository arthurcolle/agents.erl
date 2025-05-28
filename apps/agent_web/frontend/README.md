# Erlang Agent System - Modern UI with shadcn/ui

This is a modern React-based frontend for the Erlang Agent System, built with shadcn/ui components for a beautiful and consistent UI.

## Features

- **Modern UI Components**: Built with shadcn/ui and Radix UI primitives
- **Real-time Updates**: WebSocket integration for live agent monitoring
- **Responsive Design**: Fully responsive layout that works on all devices
- **Dark Mode Ready**: Supports light and dark themes
- **Interactive Dashboard**: Monitor system metrics and agent performance
- **Agent Management**: Create, monitor, and control agents
- **Network Visualization**: See agent topology and connections
- **Chat Interface**: Communicate with AI agents in real-time
- **Examples Gallery**: Run pre-built examples with one click
- **Log Viewer**: Filter and search through system logs

## Setup

1. Install dependencies:
```bash
cd apps/agent_web/frontend
npm install
```

2. Start the development server:
```bash
npm run dev
```

3. Build for production:
```bash
npm run build
```

The built files will be placed in `../priv/static/dist/`

## Switching to the New UI

To use the new React UI instead of the vanilla JS interface:

1. Build the frontend:
```bash
npm run build
```

2. Edit `apps/agent_web/src/agent_web_handler.erl` and uncomment the React UI lines while commenting out the old UI lines.

3. Restart the Erlang application.

## Technology Stack

- **React 18** with TypeScript
- **shadcn/ui** for UI components
- **Tailwind CSS** for styling
- **Vite** for fast development and building
- **Recharts** for data visualization
- **Lucide Icons** for consistent iconography

## Component Structure

```
src/
├── components/
│   ├── ui/           # shadcn/ui components
│   ├── Dashboard.tsx
│   ├── AgentList.tsx
│   ├── ChatInterface.tsx
│   ├── NetworkTopology.tsx
│   ├── MonitoringPanel.tsx
│   ├── ExamplesPanel.tsx
│   └── LogsViewer.tsx
├── lib/
│   └── utils.ts      # Utility functions
├── App.tsx           # Main application
├── main.tsx          # Entry point
└── index.css         # Global styles
```

## Development

The UI is designed to work seamlessly with the existing Erlang backend API. All API calls are proxied through Vite during development.

### Adding New Components

To add new shadcn/ui components:

```bash
npx shadcn-ui@latest add [component-name]
```

### Customizing Theme

Edit the CSS variables in `src/index.css` to customize the color scheme and theme.