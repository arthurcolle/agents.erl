# Graphlit MCP Server Integration

This document explains how to use the integrated Graphlit MCP (Model Context Protocol) server in the agents.erl project.

## Overview

Graphlit is a powerful knowledge management platform that provides:
- Data ingestion from multiple sources (Slack, Discord, websites, Google Drive, email, Jira, Linear, GitHub)
- Web crawling and search capabilities
- RAG (Retrieval Augmented Generation) ready knowledge base
- Structured data extraction
- Multi-format content processing

## Configuration

The Graphlit MCP server has been pre-configured with your project credentials:

- **Organization ID**: `1b591b0d-eb12-4f6d-be5a-ceb95b40e716`
- **Environment ID**: `42d0e0ef-8eeb-463d-921c-ef5119541eb9`
- **API URL**: `https://data-scus.graphlit.io/api/v1/graphql`

Configuration files:
- `apps/agent/src/mcp_server_config.erl` - Erlang server configuration
- `apps/agent_web/priv/mcp_servers.json` - MCP client configuration
- `apps/agent_web/src/mcp_manager.erl` - Auto-connection logic

## Available Capabilities

The Graphlit MCP server provides the following tools:

### Core Operations
- `query_contents` - Search through ingested content
- `query_collections` - Query organized content collections
- `query_feeds` - Query RSS/data feeds
- `retrieve_relevant_sources` - Get relevant documents for queries
- `extract_structured_json` - Extract structured data from text

### Data Ingestion
- `ingest_files` - Upload and process files
- `ingest_web_pages` - Ingest content from web URLs
- `web_crawling` - Crawl websites for content
- `web_search` - Search the web and ingest results

### Integrations
- `slack_integration` - Connect to Slack channels and messages
- `discord_integration` - Connect to Discord servers
- `github_integration` - Access GitHub repositories and issues
- `notion_integration` - Connect to Notion databases
- `linear_integration` - Access Linear project management

## Usage

### 1. Start the Web Application

```bash
./start_web.sh
```

### 2. Access the MCP Dashboard

Navigate to the web interface and look for the MCP server management section. You should see the Graphlit server listed as available.

### 3. Using Graphlit Tools

Example queries you can try:

```javascript
// Search for content
{
  "tool": "query_contents",
  "arguments": {
    "query": "machine learning algorithms",
    "limit": 10
  }
}

// Ingest a web page
{
  "tool": "ingest_web_pages", 
  "arguments": {
    "urls": ["https://example.com/article"],
    "collection": "research_articles"
  }
}

// Extract structured data
{
  "tool": "extract_structured_json",
  "arguments": {
    "text": "Contact John Doe at john@example.com, phone: 555-1234",
    "schema": {
      "type": "object",
      "properties": {
        "name": {"type": "string"},
        "email": {"type": "string"},
        "phone": {"type": "string"}
      }
    }
  }
}
```

### 4. Adding Data Connector Credentials (Optional)

To use external integrations, add these environment variables to your MCP configuration:

```json
{
  "env": {
    "SLACK_BOT_TOKEN": "your-slack-bot-token",
    "DISCORD_BOT_TOKEN": "your-discord-bot-token", 
    "GITHUB_PERSONAL_ACCESS_TOKEN": "your-github-pat",
    "LINEAR_API_KEY": "your-linear-api-key",
    "NOTION_API_KEY": "your-notion-api-key",
    "JIRA_EMAIL": "your-jira-email",
    "JIRA_TOKEN": "your-jira-token"
  }
}
```

## Troubleshooting

### Connection Issues
1. Ensure Node.js is installed (`node --version`)
2. Check that `npx` is available
3. Verify network connectivity to Graphlit API

### Authentication Problems
1. Verify the JWT secret is correct
2. Check organization and environment IDs
3. Ensure the Graphlit project is active

### Integration Not Working
1. Check the MCP manager logs for errors
2. Restart the web application
3. Test the connection with: `escript test_graphlit_integration.erl`

## Documentation Links

- [Graphlit Platform Documentation](https://docs.graphlit.dev/)
- [Graphlit MCP Server GitHub](https://github.com/graphlit/graphlit-mcp-server)
- [Model Context Protocol Specification](https://modelcontextprotocol.io/)

## Support

For Graphlit-specific issues:
- [Graphlit Discord Community](https://discord.gg/graphlit) - Use the #mcp channel
- [GitHub Issues](https://github.com/graphlit/graphlit-mcp-server/issues)

For agents.erl integration issues:
- Check the project's main documentation
- Review MCP manager implementation in `apps/agent_web/src/mcp_manager.erl`