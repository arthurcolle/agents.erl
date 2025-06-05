# Jina AI Tools for Erlang Agents

This document describes the comprehensive Jina AI integration for the Erlang Agents MCP system, providing powerful search, embedding, classification, and document processing capabilities.

## Overview

The Jina AI tools integrate with [Jina AI's Search Foundation APIs](https://jina.ai/) to provide:

- **Web Search** - Structured web search with clean results
- **Content Extraction** - Read and extract content from any webpage  
- **Fact Checking** - Ground statements against reliable sources
- **Text Embeddings** - Create vector representations for semantic search
- **Image Embeddings** - Multimodal embeddings for images
- **Document Reranking** - Improve search relevance with AI reranking
- **Text Classification** - Categorize content into predefined labels
- **Content Segmentation** - Split large text into semantic chunks
- **Deep Search** - Advanced search with reasoning and citations

## Setup

### 1. Get Jina AI API Key

Get your free Jina AI API key at: https://jina.ai/?sui=apikey

### 2. Set Environment Variable

```bash
export JINA_API_KEY="your_api_key_here"
```

### 3. Start the Server

```bash
# Start web application with Jina tools
./start_mcp web

# Or start Inspector-compatible server
./start_mcp inspector
```

When the `JINA_API_KEY` environment variable is set, the system automatically registers all Jina AI tools. If the key is not set, the tools are skipped and a warning is logged.

## Available Tools

### 🔍 `jina_search`
**Search the web with structured results**

```json
{
  "query": "artificial intelligence trends 2024",
  "num_results": 5,
  "site": "arxiv.org",
  "country": "US",
  "language": "en"
}
```

### 📄 `jina_read_webpage`  
**Extract clean content from any webpage**

```json
{
  "url": "https://example.com/article",
  "target_selector": ".article-content",
  "remove_selector": ".ads",
  "timeout": 30,
  "no_cache": false
}
```

### ✅ `jina_fact_check`
**Verify statements against reliable sources**

```json
{
  "query": "The Earth is round and orbits the Sun"
}
```

### 🧠 `jina_embed_text`
**Create vector embeddings for text**

```json
{
  "text": "Machine learning is transforming industries",
  "model": "jina-embeddings-v3"
}
```

### 🖼️ `jina_embed_image`
**Create multimodal embeddings for images**

```json
{
  "image_url": "https://example.com/image.jpg",
  "model": "jina-clip-v2"
}
```

### 📈 `jina_rerank`
**Rerank documents by relevance to a query**

```json
{
  "query": "machine learning applications",
  "documents": "Document 1 text---Document 2 text---Document 3 text",
  "model": "jina-reranker-v2-base-multilingual",
  "top_n": 3
}
```

### 🏷️ `jina_classify`
**Classify text or images into categories**

```json
{
  "inputs": "This product is amazing---This product is terrible",
  "labels": "positive, negative, neutral",
  "is_image": false,
  "model": "jina-embeddings-v3"
}
```

### ✂️ `jina_segment`
**Split text into semantic chunks**

```json
{
  "content": "Long text content to be segmented...",
  "tokenizer": "cl100k_base",
  "max_chunk_length": 1000,
  "return_tokens": false
}
```

### 🔬 `jina_deep_search`
**Advanced search with reasoning and citations**

```json
{
  "query": "How does quantum computing impact cryptography?",
  "reasoning_effort": "high",
  "max_returned_urls": 5,
  "boost_hostnames": "arxiv.org,nature.com",
  "bad_hostnames": "spam-site.com"
}
```

## API Limits

- **Embedding & Reranker APIs**: 500 RPM & 1M TPM (free), 2K RPM & 5M TPM (premium)
- **Reader APIs** (r.jina.ai, s.jina.ai): 200 RPM (free), 2K RPM (premium)  
- **Classifier APIs**: 20 RPM & 200K TPM (free), 60 RPM & 1M TPM (premium)
- **Segmenter API**: 200 RPM (free), 1K RPM (premium)

## Usage Examples

### Example 1: Research Workflow

```bash
# Search for recent AI papers
./start_mcp call agents_main jina_search '{"query": "transformer architecture 2024", "site": "arxiv.org"}'

# Read a specific paper
./start_mcp call agents_main jina_read_webpage '{"url": "https://arxiv.org/abs/2024.xxxxx"}'

# Fact-check claims from the paper
./start_mcp call agents_main jina_fact_check '{"query": "Transformers achieve state-of-the-art performance on language tasks"}'
```

### Example 2: Content Processing Pipeline

```bash
# Segment long content
./start_mcp call agents_main jina_segment '{"content": "Very long article text...", "max_chunk_length": 500}'

# Create embeddings for chunks
./start_mcp call agents_main jina_embed_text '{"text": "First chunk of content"}'

# Classify content
./start_mcp call agents_main jina_classify '{"inputs": "Technical documentation", "labels": "tutorial, reference, guide"}'
```

### Example 3: Search and Rerank

```bash
# Perform initial search
./start_mcp call agents_main jina_search '{"query": "machine learning tutorials"}'

# Rerank results by specific criteria
./start_mcp call agents_main jina_rerank '{"query": "beginner-friendly ML tutorials", "documents": "Result 1---Result 2---Result 3"}'
```

### Example 4: Advanced Deep Search

```bash
# Deep search with reasoning
./start_mcp call agents_main jina_deep_search '{"query": "What are the environmental impacts of large language models?", "reasoning_effort": "high"}'
```

## Inspector Integration

The Jina AI tools are fully compatible with the MCP Inspector:

```bash
# Export configuration for Inspector
./start_mcp export all

# Use with Inspector
npx @modelcontextprotocol/inspector --config mcp.json --server agents_main
```

## Architecture

### Components

1. **`jina_client.erl`** - HTTP client for Jina AI APIs
2. **`jina_tools.erl`** - Tool implementations and MCP integration
3. **`mcp_server.erl`** - Automatic tool registration

### Key Features

- **Automatic Registration** - Tools are automatically registered when API key is present
- **Error Handling** - Comprehensive error handling with meaningful messages
- **Type Safety** - Full JSON schema validation for all tool parameters
- **Async Support** - Built for high-performance concurrent operations
- **Inspector Compatible** - Full compatibility with official MCP Inspector

### Error Handling

All tools provide structured error responses:

```json
{
  "error": {
    "code": "JINA_API_KEY_MISSING",
    "message": "JINA_API_KEY environment variable not set. Get your free key at: https://jina.ai/?sui=apikey"
  }
}
```

## Best Practices

### 1. API Key Management
- Store API key securely in environment variables
- Never commit API keys to version control
- Rotate keys regularly for production use

### 2. Rate Limiting
- Implement backoff strategies for high-volume usage
- Cache results when appropriate
- Use batch operations when available

### 3. Content Processing
- Chunk large documents before embedding
- Use appropriate models for your use case
- Validate and sanitize input content

### 4. Search Optimization
- Use specific queries for better results
- Leverage site restrictions when appropriate
- Combine search with reranking for best relevance

## Troubleshooting

### Common Issues

1. **API Key Not Found**
   ```
   Error: JINA_API_KEY environment variable not set
   Solution: Set the environment variable with your API key
   ```

2. **Rate Limit Exceeded**
   ```
   Error: HTTP 429: Too Many Requests
   Solution: Implement rate limiting and retry logic
   ```

3. **Invalid URL Format**
   ```
   Error: Invalid URL provided
   Solution: Ensure URLs are properly formatted with protocol
   ```

### Debugging

Enable debug logging:

```bash
# Set log level for detailed output
export ERLANG_LOG_LEVEL=debug
./start_mcp web
```

## Contributing

To add new Jina AI capabilities:

1. Add the API call to `jina_client.erl`
2. Implement the tool in `jina_tools.erl`
3. Register the tool in `mcp_server.erl`
4. Add tests and documentation

## Resources

- [Jina AI Documentation](https://jina.ai/dev)
- [Get API Key](https://jina.ai/?sui=apikey)
- [MCP Specification](https://modelcontextprotocol.io)
- [Erlang Agents Documentation](./README.md)

## License

This integration is part of the agents.erl project and follows the same license terms.