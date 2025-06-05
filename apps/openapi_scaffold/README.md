# OpenAPI Scaffold

A powerful self-scaffolding OpenAPI server generator for Erlang/OTP that can dynamically create API servers from any OpenAPI 3.0+ specification.

## Features

- **Dynamic API Generation**: Automatically generate Erlang modules, records, and handlers from OpenAPI schemas
- **Real-time Route Management**: Dynamic route creation and management based on OpenAPI paths
- **Request/Response Validation**: Built-in validation against OpenAPI schemas
- **API Proxying**: Optional proxying to actual API endpoints with configurable settings
- **Hot Reloading**: Automatic reload when OpenAPI spec files change
- **Introspection API**: Built-in endpoints for exploring loaded specs, routes, and schemas
- **Mock Data Generation**: Automatic mock response generation based on schemas

## Architecture

```
openapi_scaffold
├── openapi_parser        - Parses YAML/JSON OpenAPI specifications
├── openapi_codegen       - Generates Erlang code from schemas
├── openapi_router        - Manages dynamic routing
├── openapi_validator     - Validates requests/responses
├── openapi_proxy         - Proxies to actual APIs
└── openapi_introspection - Provides API exploration endpoints
```

## Usage

### Starting the System

```erlang
%% Start the application
application:start(openapi_scaffold).

%% Parse and load an OpenAPI spec
{ok, Spec} = openapi_parser:parse_file("path/to/openapi.yaml").
ok = openapi_router:load_spec(<<"my_api">>, Spec).
```

### Configuration

```erlang
%% In sys.config
{openapi_scaffold, [
    {specs_dir, "priv/openapi_specs"},           % Directory for spec files
    {generated_dir, "src/generated"},             % Generated code directory
    {proxy_enabled, true},                        % Enable API proxying
    {validation_enabled, true},                   % Enable validation
    {introspection_enabled, true},                % Enable introspection
    {hot_reload_enabled, true}                    % Enable hot reloading
]}.
```

### Introspection Endpoints

- `GET /openapi/specs` - List all loaded specifications
- `GET /openapi/specs/:spec_id` - Get detailed info about a spec
- `GET /openapi/routes/:spec_id` - List all routes for a spec
- `GET /openapi/schemas/:spec_id` - Get all schemas for a spec
- `GET /health` - System health check

### Generated Code

For each OpenAPI spec, the system generates:

1. **Schema Modules** - One module per schema with:
   - Record definitions
   - Constructor functions (`new/0`, `new/1`)
   - Conversion functions (`to_map/1`, `from_map/1`)
   - Validation functions (`validate/1`)

2. **Handler Modules** - One module per path with:
   - Cowboy handler implementation
   - Method-specific handlers
   - Request validation
   - Response generation

3. **Client Module** - API client with:
   - Functions for each operation
   - Type-safe request building
   - Response parsing

### Example: OpenAI API

```erlang
%% Load OpenAI spec
{ok, Spec} = openapi_parser:parse_file("openai.yaml").
ok = openapi_router:load_spec(<<"openai">>, Spec).

%% The system automatically generates:
%% - openai_schema_chatcompletionrequest
%% - openai_schema_chatcompletionresponse
%% - openai_handler_v1_chat_completions
%% - openai_client
%% etc.

%% Use generated schema
ChatReq = openai_schema_chatcompletionrequest:new([
    {model, <<"gpt-4">>},
    {messages, [#{role => <<"user">>, content => <<"Hello!">>}]}
]).

%% Validate
ok = openai_schema_chatcompletionrequest:validate(ChatReq).

%% Convert to map for API call
ReqMap = openai_schema_chatcompletionrequest:to_map(ChatReq).
```

### Proxy Configuration

```erlang
%% Add proxy configuration for a spec
openapi_proxy:add_proxy_config(<<"openai">>, #{
    <<"base_url">> => <<"https://api.openai.com">>,
    <<"headers">> => [{<<"Authorization">>, <<"Bearer YOUR_KEY">>}],
    <<"timeout">> => 60000
}).
```

### Hot Reloading

When a spec file changes, the system automatically:
1. Reloads the specification
2. Regenerates affected modules
3. Updates routing tables
4. Recompiles generated code

## Demo

Run the OpenAI demo:

```bash
./examples/openai_scaffold_demo.erl
```

This will:
1. Download the OpenAI OpenAPI specification
2. Generate all required modules
3. Start a mock server on http://localhost:8081
4. Demonstrate API calls and introspection

## Advanced Features

### Custom Validation

```erlang
%% Disable validation for performance
openapi_validator:set_validation_enabled(false).
```

### Custom Code Generation

The code generator can be extended to generate additional artifacts:
- Database models
- GraphQL schemas
- API documentation
- Test suites

### Multiple Spec Support

Load multiple API specs simultaneously:

```erlang
openapi_router:load_spec(<<"openai">>, OpenAISpec).
openapi_router:load_spec(<<"stripe">>, StripeSpec).
openapi_router:load_spec(<<"github">>, GitHubSpec).
```

Each spec maintains its own namespace and routes.