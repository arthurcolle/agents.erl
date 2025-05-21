# Erlang Agent for OpenAI API

A comprehensive, distributed Erlang framework for OpenAI API integration with built-in supervision trees, dynamic API client generation, and tool execution.

## Features

- **Distributed Architecture**: Each API endpoint runs in its own supervised process
- **Dynamic API Client Generation**: Auto-generates client modules from the OpenAI API spec
- **Comprehensive API Coverage**: Supports all OpenAI API endpoints
- **Fault Tolerance**: Built-in supervision trees and error handling
- **Rate Limiting**: Smart rate limiting to prevent API quota issues
- **Streaming Support**: Handles streaming responses
- **Tool Execution**: Register and execute custom tools in your agent
- **Configuration Management**: Centralized configuration with environment variable fallbacks
- **OTP Compliance**: Follows OTP design principles for reliability and scalability
- **Hot Code Swapping**: Supports Erlang's hot code reloading capabilities
- **Concurrency Model**: Leverages Erlang's lightweight processes for maximum concurrency

## Architecture

The system is organized as a hierarchical supervision tree:

```
agent (top-level supervisor)
├── openai_sup (OpenAI API supervisor)
│   ├── openai_generator_sup
│   │   └── openai_generator (generates API client modules)
│   ├── openai_clients_sup (supervises API client processes)
│   │   ├── openai_chat
│   │   ├── openai_completions
│   │   ├── openai_embeddings
│   │   └── ... (one process per API group)
│   ├── openai_rate_limiter (handles rate limiting)
│   └── openai_config (manages configuration)
├── agent_tools (tools registry)
├── agent_registry (tracks active agent processes)
├── agent_discovery (service discovery)
├── agent_messenger (inter-agent communication)
└── agent_protocol (manages communication protocols)
```

## Installation

### Prerequisites

- Erlang/OTP 24 or later
- rebar3 3.18.0 or later
- OpenAI API key

### From Source

```bash
# Clone the repository
git clone https://github.com/yourusername/erlang-agent.git
cd erlang-agent

# Build the project
make compile
# OR
./rebar3 compile
```

### As a Dependency

Add to your `rebar.config`:

```erlang
{deps, [
    {agent, {git, "https://github.com/yourusername/erlang-agent.git", {tag, "v0.1.0"}}}
]}.
```

### Docker

```bash
# Build the Docker image
docker build -t erlang-agent .

# Run with your OpenAI API key
docker run -e OPENAI_API_KEY=your_api_key -p 8080:8080 erlang-agent
```

## Usage

### Starting the Agent

```erlang
% Start the application
application:ensure_all_started(agent).

% OR use the convenience function
agent:start().

% Start with custom configuration
Config = #{
    openai_api_key => <<"your_api_key">>,
    default_model => <<"gpt-4.1-nano">>,
    log_level => info
},
agent:start(Config).
```

### Running an Agent with Tools

```erlang
% Basic usage
Prompt = <<"What is the current time?">>,
ToolNames = [shell, file_read],
Response = agent:run_agent(Prompt, ToolNames).

% Advanced options
Options = #{
    model => <<"gpt-4.1-nano">>,
    system_message => <<"You are a helpful assistant with access to system tools.">>,
    timeout => 120000,  % 2 minutes
    temperature => 0.2, % Lower temperature for more deterministic responses
    max_tokens => 1500,
    top_p => 0.95,
    frequency_penalty => 0.0,
    presence_penalty => 0.0,
    tools_config => #{
        shell => #{allow_sudo => false},
        file_read => #{allowed_paths => [<<"/tmp">>]}
    }
},
Response = agent:run_agent(Prompt, ToolNames, Options).
```

### Handling Agent Responses Asynchronously

```erlang
% Run agent asynchronously with callback
CallbackFn = fun(Result) ->
    case Result of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [Response]);
        {error, Reason} ->
            io:format("Agent error: ~p~n", [Reason])
    end
end,

% Start the agent with async option
agent:run_agent(Prompt, ToolNames, #{
    async => true,
    callback => CallbackFn
}).
```

### Defining Custom Tools

```erlang
% Define a custom tool
ToolName = my_custom_tool,
ToolSchema = #{
    <<"name">> => <<"my_custom_tool">>,
    <<"description">> => <<"A custom tool that does something useful">>,
    <<"parameters">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Input for the tool">>
            },
            <<"options">> => #{
                <<"type">> => <<"object">>,
                <<"description">> => <<"Additional options for the tool">>,
                <<"properties">> => #{
                    <<"format">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"json">>, <<"text">>, <<"binary">>],
                        <<"description">> => <<"Output format">>
                    }
                }
            }
        },
        <<"required">> => [<<"input">>]
    }
},
agent:define_tool(ToolName, ToolSchema).

% Register a function to execute the tool with error handling and timeout
ExecutorFn = fun(_Name, Arguments) ->
    try
        Input = maps:get(<<"input">>, Arguments, <<"">>),
        Options = maps:get(<<"options">>, Arguments, #{}),
        Format = maps:get(<<"format">>, Options, <<"text">>),
        
        % Process the input based on format
        Result = case Format of
            <<"json">> ->
                process_as_json(Input);
            <<"binary">> ->
                process_as_binary(Input);
            _ -> % Default to text
                process_as_text(Input)
        end,
        
        % Return the result
        Result
    catch
        error:Reason:Stacktrace ->
            {error, #{
                reason => Reason,
                stacktrace => Stacktrace,
                message => <<"Tool execution failed">>
            }}
    after
        % Cleanup resources if needed
        cleanup_resources()
    end
end,

% Register with options
ExecutorOptions = #{
    timeout => 5000,     % 5 second timeout
    retry => #{
        max_retries => 3,
        delay => 1000     % 1 second between retries
    },
    concurrency => 5      % Max concurrent executions
},
agent:execute_tool(ToolName, ExecutorFn, ExecutorOptions).
```

### Predefined Tools

The framework comes with several predefined tools:

1. **shell** - Execute shell commands
   ```erlang
   agent:run_agent(<<"List files in current directory">>, [shell]).
   
   % With security constraints
   agent:run_agent(<<"Run system diagnostics">>, [shell], #{
       tools_config => #{
           shell => #{
               allowed_commands => [<<"ls">>, <<"ps">>, <<"df">>],
               timeout => 2000
           }
       }
   }).
   ```

2. **file_read** - Read file contents
   ```erlang
   agent:run_agent(<<"Show me the content of hello.txt">>, [file_read]).
   
   % With path constraints
   agent:run_agent(<<"Analyze log files">>, [file_read], #{
       tools_config => #{
           file_read => #{
               allowed_paths => [<<"/var/log">>, <<"/tmp">>],
               max_size => 1048576  % 1MB max file size
           }
       }
   }).
   ```

3. **file_write** - Write content to a file
   ```erlang
   agent:run_agent(<<"Create a file named test.txt with 'Hello World' content">>, [file_write]).
   ```

4. **http_request** - Make HTTP requests
   ```erlang
   agent:run_agent(<<"Fetch the latest weather from api.weather.com">>, [http_request]).
   
   % With allowed domains
   agent:run_agent(<<"Get current GitHub status">>, [http_request], #{
       tools_config => #{
           http_request => #{
               allowed_domains => [<<"api.github.com">>, <<"status.github.com">>],
               max_response_size => 1048576,  % 1MB max response size
               timeout => 10000               % 10 second timeout
           }
       }
   }).
   ```

### Direct API Access

You can also access the OpenAI API directly:

```erlang
% Ensure chat API client is available
agent:ensure_api_client(chat).

% Create a chat completion
Model = <<"gpt-4.1-nano">>,
Messages = [
    #{role => <<"system">>, content => <<"You are a helpful assistant.">>},
    #{role => <<"user">>, content => <<"What is the capital of France?">>}
],
Options = #{
    temperature => 0.3,
    max_tokens => 500
},
{ok, Response} = openai_chat:create_chat_completion(Model, Messages, Options).

% Extract just the response text
{ok, ResponseText} = openai_chat:extract_content(Response).
```

### Working with Embeddings

```erlang
% Ensure embeddings API client is available
agent:ensure_api_client(embeddings).

% Create embeddings for a single text
Text = <<"This is a sample text to embed">>,
Model = <<"text-embedding-3-small">>,
{ok, Response} = openai_embeddings:create_embedding(Model, Text, #{}).

% Create embeddings for multiple texts
Texts = [
    <<"First document to embed">>,
    <<"Second document to embed">>,
    <<"Third document with different content">>
],
{ok, BatchResponse} = openai_embeddings:create_embeddings(Model, Texts, #{}).

% Extract the embedding vectors
{ok, Vectors} = openai_embeddings:extract_vectors(BatchResponse).

% Calculate cosine similarity between embeddings
FirstVector = lists:nth(1, Vectors),
SecondVector = lists:nth(2, Vectors),
Similarity = openai_embeddings:cosine_similarity(FirstVector, SecondVector).
```

### Completions API

```erlang
% Ensure completions API client is available
agent:ensure_api_client(completions).

% Create a completion
Prompt = <<"Once upon a time">>,
Model = <<"gpt-3.5-turbo-instruct">>,
Options = #{
    max_tokens => 100,
    temperature => 0.7,
    stop => [<<".">>, <<"!">>]  % Stop at first period or exclamation mark
},
{ok, Response} = openai_completions:create_completion(Model, Prompt, Options).
```

### Agent Discovery and Communication

For distributed systems:

```erlang
% Connect nodes
net_kernel:connect_node('agent2@host2.example.com').

% Register this agent in the discovery service
agent_discovery:register(#{
    name => <<"pricing_agent">>,
    capabilities => [pricing, currency_conversion],
    status => available
}).

% Find agents with specific capabilities
{ok, Agents} = agent_discovery:find_by_capability(currency_conversion).

% Send a message to another agent
AgentPid = proplists:get_value(<<"inventory_agent">>, Agents),
agent_messenger:send_message(AgentPid, #{
    type => request,
    action => check_stock,
    parameters => #{
        product_id => <<"ABC123">>,
        warehouse => <<"MAIN">>
    }
}).

% Register a message handler
agent_messenger:register_handler(fun(Message) ->
    case maps:get(type, Message) of
        request ->
            handle_request(Message);
        response ->
            handle_response(Message);
        _ ->
            {error, unknown_message_type}
    end
end).
```

## Environment Variables

- `OPENAI_API_KEY`: Your OpenAI API key
- `OPENAI_ORGANIZATION`: Your OpenAI organization ID (optional)
- `OPENAI_BASE_URL`: Base URL for OpenAI API (defaults to https://api.openai.com/v1)
- `OPENAI_TIMEOUT`: Default timeout for API requests in milliseconds (defaults to 30000)
- `OPENAI_MAX_RETRIES`: Maximum number of retries for failed requests (defaults to 3)
- `OPENAI_RETRY_DELAY`: Delay between retries in milliseconds (defaults to 1000)
- `OPENAI_DEFAULT_MODEL`: Default model to use (defaults to gpt-4.1-nano)
- `AGENT_LOG_LEVEL`: Log level (debug, info, warning, error) - defaults to info
- `AGENT_METRICS_ENABLED`: Enable prometheus metrics (true/false) - defaults to false
- `AGENT_POOL_SIZE`: Size of the agent process pool (defaults to 10)
- `AGENT_DEFAULT_TIMEOUT`: Default timeout for agent operations (defaults to 60000)

## Building and Running

```bash
# Clone the repository
git clone https://github.com/yourusername/erlang-agent.git
cd erlang-agent

# Build the project
make compile

# Run the tests
make test

# Run dialyzer type checking
make dialyzer

# Generate documentation
make docs

# Start an interactive Erlang shell with the agent loaded
make shell

# OR use rebar3 directly
./rebar3 shell
```

## Advanced Topics

### Rate Limiting

The `openai_rate_limiter` module provides intelligent rate limiting to prevent hitting API quotas:

```erlang
% Configure rate limits for a specific API endpoint
openai_rate_limiter:set_rate_limit(chat, 60, 60000). % 60 requests per minute

% Configure with burst capacity
openai_rate_limiter:set_rate_limit(chat, 60, 60000, #{
    burst => 20,              % Allow bursts up to 20 additional requests
    token_refresh => smooth   % Refill tokens smoothly over time instead of all at once
}).

% Set different limits for different models
openai_rate_limiter:set_model_limit(<<"gpt-4.1-nano">>, 100, 60000). % 100 requests per minute
openai_rate_limiter:set_model_limit(<<"gpt-4-vision">>, 20, 60000).  % 20 requests per minute

% Get current rate limit status
{ok, Status} = openai_rate_limiter:get_status(chat).
```

### Custom API Endpoints

You can extend the system with custom API endpoints:

```erlang
% Define a custom API group
ApiGroup = my_custom_api,

% Define the API structure
ApiStructure = #{
    <<"paths">> => #{
        <<"/custom/endpoint">> => #{
            <<"post">> => #{
                <<"operationId">> => <<"customOperation">>,
                <<"parameters">> => []
            }
        }
    }
},

% Register the API structure
openai_api_structure:register_api_group(ApiGroup, ApiStructure),

% Start a client for this API group
openai_clients_sup:start_client(ApiGroup, #{
    base_url => <<"https://custom-api.example.com/v1">>,
    auth_token => <<"your-custom-token">>,
    headers => #{
        <<"X-Custom-Header">> => <<"value">>
    }
}).

% Use the custom client
my_custom_api:custom_operation(#{
    parameter1 => value1,
    parameter2 => value2
}).
```

### Handling Streaming Responses

For chat completions with streaming:

```erlang
% Define a streaming handler function
StreamHandler = fun(Chunk) ->
    case maps:get(<<"choices">>, Chunk, []) of
        [] -> 
            ok;
        [Choice | _] ->
            case maps:get(<<"delta">>, Choice, #{}) of
                #{<<"content">> := Content} when Content =/= null ->
                    io:format("~s", [Content]);
                _ ->
                    ok
            end
    end
end,

% Create a streaming chat completion
Model = <<"gpt-4.1-nano">>,
Messages = [
    #{role => <<"user">>, content => <<"Tell me a long story">>}
],
Options = #{
    stream => true,
    stream_handler => StreamHandler,
    timeout => 300000  % 5 minutes for long responses
},
openai_chat:create_chat_completion(Model, Messages, Options).
```

### Function Calling

Using OpenAI's function calling capabilities:

```erlang
% Define available functions
Functions = [
    #{
        <<"name">> => <<"get_weather">>,
        <<"description">> => <<"Get the current weather in a location">>,
        <<"parameters">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"location">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"The city and state, e.g. San Francisco, CA">>
                },
                <<"unit">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"celsius">>, <<"fahrenheit">>],
                    <<"description">> => <<"The temperature unit to use">>
                }
            },
            <<"required">> => [<<"location">>]
        }
    }
],

% Register function implementations
FunctionHandlers = #{
    <<"get_weather">> => fun(Args) ->
        Location = maps:get(<<"location">>, Args),
        Unit = maps:get(<<"unit">>, Args, <<"celsius">>),
        % Call your weather API here
        get_weather_data(Location, Unit)
    end
},

% Run the agent with function calling
agent:run_agent(
    <<"What's the weather like in Tokyo?">>,
    [],  % No tools needed
    #{
        functions => Functions,
        function_handlers => FunctionHandlers,
        function_call => <<"auto">>  % Let the model decide when to call functions
    }
).
```

### Distributed Operation

Running in a distributed Erlang cluster:

```erlang
% Start distributed Erlang
net_kernel:start(['agent1@host1.example.com', longnames]).

% Define the cluster nodes
Nodes = ['agent2@host2.example.com', 'agent3@host3.example.com'],

% Start agent distributed
agent:start_distributed(Nodes, #{
    strategy => shard_by_endpoint,  % Distribute API calls across nodes
    fallback => true                % Fall back to local node if remote node is down
}).

% Run an agent that can use tools from any node in the cluster
agent:run_agent(<<"Analyze the logs on all servers">>, 
    [shell, file_read], 
    #{distributed => true}
).
```

### Observability and Metrics

Monitor your agent ecosystem:

```erlang
% Enable metrics
application:set_env(agent, metrics_enabled, true).

% Get API usage statistics
Stats = openai_rate_limiter:get_usage_stats().

% Get active agent count
{ok, Count} = agent_registry:count_active().

% Get detailed metrics in Prometheus format
{ok, Metrics} = agent:get_metrics_prometheus().

% Log all API calls (for debugging)
application:set_env(agent, log_api_calls, true).

% Configure structured logging
application:set_env(agent, log_format, json).
```

### Implementing Custom Agent Behaviors

Create specialized agents with custom behaviors:

```erlang
% Define a custom agent behavior
-module(my_specialized_agent).
-behavior(agent_behavior).

% Implement the behavior callbacks
init(Options) ->
    % Initialize agent state
    {ok, #{}}.
    
handle_prompt(Prompt, Tools, Options, State) ->
    % Custom prompt handling logic
    {Response, NewState} = process_prompt(Prompt, Tools, Options, State),
    {ok, Response, NewState}.
    
handle_tool_call(ToolName, Arguments, State) ->
    % Custom tool handling logic
    {Result, NewState} = execute_custom_tool(ToolName, Arguments, State),
    {ok, Result, NewState}.

terminate(Reason, State) ->
    % Clean up resources
    ok.

% Register your custom agent behavior
agent:register_behavior(my_specialized_agent, #{
    description => <<"A specialized agent for specific tasks">>,
    default_tools => [shell, file_read, custom_tool]
}).

% Use your custom agent behavior
agent:run_with_behavior(my_specialized_agent, Prompt, Tools, Options).
```

## Troubleshooting

If you encounter any of these issues:

1. **Already Started Error**: The "already_started" error with the OpenAI application has been fixed in the latest version. The application now properly handles startup order to prevent conflicts.

2. **Missing API Modules**: If you encounter errors about missing modules like `openai_chat`, `openai_completions`, or `openai_embeddings`, make sure you have compiled the project properly using:
   ```bash
   ./rebar3 compile
   ```

3. **Agent Registry Errors**: If you see errors related to `agent_registry`, ensure that all application dependencies are set up correctly in their respective `.app.src` files.

4. **Model-related Errors**: Make sure to use a supported model like "gpt-4.1-nano" or "gpt-3.5-turbo".

5. **Startup Issues**: To verify that the application is starting correctly, use:
   ```erlang
   application:ensure_all_started(agent).
   application:which_applications().
   % You should see both 'agent' and 'openai' in the list
   ```

6. **Network Issues**: If you're having trouble connecting to the OpenAI API, check your network connection and proxy settings.

7. **Rate Limit Errors**: If you're hitting rate limits, consider adjusting the rate limit settings through the `openai_rate_limiter` module.

8. **Memory Issues**: If you're processing large responses or running many agents simultaneously, you might encounter memory issues. Monitor your Erlang VM memory usage:
   ```erlang
   % Check memory usage
   erlang:memory().
   
   % Trigger garbage collection if needed
   erlang:garbage_collect().
   ```

9. **Tool Execution Timeouts**: If tool executions are timing out, you can adjust the timeout settings:
   ```erlang
   % Set a longer timeout for a specific tool
   agent:run_agent(Prompt, [shell], #{
       tools_config => #{
           shell => #{timeout => 60000}  % 60 seconds
       }
   }).
   ```

10. **API Version Compatibility**: The OpenAI API evolves over time. If you encounter unexpected behavior, ensure you're using the latest version of the agent framework compatible with the current OpenAI API. You can specify API versions:
    ```erlang
    application:set_env(openai, api_version, <<"2023-05-15">>).
    ```

## Performance Optimization

For high-throughput applications:

1. **Connection Pooling**: Configure the HTTP client connection pool size:
   ```erlang
   application:set_env(openai, http_pool_size, 50).
   ```

2. **Caching**: Enable response caching for repetitive requests:
   ```erlang
   application:set_env(agent, enable_cache, true).
   application:set_env(agent, cache_ttl, 3600).  % Cache time-to-live in seconds
   ```

3. **Batch Processing**: Use batch operations for embeddings:
   ```erlang
   % Process texts in batches of 20 for efficiency
   openai_embeddings:create_embeddings_batched(Model, LargeListOfTexts, 20, Options).
   ```

4. **Distributed Load**: Spread load across a cluster as described in the "Distributed Operation" section.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines

- Follow the [Erlang Style Guide](https://github.com/inaka/erlang_guidelines)
- Include documentation with exported functions
- Add tests for new functionality
- Run the test suite before submitting PRs: `make test`
- Run dialyzer to check for type errors: `make dialyzer`

## License

MIT