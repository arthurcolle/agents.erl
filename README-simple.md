# Simple Erlang Agent

A simplified interface to the Erlang Agent for OpenAI API.

## Getting Started

### Prerequisites

- Erlang/OTP 22 or newer
- rebar3 build tool
- OpenAI API Key

### Environment Setup

Make sure to set your OpenAI API key as an environment variable:

```bash
export OPENAI_API_KEY="your-api-key-here"
```

### Building

```bash
# Clean and compile the project
./rebar3 clean
./rebar3 compile
```

## Usage

### Command Line Interface

The project provides a simple CLI for interacting with the AI:

```bash
# Display help
./cli

# Run the hello world example
./cli hello

# Run hello world with a custom message
./cli hello "How does Erlang handle concurrency?"

# Simple chat with AI
./cli chat "What is the capital of France?"

# Run the full agent with tools
./cli run --prompt "What is the current time?" --tools "shell"
```

### From Erlang Code

You can also use the agent programmatically from your Erlang code:

```erlang
% Start the application
simple_agent:start().

% Send a simple chat message
Response = simple_agent:chat(<<"What is the capital of France?">>).

% Send a chat with options
Options = #{
    model => <<"gpt-4">>,
    system_message => <<"You are a helpful assistant who loves Erlang.">>
},
Response = simple_agent:chat(<<"How do I implement a gen_server?">>, Options).

% Stop the application when done
simple_agent:stop().
```

## Advanced Usage

For more advanced usage, refer to the main README.md which describes the full API and capabilities of the Erlang Agent framework.

## Troubleshooting

### Configuration Issues

If you encounter errors related to missing API key:

1. Make sure `OPENAI_API_KEY` environment variable is set
2. Restart your shell session to ensure environment variables are loaded

### Connection Issues

If you encounter connection errors:

1. Check your internet connection
2. Verify your API key is valid
3. Ensure the OpenAI API is accessible from your network

## License

MIT