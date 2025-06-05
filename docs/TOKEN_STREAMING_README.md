# Token Streaming System for Erlang

## 🌊 Overview

The Token Streaming System provides robust, real-time token processing capabilities for AI applications using Erlang/OTP. Built on `gen_statem` for reliable state management, it offers fine-grained control over token streams with features like buffering, rate limiting, and event-driven processing.

## 🚀 Key Features

### Core Components

1. **`token_stream_fsm.erl`** - Main state machine using `gen_statem`
   - Idle, Buffering, Streaming, Rate-limited, and Error states
   - Configurable buffer sizes and batch processing
   - Rate limiting and error handling
   - Real-time statistics and monitoring

2. **`token_stream_handler.erl`** - High-level handler using `gen_server`
   - Multiple stream management
   - Subscription-based event system
   - Process monitoring and cleanup
   - Stream lifecycle management

3. **`token_stream_integration.erl`** - OpenAI integration layer
   - Seamless integration with existing OpenAI streaming
   - Token extraction from streaming events
   - Enhanced event handlers
   - Chat and Response API support

## 📋 State Machine Architecture

The token stream FSM follows this state flow:

```
idle -> buffering -> streaming -> rate_limited -> streaming
  |         |           |            |
  v         v           v            v
error_state <- <- <- <- <- <- <- <- <-
```

### States

- **`idle`** - Waiting for tokens
- **`buffering`** - Collecting tokens up to batch size
- **`streaming`** - Actively processing and flushing tokens
- **`rate_limited`** - Temporarily paused due to rate limiting
- **`error_state`** - Handling errors with recovery options

## 🔧 Configuration Options

### Token Stream FSM Options

```erlang
Options = #{
    max_buffer_size => 1000,    % Maximum tokens to buffer
    batch_size => 10,           % Tokens per batch
    flush_interval => 100,      % Milliseconds between flushes
    rate_limit => 100,          % Tokens per second
    max_errors => 5             % Maximum errors before stopping
}.
```

### Integration Options

```erlang
StreamOptions = #{
    stream => true,             % Enable streaming
    token_buffer_size => 50,    % Buffer size for tokens
    token_batch_size => 5,      % Batch size for processing
    token_flush_interval => 100 % Flush interval in ms
}.
```

## 🛠️ Usage Examples

### Basic Token Stream

```erlang
% Start a simple token handler
HandlerPid = spawn(fun() -> token_handler_loop() end),

% Create token stream FSM
Options = #{max_buffer_size => 10, batch_size => 3},
{ok, StreamPid} = token_stream_fsm:start_link(HandlerPid, Options),

% Process tokens
token_stream_fsm:process_token(StreamPid, <<"Hello">>),
token_stream_fsm:process_tokens(StreamPid, [<<"world">>, <<"!">>]),

% Get statistics
{ok, Stats} = token_stream_fsm:get_statistics(StreamPid),

% Stop the stream
token_stream_fsm:stop(StreamPid).
```

### Token Stream Handler

```erlang
% Start token stream handler
{ok, HandlerPid} = token_stream_handler:start_link(my_handler, #{}),

% Create a stream
StreamId = <<"my_stream">>,
StreamOptions = #{batch_size => 5, flush_interval => 200},
{ok, StreamPid} = token_stream_handler:create_token_stream(
    HandlerPid, StreamId, StreamOptions),

% Subscribe to events
token_stream_handler:subscribe(HandlerPid, StreamId),

% Process tokens
token_stream_fsm:process_tokens(StreamPid, [<<"token1">>, <<"token2">>]),

% Get stream info
{ok, Info} = token_stream_handler:get_stream_info(HandlerPid, StreamId).
```

### OpenAI Integration

```erlang
% Create chat token stream
Model = <<"gpt-4.1-mini">>,
Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>}],
Options = #{
    stream => true,
    token_buffer_size => 10,
    token_batch_size => 3
},

{ok, StreamInfo} = token_stream_integration:create_chat_token_stream(
    Model, Messages, Options),

% Subscribe to token events
HandlerPid = maps:get(handler_pid, StreamInfo),
StreamId = maps:get(stream_id, StreamInfo),
token_stream_handler:subscribe(HandlerPid, StreamId).
```

### Response API Integration

```erlang
% Create response token stream
Input = [#{<<"role">> => <<"user">>, <<"content">> => <<"Explain AI">>}],
Model = <<"gpt-4">>,
Options = #{stream => true, token_batch_size => 5},

{ok, StreamInfo} = token_stream_integration:create_response_token_stream(
    Input, Model, Options).
```

## 📊 Event System

### Token Events

The system generates `token_stream_event` messages:

```erlang
#token_event{
    stream_id = <<"stream_123">>,
    tokens = [<<"Hello">>, <<"world">>],
    timestamp = 1634567890123,
    metadata = #{event_type => content}
}
```

### Subscription Model

```erlang
% Subscribe to stream events
token_stream_handler:subscribe(HandlerPid, StreamId),

% Receive events
receive
    {token_stream_event, Event} ->
        Tokens = Event#token_event.tokens,
        process_tokens(Tokens)
end.
```

## 🧪 Testing and Examples

### Run Comprehensive Tests

```bash
./test_token_streaming.erl
```

This runs five demo scenarios:
1. Basic Token Stream FSM
2. Token Stream Handler
3. OpenAI Integration
4. Advanced Token Processing
5. Real-world Scenario

### Simple Examples

```bash
# Start Erlang shell
./rebar3 shell

# Run examples
token_streaming_example:simple_token_stream().
token_streaming_example:chat_with_token_streaming().
token_streaming_example:document_processor("README.md").
```

## 🔍 Monitoring and Statistics

### Stream Statistics

```erlang
{ok, Stats} = token_stream_fsm:get_statistics(StreamPid),
% Returns:
#{
    tokens_processed => 150,
    batches_flushed => 15,
    state_changes => 8,
    errors => 0,
    start_time => 1634567890000,
    last_activity => 1634567895000
}
```

### Handler Statistics

```erlang
{ok, Streams} = token_stream_handler:get_active_streams(HandlerPid),
% Returns list of active streams with info

{ok, Info} = token_stream_handler:get_stream_info(HandlerPid, StreamId),
% Returns detailed stream information
```

## ⚡ Performance Features

### Rate Limiting

Automatic rate limiting prevents overwhelming downstream systems:

```erlang
Options = #{
    rate_limit => 50,  % 50 tokens per second
    max_buffer_size => 100
}
```

### Buffering Strategies

Multiple buffering strategies for different use cases:

- **Batch-based**: Process tokens in fixed-size batches
- **Time-based**: Flush buffer at regular intervals
- **Hybrid**: Combine batch and time-based flushing

### Error Recovery

Robust error handling with automatic recovery:

```erlang
Options = #{
    max_errors => 5,  % Maximum errors before stopping
    error_recovery => true
}
```

## 🔧 Configuration Best Practices

### Chat Applications

```erlang
ChatOptions = #{
    max_buffer_size => 50,
    batch_size => 5,
    flush_interval => 100,
    rate_limit => 20
}
```

### Document Processing

```erlang
DocumentOptions = #{
    max_buffer_size => 200,
    batch_size => 20,
    flush_interval => 300,
    rate_limit => 100
}
```

### Real-time Applications

```erlang
RealTimeOptions = #{
    max_buffer_size => 10,
    batch_size => 2,
    flush_interval => 50,
    rate_limit => 30
}
```

## 🔗 Integration with Existing Systems

### With Existing OpenAI Streaming

```erlang
% Wrap existing handler
ExistingHandler = fun(Event) -> process_event(Event) end,
EnhancedHandler = token_stream_integration:wrap_existing_handler(
    ExistingHandler, TokenStreamPid).
```

### With Custom Event Systems

```erlang
% Create custom token handler
CustomHandler = fun(Event, Context) ->
    Tokens = extract_custom_tokens(Event),
    {tokens, Tokens, #{custom_metadata => Context}}
end.
```

## 🚨 Error Handling

### Common Error Scenarios

1. **Handler Process Down**: Automatic cleanup and notification
2. **Rate Limit Exceeded**: Temporary pause with automatic resume
3. **Buffer Overflow**: Configurable handling (drop/block/error)
4. **Network Issues**: Retry logic with exponential backoff

### Error Recovery

```erlang
% Set options for error recovery
Options = #{
    max_errors => 5,
    error_recovery_delay => 1000,  % ms
    auto_restart => true
}
```

## 📈 Scaling Considerations

### Multiple Streams

The system supports multiple concurrent streams:

```erlang
% Create multiple streams with different characteristics
FastStream = {<<"fast">>, #{batch_size => 2, flush_interval => 50}},
SlowStream = {<<"slow">>, #{batch_size => 20, flush_interval => 500}}.
```

### Resource Management

- Monitor memory usage with large buffers
- Adjust rate limits based on downstream capacity
- Use appropriate batch sizes for your use case

## 🔮 Advanced Features

### Custom Token Parsing

```erlang
% Implement custom token parser
parse_custom_tokens(Content) ->
    % Your custom tokenization logic
    custom_tokenize(Content).
```

### Metadata Tracking

```erlang
% Add metadata to token events
Metadata = #{
    source => <<"chat">>,
    user_id => <<"user123">>,
    session_id => <<"session456">>
}.
```

### Stream Chaining

```erlang
% Chain multiple streams for complex processing
Stream1 -> TokenProcessor -> Stream2 -> OutputHandler
```

## 🔧 Development and Debugging

### Enable Debug Logging

```erlang
% Start with debug options
Options = #{debug => true, log_level => debug}.
```

### Format Status for Debugging

The system provides `format_status/2` callbacks for debugging:

```erlang
sys:get_status(StreamPid).
```

## 🚀 Future Enhancements

- **Persistent Streams**: Save/restore stream state
- **Distributed Streams**: Multi-node token processing
- **Advanced Analytics**: Token analysis and insights
- **Machine Learning**: Intelligent batch sizing
- **WebSocket Integration**: Direct browser streaming

## 📝 License

This token streaming system is part of the agents.erl project and follows the same licensing terms.

## 🤝 Contributing

Contributions are welcome! Please ensure:

1. All new features include comprehensive tests
2. Documentation is updated for new functionality
3. Performance impact is considered and documented
4. Error handling is robust and well-tested

## 🔗 Related Documentation

- [STREAMING_IMPLEMENTATION_SUMMARY.md](STREAMING_IMPLEMENTATION_SUMMARY.md) - Overview of streaming features
- [examples/README.md](examples/README.md) - Example implementations
- [Erlang gen_statem Documentation](http://erlang.org/doc/design_principles/statem.html) - Official gen_statem guide