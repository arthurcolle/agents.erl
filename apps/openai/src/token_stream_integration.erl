%% token_stream_integration.erl
%% Integration layer between OpenAI streaming and token stream system
%% Provides seamless integration with existing streaming infrastructure

-module(token_stream_integration).

%% Public API
-export([
    start_token_streaming/3,
    start_token_streaming/4,
    create_chat_token_stream/3,
    create_response_token_stream/3,
    token_event_handler/2,
    enhanced_event_handler/2,
    parse_stream_tokens/1,
    extract_tokens_from_event/1
]).

%% Integration exports
-export([
    integrate_with_chat_streaming/2,
    integrate_with_response_streaming/2,
    wrap_existing_handler/2
]).

%% Logging macros
-define(LOG_INFO(Msg), log_safe(info, Msg)).
-define(LOG_INFO(Msg, Args), log_safe(info, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), log_safe(error, Msg)).
-define(LOG_ERROR(Msg, Args), log_safe(error, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), log_safe(debug, Msg)).
-define(LOG_DEBUG(Msg, Args), log_safe(debug, io_lib:format(Msg, Args))).

%% Public API

%% Start token streaming with automatic handler creation
start_token_streaming(Model, Messages, Options) ->
    start_token_streaming(Model, Messages, Options, default_token_handler()).

start_token_streaming(Model, Messages, Options, TokenHandler) ->
    ?LOG_INFO("[TOKEN_INTEGRATION] 🚀 Starting integrated token streaming"),
    ?LOG_INFO("[TOKEN_INTEGRATION] Model: ~p, Messages: ~p", [Model, length(Messages)]),
    
    % Generate unique stream ID
    StreamId = generate_stream_id(Model, Messages),
    
    % Start token stream handler if not already running
    {ok, HandlerPid} = ensure_token_handler_running(),
    
    % Create token stream
    TokenStreamOptions = extract_token_options(Options),
    case token_stream_handler:create_token_stream(HandlerPid, StreamId, TokenStreamOptions) of
        {ok, StreamPid} ->
            ?LOG_INFO("[TOKEN_INTEGRATION] ✅ Token stream created: ~p", [StreamId]),
            
            % Create enhanced event handler that feeds tokens to our stream
            EnhancedHandler = create_enhanced_handler(StreamPid, TokenHandler),
            
            % Start OpenAI streaming with enhanced handler
            case openai_streaming:create_streaming_chat(Model, Messages, Options) of
                {ok, OpenAIStreamPid} ->
                    ?LOG_INFO("[TOKEN_INTEGRATION] 🌊 OpenAI streaming started"),
                    
                    % Handle stream events
                    spawn(fun() ->
                        Result = openai_streaming:handle_stream_events(EnhancedHandler, 30000),
                        ?LOG_INFO("[TOKEN_INTEGRATION] 🏁 Streaming completed: ~p", [Result])
                    end),
                    
                    {ok, #{
                        stream_id => StreamId,
                        token_stream_pid => StreamPid,
                        openai_stream_pid => OpenAIStreamPid,
                        handler_pid => HandlerPid
                    }};
                {error, Reason} ->
                    ?LOG_ERROR("[TOKEN_INTEGRATION] ❌ OpenAI streaming failed: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("[TOKEN_INTEGRATION] ❌ Token stream creation failed: ~p", [Reason]),
            {error, Reason}
    end.

%% Create chat token stream
create_chat_token_stream(Model, Messages, Options) ->
    ?LOG_INFO("[TOKEN_INTEGRATION] 🗨️  Creating chat token stream"),
    
    % Use chat-specific settings
    ChatOptions = Options#{
        stream => true,
        max_buffer_size => maps:get(token_buffer_size, Options, 50),
        batch_size => maps:get(token_batch_size, Options, 5),
        flush_interval => maps:get(token_flush_interval, Options, 50)
    },
    
    start_token_streaming(Model, Messages, ChatOptions, chat_token_handler()).

%% Create response token stream
create_response_token_stream(Input, Model, Options) ->
    ?LOG_INFO("[TOKEN_INTEGRATION] 📝 Creating response token stream"),
    
    % Use response-specific settings
    ResponseOptions = Options#{
        stream => true,
        max_buffer_size => maps:get(token_buffer_size, Options, 100),
        batch_size => maps:get(token_batch_size, Options, 10),
        flush_interval => maps:get(token_flush_interval, Options, 100)
    },
    
    % Generate unique stream ID
    StreamId = generate_stream_id(Model, Input),
    
    % Start token stream handler
    {ok, HandlerPid} = ensure_token_handler_running(),
    
    % Create token stream
    TokenStreamOptions = extract_token_options(ResponseOptions),
    case token_stream_handler:create_token_stream(HandlerPid, StreamId, TokenStreamOptions) of
        {ok, StreamPid} ->
            % Create enhanced handler for responses
            EnhancedHandler = create_response_enhanced_handler(StreamPid, response_token_handler()),
            
            % Start OpenAI response streaming
            case openai_streaming:create_streaming_response(Input, Model, ResponseOptions) of
                {ok, OpenAIStreamPid} ->
                    ?LOG_INFO("[TOKEN_INTEGRATION] 🌊 Response streaming started"),
                    
                    % Handle stream events
                    spawn(fun() ->
                        Result = openai_streaming:handle_stream_events(EnhancedHandler, 60000),
                        ?LOG_INFO("[TOKEN_INTEGRATION] 🏁 Response streaming completed: ~p", [Result])
                    end),
                    
                    {ok, #{
                        stream_id => StreamId,
                        token_stream_pid => StreamPid,
                        openai_stream_pid => OpenAIStreamPid,
                        handler_pid => HandlerPid
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Default token event handler
token_event_handler(Event, _Context) ->
    case maps:get(type, Event, undefined) of
        content ->
            Content = maps:get(data, Event, <<>>),
            Tokens = parse_content_to_tokens(Content),
            {tokens, Tokens};
        text_delta ->
            Content = maps:get(data, Event, <<>>),
            Tokens = parse_content_to_tokens(Content),
            {tokens, Tokens};
        <<"text_delta">> ->
            Content = maps:get(content, Event, <<>>),
            Tokens = parse_content_to_tokens(Content),
            {tokens, Tokens};
        _ ->
            continue
    end.

%% Enhanced token event handler with metadata
enhanced_event_handler(Event, Context) ->
    BaseResult = token_event_handler(Event, Context),
    
    case BaseResult of
        {tokens, Tokens} ->
            % Add metadata and context
            Metadata = #{
                event_type => maps:get(type, Event, unknown),
                timestamp => erlang:system_time(millisecond),
                context => Context,
                original_event => Event
            },
            {tokens, Tokens, Metadata};
        Other ->
            Other
    end.

%% Parse stream content into tokens
parse_stream_tokens(Content) when is_binary(Content) ->
    % Simple word-based tokenization (can be enhanced with proper tokenizer)
    Words = binary:split(Content, [<<" ">>, <<"\n">>, <<"\t">>], [global]),
    lists:filter(fun(Token) -> byte_size(Token) > 0 end, Words);
parse_stream_tokens(Content) when is_list(Content) ->
    parse_stream_tokens(list_to_binary(Content));
parse_stream_tokens(_) ->
    [].

%% Extract tokens from streaming event
extract_tokens_from_event(#{type := content, data := Data}) ->
    parse_stream_tokens(Data);
extract_tokens_from_event(#{type := text_delta, data := Data}) ->
    parse_stream_tokens(Data);
extract_tokens_from_event(#{<<"type">> := <<"response.output_text.delta">>, <<"delta">> := Delta}) ->
    parse_stream_tokens(Delta);
extract_tokens_from_event(#{content := Content}) ->
    parse_stream_tokens(Content);
extract_tokens_from_event(_) ->
    [].

%% Integration functions

%% Integrate with existing chat streaming
integrate_with_chat_streaming(Options, TokenStreamPid) ->
    OriginalHandler = maps:get(event_handler, Options, fun(_) -> continue end),
    
    EnhancedHandler = fun(Event) ->
        % Extract tokens and send to token stream
        Tokens = extract_tokens_from_event(Event),
        case Tokens of
            [] -> ok;
            _ -> token_stream_fsm:process_tokens(TokenStreamPid, Tokens)
        end,
        
        % Call original handler
        OriginalHandler(Event)
    end,
    
    Options#{event_handler => EnhancedHandler}.

%% Integrate with existing response streaming
integrate_with_response_streaming(Options, TokenStreamPid) ->
    OriginalHandler = maps:get(event_handler, Options, fun(_) -> continue end),
    
    EnhancedHandler = fun(Event) ->
        % Extract tokens and send to token stream
        Tokens = extract_tokens_from_event(Event),
        case Tokens of
            [] -> ok;
            _ -> token_stream_fsm:process_tokens(TokenStreamPid, Tokens)
        end,
        
        % Call original handler
        OriginalHandler(Event)
    end,
    
    Options#{event_handler => EnhancedHandler}.

%% Wrap existing event handler with token processing
wrap_existing_handler(ExistingHandler, TokenStreamPid) ->
    fun(Event) ->
        % Process tokens first
        Tokens = extract_tokens_from_event(Event),
        case Tokens of
            [] -> ok;
            _ -> token_stream_fsm:process_tokens(TokenStreamPid, Tokens)
        end,
        
        % Call existing handler
        ExistingHandler(Event)
    end.

%% Internal helper functions

%% Generate unique stream ID
generate_stream_id(Model, Input) ->
    Timestamp = erlang:system_time(millisecond),
    Hash = erlang:phash2({Model, Input, Timestamp}),
    iolist_to_binary(io_lib:format("stream_~p_~p", [Hash, Timestamp])).

%% Ensure token handler is running
ensure_token_handler_running() ->
    case whereis(token_stream_handler) of
        undefined ->
            ?LOG_INFO("[TOKEN_INTEGRATION] 🚀 Starting token stream handler"),
            token_stream_handler:start_link(token_stream_handler, #{});
        Pid when is_pid(Pid) ->
            ?LOG_DEBUG("[TOKEN_INTEGRATION] ✅ Token stream handler already running"),
            {ok, Pid}
    end.

%% Extract token-specific options
extract_token_options(Options) ->
    TokenKeys = [
        max_buffer_size, batch_size, flush_interval, 
        rate_limit, max_errors, token_buffer_size,
        token_batch_size, token_flush_interval
    ],
    
    maps:with(TokenKeys, Options).

%% Create enhanced handler for chat streaming
create_enhanced_handler(StreamPid, TokenHandler) ->
    fun(Event) ->
        % Process event with token handler
        case TokenHandler(Event, #{stream_pid => StreamPid}) of
            {tokens, Tokens} ->
                token_stream_fsm:process_tokens(StreamPid, Tokens);
            {tokens, Tokens, _Metadata} ->
                token_stream_fsm:process_tokens(StreamPid, Tokens);
            _ ->
                ok
        end,
        
        % Continue normal processing
        case maps:get(type, Event, undefined) of
            finish ->
                ?LOG_DEBUG("[TOKEN_INTEGRATION] 🏁 Stream finished"),
                stop;
            error ->
                ?LOG_ERROR("[TOKEN_INTEGRATION] ❌ Stream error: ~p", [Event]),
                {error, stream_error};
            _ ->
                continue
        end
    end.

%% Create enhanced handler for response streaming
create_response_enhanced_handler(StreamPid, TokenHandler) ->
    fun(Event) ->
        % Process event with token handler
        case TokenHandler(Event, #{stream_pid => StreamPid}) of
            {tokens, Tokens} ->
                token_stream_fsm:process_tokens(StreamPid, Tokens);
            {tokens, Tokens, _Metadata} ->
                token_stream_fsm:process_tokens(StreamPid, Tokens);
            _ ->
                ok
        end,
        
        % Continue normal processing
        case maps:get(<<"type">>, Event, undefined) of
            <<"response.completed">> ->
                ?LOG_DEBUG("[TOKEN_INTEGRATION] 🏁 Response completed"),
                stop;
            <<"response.failed">> ->
                ?LOG_ERROR("[TOKEN_INTEGRATION] ❌ Response failed: ~p", [Event]),
                {error, response_failed};
            <<"error">> ->
                ?LOG_ERROR("[TOKEN_INTEGRATION] ❌ Response error: ~p", [Event]),
                {error, api_error};
            _ ->
                continue
        end
    end.

%% Parse content into tokens (simple implementation)
parse_content_to_tokens(Content) when is_binary(Content) ->
    % Split on whitespace and punctuation
    Separators = [<<" ">>, <<"\n">>, <<"\t">>, <<".">>, <<",">>, <<"!">>, <<"?">>],
    Tokens = binary:split(Content, Separators, [global]),
    lists:filter(fun(Token) -> byte_size(Token) > 0 end, Tokens);
parse_content_to_tokens(_) ->
    [].

%% Default token handlers

%% Default token handler
default_token_handler() ->
    fun(Event, _Context) ->
        Tokens = extract_tokens_from_event(Event),
        case Tokens of
            [] -> continue;
            _ -> {tokens, Tokens}
        end
    end.

%% Chat-specific token handler
chat_token_handler() ->
    fun(Event, Context) ->
        case maps:get(type, Event, undefined) of
            content ->
                Content = maps:get(data, Event, <<>>),
                Tokens = parse_content_to_tokens(Content),
                {tokens, Tokens, #{event_type => content, context => Context}};
            text_delta ->
                Content = maps:get(data, Event, <<>>),
                Tokens = parse_content_to_tokens(Content),
                {tokens, Tokens, #{event_type => text_delta, context => Context}};
            tool_calls ->
                % Handle tool calls specially
                ?LOG_DEBUG("[TOKEN_INTEGRATION] 🔧 Processing tool calls"),
                continue;
            _ ->
                continue
        end
    end.

%% Response-specific token handler
response_token_handler() ->
    fun(Event, Context) ->
        case maps:get(<<"type">>, Event, undefined) of
            <<"response.output_text.delta">> ->
                Delta = maps:get(<<"delta">>, Event, <<>>),
                Tokens = parse_content_to_tokens(Delta),
                {tokens, Tokens, #{event_type => text_delta, context => Context}};
            <<"response.function_call_arguments.delta">> ->
                Delta = maps:get(<<"delta">>, Event, <<>>),
                Tokens = parse_content_to_tokens(Delta),
                {tokens, Tokens, #{event_type => function_args, context => Context}};
            _ ->
                continue
        end
    end.

%% Safe logging function with fallback
log_safe(Level, Msg) ->
    try
        case Level of
            info -> colored_logger:data(processed, Msg);
            error -> colored_logger:fire(inferno, Msg);
            debug -> colored_logger:debug(general, Msg);
            _ -> colored_logger:info(general, Msg)
        end
    catch
        _:_ ->
            % Fallback to standard logging
            Prefix = case Level of
                error -> "[ERROR] ";
                debug -> "[DEBUG] ";
                _ -> "[INFO] "
            end,
            io:format("~s~s~n", [Prefix, Msg])
    end.