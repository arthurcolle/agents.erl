%% token_stream_fsm.erl
%% Token-level streaming state machine using gen_statem
%% Provides fine-grained token processing with buffering, rate limiting, and state management

-module(token_stream_fsm).
-behaviour(gen_statem).

%% Public API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    process_token/2,
    process_tokens/2,
    flush_buffer/1,
    get_state/1,
    get_statistics/1,
    set_options/2
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4
]).

%% State callbacks
-export([
    idle/3,
    buffering/3,
    streaming/3,
    rate_limited/3,
    error_state/3
]).

%% Internal exports
-export([
    format_status/2
]).

%% Records
-record(data, {
    tokens_buffer = [] :: [binary()],
    buffer_size = 0 :: non_neg_integer(),
    max_buffer_size = 1000 :: pos_integer(),
    batch_size = 10 :: pos_integer(),
    flush_interval = 100 :: pos_integer(), % milliseconds
    rate_limit = 100 :: pos_integer(), % tokens per second
    last_flush = 0 :: non_neg_integer(),
    statistics = #{} :: map(),
    handler_pid :: pid() | undefined,
    options = #{} :: map(),
    error_count = 0 :: non_neg_integer(),
    max_errors = 5 :: pos_integer(),
    start_time :: non_neg_integer()
}).

%% Logging macros
-define(LOG_INFO(Msg), log_safe(info, Msg)).
-define(LOG_INFO(Msg, Args), log_safe(info, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), log_safe(error, Msg)).
-define(LOG_ERROR(Msg, Args), log_safe(error, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), log_safe(debug, Msg)).
-define(LOG_DEBUG(Msg, Args), log_safe(debug, io_lib:format(Msg, Args))).

%% Public API

%% Start the token stream FSM with default options
start_link(HandlerPid) ->
    start_link(HandlerPid, #{}).

%% Start the token stream FSM with custom options
start_link(HandlerPid, Options) ->
    gen_statem:start_link(?MODULE, {HandlerPid, Options}, []).

%% Stop the token stream FSM
stop(StreamRef) ->
    gen_statem:stop(StreamRef).

%% Process a single token
process_token(StreamRef, Token) when is_binary(Token) ->
    gen_statem:cast(StreamRef, {process_token, Token}).

%% Process multiple tokens at once
process_tokens(StreamRef, Tokens) when is_list(Tokens) ->
    gen_statem:cast(StreamRef, {process_tokens, Tokens}).

%% Force flush the current buffer
flush_buffer(StreamRef) ->
    gen_statem:call(StreamRef, flush_buffer).

%% Get current state information
get_state(StreamRef) ->
    gen_statem:call(StreamRef, get_state).

%% Get streaming statistics
get_statistics(StreamRef) ->
    gen_statem:call(StreamRef, get_statistics).

%% Update options dynamically
set_options(StreamRef, NewOptions) ->
    gen_statem:call(StreamRef, {set_options, NewOptions}).

%% gen_statem callbacks

%% Initialize the state machine
init({HandlerPid, Options}) ->
    ?LOG_INFO("[TOKEN_STREAM] 🚀 Initializing token stream FSM"),
    
    % Validate handler pid
    case is_pid(HandlerPid) andalso is_process_alive(HandlerPid) of
        true ->
            ?LOG_INFO("[TOKEN_STREAM] ✅ Handler PID validated: ~p", [HandlerPid]);
        false ->
            ?LOG_ERROR("[TOKEN_STREAM] ❌ Invalid handler PID: ~p", [HandlerPid]),
            {stop, {error, invalid_handler_pid}}
    end,
    
    % Initialize data record with options
    Data = apply_options(#data{
        handler_pid = HandlerPid,
        start_time = erlang:system_time(millisecond),
        statistics = init_statistics()
    }, Options),
    
    ?LOG_INFO("[TOKEN_STREAM] 📊 Starting in idle state with buffer_size=~p, batch_size=~p", 
              [Data#data.max_buffer_size, Data#data.batch_size]),
    
    {ok, idle, Data}.

%% Callback mode - use state functions with state enter calls
callback_mode() ->
    [state_functions, state_enter].

%% State: idle - waiting for tokens
idle(enter, _OldState, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 🔄 Entered idle state"),
    {keep_state, update_statistics(Data, state_change, idle)};

idle(cast, {process_token, Token}, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 📥 Received single token: ~s", [Token]),
    NewData = add_token_to_buffer(Token, Data),
    
    case should_start_streaming(NewData) of
        true ->
            ?LOG_INFO("[TOKEN_STREAM] 🌊 Starting streaming mode"),
            {next_state, streaming, NewData, 
             [{state_timeout, Data#data.flush_interval, flush}]};
        false ->
            {next_state, buffering, NewData}
    end;

idle(cast, {process_tokens, Tokens}, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 📥 Received ~p tokens", [length(Tokens)]),
    NewData = add_tokens_to_buffer(Tokens, Data),
    
    case should_start_streaming(NewData) of
        true ->
            ?LOG_INFO("[TOKEN_STREAM] 🌊 Starting streaming mode"),
            {next_state, streaming, NewData,
             [{state_timeout, Data#data.flush_interval, flush}]};
        false ->
            {next_state, buffering, NewData}
    end;

idle({call, From}, flush_buffer, Data) ->
    {keep_state, Data, [{reply, From, {ok, []}}]};

idle({call, From}, get_state, Data) ->
    StateInfo = #{
        state => idle,
        buffer_size => Data#data.buffer_size,
        tokens_processed => get_statistic(Data, tokens_processed),
        uptime => erlang:system_time(millisecond) - Data#data.start_time
    },
    {keep_state, Data, [{reply, From, {ok, StateInfo}}]};

idle({call, From}, get_statistics, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.statistics}}]};

idle({call, From}, {set_options, Options}, Data) ->
    NewData = apply_options(Data, Options),
    {keep_state, NewData, [{reply, From, ok}]};

idle(EventType, EventContent, Data) ->
    handle_common_events(EventType, EventContent, Data).

%% State: buffering - collecting tokens
buffering(enter, _OldState, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 📦 Entered buffering state"),
    {keep_state, update_statistics(Data, state_change, buffering)};

buffering(cast, {process_token, Token}, Data) ->
    NewData = add_token_to_buffer(Token, Data),
    
    case should_flush_buffer(NewData) of
        true ->
            ?LOG_DEBUG("[TOKEN_STREAM] 🚿 Buffer full, flushing"),
            case flush_tokens(NewData) of
                {ok, FlushedData} ->
                    {next_state, streaming, FlushedData,
                     [{state_timeout, Data#data.flush_interval, flush}]};
                {error, Reason} ->
                    ?LOG_ERROR("[TOKEN_STREAM] ❌ Flush failed: ~p", [Reason]),
                    {next_state, error_state, increment_error_count(NewData, Reason)}
            end;
        false ->
            {keep_state, NewData}
    end;

buffering(cast, {process_tokens, Tokens}, Data) ->
    NewData = add_tokens_to_buffer(Tokens, Data),
    
    case should_flush_buffer(NewData) of
        true ->
            ?LOG_DEBUG("[TOKEN_STREAM] 🚿 Buffer full, flushing"),
            case flush_tokens(NewData) of
                {ok, FlushedData} ->
                    {next_state, streaming, FlushedData,
                     [{state_timeout, Data#data.flush_interval, flush}]};
                {error, Reason} ->
                    ?LOG_ERROR("[TOKEN_STREAM] ❌ Flush failed: ~p", [Reason]),
                    {next_state, error_state, increment_error_count(NewData, Reason)}
            end;
        false ->
            {keep_state, NewData}
    end;

buffering({call, From}, flush_buffer, Data) ->
    case flush_tokens(Data) of
        {ok, FlushedData} ->
            {next_state, idle, FlushedData, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, error_state, increment_error_count(Data, Reason),
             [{reply, From, {error, Reason}}]}
    end;

buffering(EventType, EventContent, Data) ->
    handle_common_events(EventType, EventContent, Data).

%% State: streaming - actively processing tokens
streaming(enter, _OldState, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] ⚡ Entered streaming state"),
    {keep_state, update_statistics(Data, state_change, streaming)};

streaming(state_timeout, flush, Data) ->
    case Data#data.buffer_size > 0 of
        true ->
            ?LOG_DEBUG("[TOKEN_STREAM] ⏰ Timeout flush triggered"),
            case flush_tokens(Data) of
                {ok, FlushedData} ->
                    {keep_state, FlushedData,
                     [{state_timeout, Data#data.flush_interval, flush}]};
                {error, Reason} ->
                    ?LOG_ERROR("[TOKEN_STREAM] ❌ Timeout flush failed: ~p", [Reason]),
                    {next_state, error_state, increment_error_count(Data, Reason)}
            end;
        false ->
            {keep_state, Data,
             [{state_timeout, Data#data.flush_interval, flush}]}
    end;

streaming(cast, {process_token, Token}, Data) ->
    NewData = add_token_to_buffer(Token, Data),
    
    case should_rate_limit(NewData) of
        true ->
            ?LOG_DEBUG("[TOKEN_STREAM] 🐌 Rate limiting triggered"),
            {next_state, rate_limited, NewData,
             [{state_timeout, calculate_rate_limit_delay(NewData), resume}]};
        false ->
            case should_flush_buffer(NewData) of
                true ->
                    case flush_tokens(NewData) of
                        {ok, FlushedData} ->
                            {keep_state, FlushedData,
                             [{state_timeout, Data#data.flush_interval, flush}]};
                        {error, Reason} ->
                            {next_state, error_state, increment_error_count(NewData, Reason)}
                    end;
                false ->
                    {keep_state, NewData}
            end
    end;

streaming(cast, {process_tokens, Tokens}, Data) ->
    NewData = add_tokens_to_buffer(Tokens, Data),
    
    case should_rate_limit(NewData) of
        true ->
            ?LOG_DEBUG("[TOKEN_STREAM] 🐌 Rate limiting triggered"),
            {next_state, rate_limited, NewData,
             [{state_timeout, calculate_rate_limit_delay(NewData), resume}]};
        false ->
            case should_flush_buffer(NewData) of
                true ->
                    case flush_tokens(NewData) of
                        {ok, FlushedData} ->
                            {keep_state, FlushedData,
                             [{state_timeout, Data#data.flush_interval, flush}]};
                        {error, Reason} ->
                            {next_state, error_state, increment_error_count(NewData, Reason)}
                    end;
                false ->
                    {keep_state, NewData}
            end
    end;

streaming({call, From}, flush_buffer, Data) ->
    case flush_tokens(Data) of
        {ok, FlushedData} ->
            {keep_state, FlushedData, [{reply, From, ok}]};
        {error, Reason} ->
            {next_state, error_state, increment_error_count(Data, Reason),
             [{reply, From, {error, Reason}}]}
    end;

streaming(EventType, EventContent, Data) ->
    handle_common_events(EventType, EventContent, Data).

%% State: rate_limited - waiting due to rate limiting
rate_limited(enter, _OldState, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 🐌 Entered rate limited state"),
    {keep_state, update_statistics(Data, state_change, rate_limited)};

rate_limited(state_timeout, resume, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] 🚀 Resuming from rate limit"),
    case Data#data.buffer_size > 0 of
        true ->
            {next_state, streaming, Data,
             [{state_timeout, Data#data.flush_interval, flush}]};
        false ->
            {next_state, idle, Data}
    end;

rate_limited(cast, {process_token, Token}, Data) ->
    % Buffer tokens while rate limited
    NewData = add_token_to_buffer(Token, Data),
    {keep_state, NewData};

rate_limited(cast, {process_tokens, Tokens}, Data) ->
    % Buffer tokens while rate limited
    NewData = add_tokens_to_buffer(Tokens, Data),
    {keep_state, NewData};

rate_limited(EventType, EventContent, Data) ->
    handle_common_events(EventType, EventContent, Data).

%% State: error_state - handling errors
error_state(enter, _OldState, Data) ->
    ?LOG_ERROR("[TOKEN_STREAM] ❌ Entered error state"),
    {keep_state, update_statistics(Data, state_change, error_state)};

error_state(cast, {process_token, _Token}, Data) ->
    % Drop tokens in error state
    {keep_state, Data};

error_state(cast, {process_tokens, _Tokens}, Data) ->
    % Drop tokens in error state
    {keep_state, Data};

error_state({call, From}, flush_buffer, Data) ->
    {keep_state, Data, [{reply, From, {error, in_error_state}}]};

error_state({call, From}, {set_options, Options}, Data) ->
    % Allow options update to potentially recover
    NewData = apply_options(Data#data{error_count = 0}, Options),
    {next_state, idle, NewData, [{reply, From, ok}]};

error_state(EventType, EventContent, Data) ->
    handle_common_events(EventType, EventContent, Data).

%% Common event handling
handle_common_events({call, From}, get_state, Data) ->
    StateInfo = #{
        state => get_current_state(),
        buffer_size => Data#data.buffer_size,
        tokens_processed => get_statistic(Data, tokens_processed),
        error_count => Data#data.error_count,
        uptime => erlang:system_time(millisecond) - Data#data.start_time
    },
    {keep_state, Data, [{reply, From, {ok, StateInfo}}]};

handle_common_events({call, From}, get_statistics, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.statistics}}]};

handle_common_events(info, {'DOWN', _Ref, process, Pid, Reason}, 
                    #data{handler_pid = Pid} = Data) ->
    ?LOG_ERROR("[TOKEN_STREAM] ❌ Handler process down: ~p", [Reason]),
    {stop, {handler_down, Reason}, Data};

handle_common_events(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% Terminate callback
terminate(Reason, State, #data{statistics = Stats}) ->
    ?LOG_INFO("[TOKEN_STREAM] 🛑 Terminating in state ~p: ~p", [State, Reason]),
    ?LOG_INFO("[TOKEN_STREAM] 📊 Final statistics: ~p", [Stats]),
    ok.

%% Code change callback
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Format status for debugging
format_status(Opt, [_PDict, State, Data]) ->
    FilteredData = Data#data{
        tokens_buffer = length(Data#data.tokens_buffer),
        handler_pid = redacted
    },
    case Opt of
        terminate ->
            {State, FilteredData};
        normal ->
            [{data, [{"State", {State, FilteredData}}]}]
    end.

%% Internal helper functions

%% Get current state (for introspection)
get_current_state() ->
    % This would be set by the state machine engine
    % For now, return unknown
    unknown.

%% Initialize statistics
init_statistics() ->
    #{
        tokens_processed => 0,
        batches_flushed => 0,
        state_changes => 0,
        errors => 0,
        start_time => erlang:system_time(millisecond),
        last_activity => erlang:system_time(millisecond)
    }.

%% Update statistics
update_statistics(Data, Type, Value) ->
    Stats = Data#data.statistics,
    NewStats = case Type of
        tokens_processed ->
            Stats#{tokens_processed => maps:get(tokens_processed, Stats, 0) + Value};
        batch_flushed ->
            Stats#{batches_flushed => maps:get(batches_flushed, Stats, 0) + 1};
        state_change ->
            Stats#{
                state_changes => maps:get(state_changes, Stats, 0) + 1,
                last_activity => erlang:system_time(millisecond)
            };
        error ->
            Stats#{errors => maps:get(errors, Stats, 0) + 1};
        _ ->
            Stats
    end,
    Data#data{statistics = NewStats}.

%% Get statistic value
get_statistic(Data, Key) ->
    maps:get(Key, Data#data.statistics, 0).

%% Add single token to buffer
add_token_to_buffer(Token, Data) ->
    NewBuffer = [Token | Data#data.tokens_buffer],
    NewSize = Data#data.buffer_size + 1,
    update_statistics(
        Data#data{
            tokens_buffer = NewBuffer,
            buffer_size = NewSize
        },
        tokens_processed, 1
    ).

%% Add multiple tokens to buffer
add_tokens_to_buffer(Tokens, Data) ->
    NewBuffer = lists:reverse(Tokens) ++ Data#data.tokens_buffer,
    NewSize = Data#data.buffer_size + length(Tokens),
    update_statistics(
        Data#data{
            tokens_buffer = NewBuffer,
            buffer_size = NewSize
        },
        tokens_processed, length(Tokens)
    ).

%% Check if should start streaming
should_start_streaming(Data) ->
    Data#data.buffer_size >= Data#data.batch_size.

%% Check if should flush buffer
should_flush_buffer(Data) ->
    Data#data.buffer_size >= Data#data.max_buffer_size.

%% Check if should rate limit
should_rate_limit(Data) ->
    Now = erlang:system_time(millisecond),
    TimeSinceLastFlush = Now - Data#data.last_flush,
    TokensPerSecond = case TimeSinceLastFlush of
        0 -> Data#data.rate_limit + 1; % Force rate limit if no time passed
        _ -> (Data#data.buffer_size * 1000) div TimeSinceLastFlush
    end,
    TokensPerSecond > Data#data.rate_limit.

%% Calculate rate limit delay
calculate_rate_limit_delay(Data) ->
    BaseDelay = 1000 div Data#data.rate_limit, % milliseconds per token
    MinDelay = max(BaseDelay, 10), % At least 10ms
    MinDelay + (Data#data.buffer_size div 10). % Scale with buffer size

%% Flush tokens to handler
flush_tokens(Data) ->
    case Data#data.buffer_size of
        0 ->
            {ok, Data};
        _ ->
            Tokens = lists:reverse(Data#data.tokens_buffer),
            case send_tokens_to_handler(Data#data.handler_pid, Tokens) of
                ok ->
                    ?LOG_DEBUG("[TOKEN_STREAM] 🚿 Flushed ~p tokens", [length(Tokens)]),
                    NewData = update_statistics(
                        Data#data{
                            tokens_buffer = [],
                            buffer_size = 0,
                            last_flush = erlang:system_time(millisecond)
                        },
                        batch_flushed, 1
                    ),
                    {ok, NewData};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Send tokens to handler process
send_tokens_to_handler(HandlerPid, Tokens) ->
    try
        HandlerPid ! {token_batch, Tokens},
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%% Increment error count
increment_error_count(Data, Reason) ->
    NewErrorCount = Data#data.error_count + 1,
    NewData = update_statistics(Data#data{error_count = NewErrorCount}, error, 1),
    
    case NewErrorCount >= Data#data.max_errors of
        true ->
            ?LOG_ERROR("[TOKEN_STREAM] ❌ Max errors reached (~p), staying in error state", 
                      [Data#data.max_errors]);
        false ->
            ?LOG_ERROR("[TOKEN_STREAM] ⚠️  Error (~p/~p): ~p", 
                      [NewErrorCount, Data#data.max_errors, Reason])
    end,
    
    NewData.

%% Apply options to data record
apply_options(Data, Options) ->
    lists:foldl(fun({Key, Value}, AccData) ->
        apply_option(Key, Value, AccData)
    end, Data, maps:to_list(Options)).

%% Apply individual option
apply_option(max_buffer_size, Value, Data) when is_integer(Value), Value > 0 ->
    Data#data{max_buffer_size = Value};
apply_option(batch_size, Value, Data) when is_integer(Value), Value > 0 ->
    Data#data{batch_size = Value};
apply_option(flush_interval, Value, Data) when is_integer(Value), Value > 0 ->
    Data#data{flush_interval = Value};
apply_option(rate_limit, Value, Data) when is_integer(Value), Value > 0 ->
    Data#data{rate_limit = Value};
apply_option(max_errors, Value, Data) when is_integer(Value), Value > 0 ->
    Data#data{max_errors = Value};
apply_option(Key, Value, Data) ->
    ?LOG_DEBUG("[TOKEN_STREAM] ⚠️  Unknown option: ~p = ~p", [Key, Value]),
    Data#data{options = maps:put(Key, Value, Data#data.options)}.

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