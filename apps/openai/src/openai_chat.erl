%% openai_chat.erl
%% OpenAI Chat Completions API client
-module(openai_chat).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_chat_completion/3,
    create_streaming_completion/3,
    create_completion/2,
    stream_chat/3,
    chat_completion/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 60000).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:system(network, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:system(network, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).

-record(state, {
    api_key = undefined :: binary() | undefined,
    base_url = <<"https://api.openai.com/v1">> :: binary(),
    rate_limiter = undefined :: pid() | undefined,
    options = #{} :: map()
}).

%% Public API

%% Start the chat client
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Create a chat completion
create_chat_completion(Model, Messages, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_chat_completion, Model, Messages, Options}, Timeout).

%% Create a streaming chat completion
create_streaming_completion(Model, Messages, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_streaming_completion, Model, Messages, Options}, Timeout).

%% gen_server callbacks

init(Options) ->
    % Ensure inets application is started for httpc
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, _} = Error -> error(Error)
    end,
    
    % Get API key from environment
    ApiKey = case os:getenv("OPENAI_API_KEY") of
        false -> 
            error({missing_api_key, "OPENAI_API_KEY environment variable not set"});
        Key -> 
            list_to_binary(Key)
    end,
    
    % Set up rate limiter
    RateLimiter = case whereis(openai_rate_limiter) of
        undefined -> undefined;
        Pid -> Pid
    end,
    
    {ok, #state{
        api_key = ApiKey,
        rate_limiter = RateLimiter,
        options = Options
    }}.

handle_call({create_chat_completion, Model, Messages, Options}, From, State) ->
    ?LOG_INFO("[CHAT_API] 📤 Chat completion request"),
    ?LOG_INFO("[CHAT_API] Model: ~p", [Model]),
    ?LOG_INFO("[CHAT_API] Messages: ~p", [length(Messages)]),
    ?LOG_INFO("[CHAT_API] From: ~p", [From]),
    ?LOG_DEBUG("[CHAT_API] Options: ~p", [Options]),
    
    StartTime = erlang:monotonic_time(millisecond),
    case do_create_chat_completion(Model, Messages, Options, State) of
        {ok, Response} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            Usage = maps:get(<<"usage">>, Response, #{}),
            TotalTokens = maps:get(<<"total_tokens">>, Usage, 0),
            ?LOG_SUCCESS("[CHAT_API] ✅ Completion successful in ~pms (tokens: ~p)", [Duration, TotalTokens]),
            {reply, {ok, Response}, State};
        {error, Reason} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_ERROR("[CHAT_API] ❌ Completion failed after ~pms: ~p", [Duration, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({create_streaming_completion, Model, Messages, Options}, {From, _}, State) ->
    ?LOG_INFO("[CHAT_API] 🌊 Starting streaming completion"),
    ?LOG_INFO("[CHAT_API] Model: ~p", [Model]),
    ?LOG_INFO("[CHAT_API] Messages: ~p", [length(Messages)]),
    ?LOG_DEBUG("[CHAT_API] Stream options: ~p", [Options]),
    
    spawn(fun() -> 
        ?LOG_INFO("[CHAT_API] Spawned streaming worker for ~p", [From]),
        do_create_streaming_completion(Model, Messages, Options#{stream => true}, From, State) 
    end),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

do_create_chat_completion(Model, Messages, Options, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    ?LOG_DEBUG("[CHAT_API] Building request for model: ~p", [Model]),
    Url = <<BaseUrl/binary, "/chat/completions">>,
    
    % Calculate message tokens estimate
    MessageSizeEstimate = lists:foldl(fun(Msg, Acc) ->
        Content = maps:get(<<"content">>, Msg, <<"">>),
        Acc + byte_size(Content) div 4  % Rough token estimate
    end, 0, Messages),
    ?LOG_DEBUG("[CHAT_API] Estimated input tokens: ~p", [MessageSizeEstimate]),
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"messages">> => Messages
    },
    
    % Add optional parameters based on model type
    IsOSeries = is_o_series_model(Model),
    
    Body1 = case {maps:get(temperature, Options, undefined), IsOSeries} of
        {undefined, _} -> Body;
        {_, true} -> Body; % O-series models don't support temperature
        {Temp, false} -> Body#{<<"temperature">> => Temp}
    end,
    
    Body2 = case {maps:get(max_tokens, Options, undefined), maps:get(max_completion_tokens, Options, undefined), IsOSeries} of
        {undefined, undefined, _} -> Body1;
        {_, undefined, true} -> Body1; % O-series models don't support max_tokens, only max_completion_tokens
        {MaxTokens, undefined, false} -> Body1#{<<"max_tokens">> => MaxTokens};
        {_, MaxCompletionTokens, true} -> Body1#{<<"max_completion_tokens">> => MaxCompletionTokens};
        {_, MaxCompletionTokens, false} -> Body1#{<<"max_completion_tokens">> => MaxCompletionTokens} % Prefer max_completion_tokens if both provided
    end,
    
    % Add reasoning_effort for o-series models
    Body2_5 = case {maps:get(reasoning_effort, Options, undefined), IsOSeries} of
        {undefined, _} -> Body2;
        {_, false} -> Body2; % Non-o-series models don't support reasoning_effort
        {ReasoningEffort, true} -> Body2#{<<"reasoning_effort">> => ReasoningEffort}
    end,
    
    Body3 = case maps:get(stream, Options, undefined) of
        undefined -> Body2_5;
        Stream -> Body2_5#{<<"stream">> => Stream}
    end,
    Body4 = case maps:get(tools, Options, undefined) of
        undefined -> Body3;
        Tools -> Body3#{<<"tools">> => Tools}
    end,
    Body5 = case maps:get(tool_choice, Options, undefined) of
        undefined -> Body4;
        ToolChoice -> Body4#{<<"tool_choice">> => ToolChoice}
    end,
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body5),
    ?LOG_DEBUG("[CHAT_API] Request body size: ~p bytes", [byte_size(JsonBody)]),
    
    % Log if tools are being used
    case maps:get(<<"tools">>, Body5, undefined) of
        undefined -> ok;
        ToolsList -> ?LOG_INFO("[CHAT_API] 🔧 Using ~p tools", [length(ToolsList)])
    end,
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    ?LOG_INFO("[CHAT_API] 🚀 Sending request to OpenAI..."),
    RequestStartTime = erlang:monotonic_time(millisecond),
    
    % Make HTTP request
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            RequestDuration = erlang:monotonic_time(millisecond) - RequestStartTime,
            ?LOG_SUCCESS("[CHAT_API] ✅ Response received in ~pms", [RequestDuration]),
            ?LOG_DEBUG("[CHAT_API] Response size: ~p bytes", [byte_size(ResponseBody)]),
            
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    % Log usage stats
                    case maps:get(<<"usage">>, Response, undefined) of
                        undefined -> ok;
                        Usage ->
                            ?LOG_INFO("[CHAT_API] 📊 Token usage - Prompt: ~p, Completion: ~p, Total: ~p",
                                    [maps:get(<<"prompt_tokens">>, Usage, 0),
                                     maps:get(<<"completion_tokens">>, Usage, 0),
                                     maps:get(<<"total_tokens">>, Usage, 0)])
                    end,
                    
                    % Check for function calls
                    case maps:get(<<"choices">>, Response, []) of
                        [#{<<"message">> := #{<<"tool_calls">> := ToolCalls}} | _] when is_list(ToolCalls) ->
                            ?LOG_INFO("[CHAT_API] 🔧 Response contains ~p tool calls", [length(ToolCalls)]);
                        _ -> ok
                    end,
                    
                    {ok, Response};
                _ ->
                    ?LOG_ERROR("[CHAT_API] Invalid response format"),
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            RequestDuration = erlang:monotonic_time(millisecond) - RequestStartTime,
            ?LOG_ERROR("[CHAT_API] ❌ API error after ~pms - Status: ~p", [RequestDuration, StatusCode]),
            
            % Try to parse error response
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    ErrorMsg = maps:get(<<"message">>, Error, <<"Unknown error">>),
                    ErrorType = maps:get(<<"type">>, Error, <<"unknown">>),
                    ?LOG_ERROR("[CHAT_API] Error details - Type: ~s, Message: ~s", [ErrorType, ErrorMsg]),
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    ?LOG_ERROR("[CHAT_API] Raw error response: ~s", [ResponseBody]),
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            RequestDuration = erlang:monotonic_time(millisecond) - RequestStartTime,
            ?LOG_ERROR("[CHAT_API] 💥 Request failed after ~pms: ~p", [RequestDuration, Reason]),
            {error, {request_failed, Reason}}
    end.

%% Streaming implementation
do_create_streaming_completion(Model, Messages, Options, CallerPid, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    ?LOG_INFO("[CHAT_STREAM] 🌊 Starting streaming request for ~p", [CallerPid]),
    Url = <<BaseUrl/binary, "/chat/completions">>,
    
    % Build request body with stream enabled
    Body = #{
        <<"model">> => Model,
        <<"messages">> => Messages,
        <<"stream">> => true
    },
    
    % Add optional parameters based on model type
    IsOSeries = is_o_series_model(Model),
    
    Body1 = case {maps:get(temperature, Options, undefined), IsOSeries} of
        {undefined, _} -> Body;
        {_, true} -> Body; % O-series models don't support temperature
        {Temp, false} -> Body#{<<"temperature">> => Temp}
    end,
    
    Body2 = case {maps:get(max_tokens, Options, undefined), maps:get(max_completion_tokens, Options, undefined), IsOSeries} of
        {undefined, undefined, _} -> Body1;
        {_, undefined, true} -> Body1; % O-series models don't support max_tokens, only max_completion_tokens
        {MaxTokens, undefined, false} -> Body1#{<<"max_tokens">> => MaxTokens};
        {_, MaxCompletionTokens, true} -> Body1#{<<"max_completion_tokens">> => MaxCompletionTokens};
        {_, MaxCompletionTokens, false} -> Body1#{<<"max_completion_tokens">> => MaxCompletionTokens} % Prefer max_completion_tokens if both provided
    end,
    
    % Add reasoning_effort for o-series models
    Body2_5 = case {maps:get(reasoning_effort, Options, undefined), IsOSeries} of
        {undefined, _} -> Body2;
        {_, false} -> Body2; % Non-o-series models don't support reasoning_effort
        {ReasoningEffort, true} -> Body2#{<<"reasoning_effort">> => ReasoningEffort}
    end,
    
    Body3 = case maps:get(tools, Options, undefined) of
        undefined -> Body2_5;
        Tools -> Body2_5#{<<"tools">> => Tools}
    end,
    Body4 = case maps:get(tool_choice, Options, undefined) of
        undefined -> Body3;
        ToolChoice -> Body3#{<<"tool_choice">> => ToolChoice}
    end,
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body4),
    ?LOG_DEBUG("[CHAT_STREAM] Request body size: ~p bytes", [byte_size(JsonBody)]),
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    ?LOG_INFO("[CHAT_STREAM] 🚀 Initiating SSE connection..."),
    
    % Make HTTP request with streaming
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody},
                      [{timeout, 60000}, {connect_timeout, 5000}], 
                      [{sync, false}, {stream, self}, {body_format, binary}]) of
        {ok, RequestId} ->
            handle_stream_response(RequestId, CallerPid, <<>>);
        {error, Reason} ->
            gen_server:reply(CallerPid, {error, {request_failed, Reason}})
    end.

handle_stream_response(RequestId, CallerPid, Buffer) ->
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            handle_stream_response(RequestId, CallerPid, Buffer);
            
        {http, {RequestId, stream, BinBodyPart}} ->
            % Accumulate data and process complete events
            NewBuffer = <<Buffer/binary, BinBodyPart/binary>>,
            {ProcessedBuffer, Events} = process_sse_events(NewBuffer),
            
            % Send events to caller
            lists:foreach(fun(Event) ->
                CallerPid ! {stream_chunk, Event}
            end, Events),
            
            % Continue streaming
            handle_stream_response(RequestId, CallerPid, ProcessedBuffer);
            
        {http, {RequestId, stream_end, _Headers}} ->
            % Process any remaining data in buffer
            {_, Events} = process_sse_events(<<Buffer/binary, "\n\n">>),
            lists:foreach(fun(Event) ->
                CallerPid ! {stream_chunk, Event}
            end, Events),
            CallerPid ! stream_complete;
            
        {http, {RequestId, {error, Reason}}} ->
            CallerPid ! {stream_error, Reason}
            
    after 30000 ->
        CallerPid ! {stream_error, timeout}
    end.

%% Process Server-Sent Events (SSE) format with semantic event support
process_sse_events(Buffer) ->
    Lines = binary:split(Buffer, <<"\n">>, [global]),
    process_sse_lines(Lines, [], []).

process_sse_lines([], Acc, Events) ->
    {iolist_to_binary(lists:reverse(Acc)), lists:reverse(Events)};
process_sse_lines([Line | Rest], Acc, Events) ->
    case Line of
        <<"data: ", Data/binary>> ->
            case Data of
                <<"[DONE]">> ->
                    % Stream is complete
                    process_sse_lines(Rest, [], [{stream_done, #{}} | Events]);
                _ ->
                    % Try to parse JSON
                    case catch jsx:decode(Data, [return_maps]) of
                        {'EXIT', _} ->
                            % Incomplete JSON, keep accumulating
                            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events);
                        Decoded ->
                            % Process semantic events for Responses API or legacy Chat API
                            Event = process_streaming_event(Decoded),
                            process_sse_lines(Rest, [], [Event | Events])
                    end
            end;
        <<>> ->
            % Empty line, potential event boundary
            process_sse_lines(Rest, Acc, Events);
        _ ->
            % Other lines, keep accumulating
            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events)
    end.

%% Process streaming events for both Responses API and Chat API
process_streaming_event(#{<<"type">> := Type} = Event) ->
    % Responses API semantic events
    case Type of
        <<"response.created">> ->
            #{type => response_created, data => Event};
        <<"response.in_progress">> ->
            #{type => response_in_progress, data => Event};
        <<"response.completed">> ->
            #{type => response_completed, data => Event};
        <<"response.failed">> ->
            #{type => response_failed, data => Event};
        <<"response.output_item.added">> ->
            #{type => output_item_added, data => Event};
        <<"response.output_item.done">> ->
            #{type => output_item_done, data => Event};
        <<"response.content_part.added">> ->
            #{type => content_part_added, data => Event};
        <<"response.content_part.done">> ->
            #{type => content_part_done, data => Event};
        <<"response.output_text.delta">> ->
            Delta = maps:get(<<"delta">>, Event, <<>>),
            #{type => text_delta, data => Delta};
        <<"response.output_text.done">> ->
            #{type => text_done, data => Event};
        <<"response.refusal.delta">> ->
            Delta = maps:get(<<"delta">>, Event, <<>>),
            #{type => refusal_delta, data => Delta};
        <<"response.refusal.done">> ->
            #{type => refusal_done, data => Event};
        <<"response.function_call_arguments.delta">> ->
            Delta = maps:get(<<"delta">>, Event, <<>>),
            #{type => function_call_arguments_delta, data => Delta};
        <<"response.function_call_arguments.done">> ->
            #{type => function_call_arguments_done, data => Event};
        <<"response.file_search_call.in_progress">> ->
            #{type => file_search_in_progress, data => Event};
        <<"response.file_search_call.searching">> ->
            #{type => file_search_searching, data => Event};
        <<"response.file_search_call.completed">> ->
            #{type => file_search_completed, data => Event};
        <<"response.code_interpreter_call.in_progress">> ->
            #{type => code_interpreter_in_progress, data => Event};
        <<"response.code_interpreter_call.code.delta">> ->
            Delta = maps:get(<<"delta">>, Event, <<>>),
            #{type => code_interpreter_code_delta, data => Delta};
        <<"response.code_interpreter_call.code.done">> ->
            #{type => code_interpreter_code_done, data => Event};
        <<"response.code_interpreter_call.interpreting">> ->
            #{type => code_interpreter_interpreting, data => Event};
        <<"response.code_interpreter_call.completed">> ->
            #{type => code_interpreter_completed, data => Event};
        <<"error">> ->
            #{type => error, data => Event};
        _ ->
            #{type => unknown_event, data => Event}
    end;
process_streaming_event(Data) ->
    % Legacy Chat API format - extract content from choices/delta
    extract_stream_content(Data).

extract_stream_content(Data) ->
    case maps:get(<<"choices">>, Data, []) of
        [] -> #{type => content, data => <<>>};
        [Choice | _] ->
            Delta = maps:get(<<"delta">>, Choice, #{}),
            Index = maps:get(<<"index">>, Choice, 0),
            
            % Check for different types of delta content
            case Delta of
                #{<<"content">> := Content} when Content =/= null -> 
                    #{type => content, data => Content};
                #{<<"tool_calls">> := ToolCalls} when ToolCalls =/= null ->
                    % Enhanced tool call handling with index tracking
                    #{type => tool_calls, data => ToolCalls, index => Index};
                #{<<"function_call">> := FuncCall} when FuncCall =/= null ->
                    % Legacy function call format
                    #{type => function_call, data => FuncCall, index => Index};
                #{<<"role">> := Role} ->
                    % Role assignment (usually at the beginning)
                    #{type => role, data => Role};
                _ -> 
                    % Check if this is a finish reason
                    case maps:get(<<"finish_reason">>, Choice, null) of
                        null -> #{type => content, data => <<>>};
                        <<"tool_calls">> -> #{type => finish, reason => tool_calls};
                        Reason -> #{type => finish, reason => Reason}
                    end
            end
    end.

%% Helper function to detect models with parameter restrictions
is_o_series_model(Model) when is_binary(Model) ->
    ModelStr = binary_to_list(Model),
    lists:any(fun(Pattern) -> 
        string:str(ModelStr, Pattern) > 0 
    end, ["o1", "o3", "o4-mini", "gpt-4.1"]);
is_o_series_model(_) -> false.

%% Legacy API compatibility functions

%% Simple chat completion with default options
chat_completion(Messages) ->
    create_chat_completion(<<"gpt-4o-mini">>, Messages, #{}).

%% Create completion (non-streaming)
create_completion(Model, Messages) ->
    create_chat_completion(Model, Messages, #{}).

%% Stream chat (legacy name for streaming completion)
stream_chat(Model, Messages, Options) ->
    create_streaming_completion(Model, Messages, Options).