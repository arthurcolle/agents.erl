-module(ai_error_processor).
-behaviour(gen_server).

%% API
-export([start_link/0, process_error/1, process_websocket_error/2, get_error_analysis/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    openai_client,
    error_cache = #{},
    analysis_queue = [],
    processing = false
}).

-define(MAX_CACHE_SIZE, 1000).
-define(ANALYSIS_TIMEOUT, 30000).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_error(ErrorData) ->
    gen_server:cast(?MODULE, {process_error, ErrorData}).

process_websocket_error(ErrorData, Context) ->
    gen_server:cast(?MODULE, {process_websocket_error, ErrorData, Context}).

get_error_analysis(ErrorId) ->
    gen_server:call(?MODULE, {get_analysis, ErrorId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    colored_logger:data(processed, "[AI_ERROR_PROCESSOR] 🤖 Starting AI Error Processor with GPT-4o-mini"),
    
    % Use the existing openai_chat process
    ClientPid = whereis(openai_chat),
    case ClientPid of
        undefined ->
            % If not started, start it
            case openai_chat:start_link(#{api_key => os:getenv("OPENAI_API_KEY")}) of
                {ok, Pid} -> 
                    {ok, #state{openai_client = Pid}};
                {error, {already_started, Pid}} ->
                    {ok, #state{openai_client = Pid}};
                Error ->
                    {stop, Error}
            end;
        Pid when is_pid(Pid) ->
            % Use existing process
            {ok, #state{openai_client = Pid}}
    end.

handle_call({get_analysis, ErrorId}, _From, State = #state{error_cache = Cache}) ->
    case maps:get(ErrorId, Cache, not_found) of
        not_found ->
            {reply, {error, not_found}, State};
        Analysis ->
            {reply, {ok, Analysis}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process_error, ErrorData}, State) ->
    NewState = queue_error_for_analysis(ErrorData, <<"general">>, State),
    {noreply, maybe_process_queue(NewState)};

handle_cast({process_websocket_error, ErrorData, Context}, State) ->
    NewState = queue_error_for_analysis(ErrorData, Context, State),
    {noreply, maybe_process_queue(NewState)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({analysis_complete, ErrorId, Analysis}, State = #state{error_cache = Cache}) ->
    colored_logger:complete(success, io_lib:format("[AI_ERROR_PROCESSOR] ✅ Analysis complete for error ~s", [ErrorId])),
    
    % Update cache
    NewCache = maps:put(ErrorId, Analysis, Cache),
    CleanedCache = maybe_cleanup_cache(NewCache),
    
    % Broadcast analysis to connected websockets
    broadcast_analysis(ErrorId, Analysis),
    
    NewState = State#state{
        error_cache = CleanedCache,
        processing = false
    },
    {noreply, maybe_process_queue(NewState)};

handle_info({analysis_error, ErrorId, Reason}, State) ->
    colored_logger:fire(inferno, io_lib:format("[AI_ERROR_PROCESSOR] ❌ Analysis failed for error ~s: ~p", [ErrorId, Reason])),
    
    NewState = State#state{processing = false},
    {noreply, maybe_process_queue(NewState)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

queue_error_for_analysis(ErrorData, Context, State = #state{analysis_queue = Queue}) ->
    ErrorId = generate_error_id(ErrorData),
    
    % Create enhanced error record with context
    EnhancedError = #{
        id => ErrorId,
        timestamp => erlang:system_time(second),
        data => ErrorData,
        context => Context,
        source => <<"websocket">>,
        priority => calculate_priority(ErrorData, Context)
    },
    
    colored_logger:data(processed, io_lib:format("[AI_ERROR_PROCESSOR] 📝 Queued error for analysis: ~s (Context: ~s)", [ErrorId, Context])),
    
    % Add to queue sorted by priority
    NewQueue = insert_by_priority(EnhancedError, Queue),
    State#state{analysis_queue = NewQueue}.

maybe_process_queue(State = #state{processing = true}) ->
    State;
maybe_process_queue(State = #state{analysis_queue = []}) ->
    State;
maybe_process_queue(State = #state{analysis_queue = [Error | Rest], openai_client = Client}) ->
    spawn(fun() -> analyze_error_with_ai(Error, Client) end),
    State#state{
        analysis_queue = Rest,
        processing = true
    }.

analyze_error_with_ai(#{id := ErrorId, data := ErrorData, context := Context}, Client) ->
    colored_logger:ocean(surface, io_lib:format("[AI_ERROR_PROCESSOR] 🔍 Starting AI analysis for error ~s", [ErrorId])),
    
    try
        % Create system prompt for error analysis
        SystemPrompt = create_system_prompt(),
        
        % Create user prompt with error details
        UserPrompt = create_error_analysis_prompt(ErrorData, Context),
        
        % Call GPT-4.1-mini for analysis
        Messages = [
            #{<<"role">> => <<"system">>, <<"content">> => SystemPrompt},
            #{<<"role">> => <<"user">>, <<"content">> => UserPrompt}
        ],
        
        % Use streaming for real-time updates
        case openai_chat:stream_chat(Client, Messages, #{
            <<"model">> => <<"gpt-4o-mini">>,
            <<"temperature">> => 0.1,
            <<"max_tokens">> => 1000,
            <<"stream">> => true
        }) of
            {ok, ResponseStream} ->
                Analysis = collect_streaming_response(ResponseStream),
                ProcessedAnalysis = process_ai_response(Analysis, ErrorData, Context),
                ?MODULE ! {analysis_complete, ErrorId, ProcessedAnalysis};
            {error, Reason} ->
                ?MODULE ! {analysis_error, ErrorId, Reason}
        end
    catch
        ErrorType:ErrorReason:Stacktrace ->
            colored_logger:fire(inferno, io_lib:format("[AI_ERROR_PROCESSOR] ❌ Exception in analysis: ~p:~p", [ErrorType, ErrorReason])),
            ?MODULE ! {analysis_error, ErrorId, {ErrorType, ErrorReason, Stacktrace}}
    end.

create_system_prompt() ->
    <<"You are an expert Erlang/OTP system debugger and WebSocket error analyst. 
Your task is to analyze errors and provide actionable insights for a distributed agent system.

Focus on:
1. Root cause analysis
2. Impact assessment 
3. Specific fix recommendations
4. Prevention strategies
5. System stability implications

Provide concise, technical responses in JSON format with these fields:
- severity: critical|high|medium|low
- category: websocket|process|network|resource|logic
- root_cause: detailed explanation
- impact: system impact description  
- fixes: array of specific actionable fixes
- prevention: prevention recommendations
- confidence: 0.0-1.0 confidence score">>.

create_error_analysis_prompt(ErrorData, Context) ->
    ErrorStr = format_error_for_analysis(ErrorData),
    ContextStr = format_context_for_analysis(Context),
    
    iolist_to_binary([
        "Analyze this Erlang/OTP system error:\n\n",
        "ERROR DATA:\n", ErrorStr, "\n\n",
        "CONTEXT:\n", ContextStr, "\n\n",
        "Provide detailed analysis in JSON format focusing on WebSocket resource exhaustion patterns and distributed system stability."
    ]).

format_error_for_analysis(ErrorData) when is_map(ErrorData) ->
    jsx:encode(ErrorData);
format_error_for_analysis(ErrorData) when is_binary(ErrorData) ->
    ErrorData;
format_error_for_analysis(ErrorData) ->
    iolist_to_binary(io_lib:format("~p", [ErrorData])).

format_context_for_analysis(Context) when is_binary(Context) ->
    Context;
format_context_for_analysis(Context) ->
    iolist_to_binary(io_lib:format("~p", [Context])).

collect_streaming_response(ResponseStream) ->
    collect_streaming_response(ResponseStream, <<>>).

collect_streaming_response(ResponseStream, Acc) ->
    receive
        {stream_token, Token} ->
            collect_streaming_response(ResponseStream, <<Acc/binary, Token/binary>>);
        {stream_complete, _Result} ->
            Acc;
        {stream_error, Error} ->
            throw({stream_error, Error})
    after ?ANALYSIS_TIMEOUT ->
        throw(timeout)
    end.

process_ai_response(RawResponse, ErrorData, Context) ->
    try
        % Try to parse as JSON first
        case jsx:decode(RawResponse, [return_maps]) of
            Analysis when is_map(Analysis) ->
                enhance_analysis(Analysis, ErrorData, Context);
            _ ->
                % Fallback to plain text analysis
                create_fallback_analysis(RawResponse, ErrorData, Context)
        end
    catch
        _:_ ->
            create_fallback_analysis(RawResponse, ErrorData, Context)
    end.

enhance_analysis(Analysis, ErrorData, Context) ->
    Analysis#{
        <<"timestamp">> => erlang:system_time(second),
        <<"error_data">> => format_error_for_analysis(ErrorData),
        <<"context">> => Context,
        <<"processor">> => <<"gpt-4o-mini">>,
        <<"version">> => <<"1.0">>
    }.

create_fallback_analysis(RawResponse, ErrorData, Context) ->
    #{
        <<"severity">> => <<"medium">>,
        <<"category">> => <<"unknown">>,
        <<"root_cause">> => RawResponse,
        <<"impact">> => <<"Analysis parsing failed, see raw response">>,
        <<"fixes">> => [<<"Review error manually">>, <<"Check logs for patterns">>],
        <<"prevention">> => [<<"Improve error handling">>, <<"Add monitoring">>],
        <<"confidence">> => 0.3,
        <<"timestamp">> => erlang:system_time(second),
        <<"error_data">> => format_error_for_analysis(ErrorData),
        <<"context">> => Context,
        <<"processor">> => <<"gpt-4o-mini">>,
        <<"version">> => <<"1.0">>,
        <<"raw_response">> => RawResponse
    }.

generate_error_id(ErrorData) ->
    Hash = crypto:hash(sha256, term_to_binary(ErrorData)),
    base64:encode(Hash).

calculate_priority(ErrorData, Context) ->
    % Higher priority for websocket and resource errors
    BaseScore = case Context of
        <<"websocket">> -> 100;
        <<"resource_exhaustion">> -> 150;
        <<"crash">> -> 200;
        _ -> 50
    end,
    
    % Adjust based on error content
    ContentScore = case is_map(ErrorData) andalso maps:get(<<"message">>, ErrorData, <<>>) of
        Msg when is_binary(Msg) ->
            case binary:match(Msg, [<<"timeout">>, <<"exhausted">>, <<"overload">>]) of
                nomatch -> 0;
                _ -> 50
            end;
        _ -> 0
    end,
    
    BaseScore + ContentScore.

insert_by_priority(Error = #{priority := Priority}, Queue) ->
    {Before, After} = lists:splitwith(
        fun(#{priority := P}) -> P >= Priority end, 
        Queue
    ),
    Before ++ [Error] ++ After.

maybe_cleanup_cache(Cache) when map_size(Cache) > ?MAX_CACHE_SIZE ->
    % Remove oldest 10% of entries
    Entries = maps:to_list(Cache),
    SortedEntries = lists:sort(
        fun({_, #{<<"timestamp">> := T1}}, {_, #{<<"timestamp">> := T2}}) ->
            T1 =< T2
        end, Entries
    ),
    KeepCount = round(map_size(Cache) * 0.9),
    KeptEntries = lists:nthtail(map_size(Cache) - KeepCount, SortedEntries),
    maps:from_list(KeptEntries);
maybe_cleanup_cache(Cache) ->
    Cache.

broadcast_analysis(ErrorId, Analysis) ->
    % Broadcast to error websocket handlers
    Message = #{
        type => <<"ai_analysis">>,
        error_id => ErrorId,
        analysis => Analysis,
        timestamp => erlang:system_time(second)
    },
    
    % Send to error tracking system
    case whereis(error_tracking_system) of
        undefined -> ok;
        Pid -> Pid ! {ai_analysis, ErrorId, Analysis}
    end,
    
    % Log for debugging
    colored_logger:complete(success, io_lib:format("[AI_ERROR_PROCESSOR] 📡 Broadcasted analysis for error ~s", [ErrorId])).