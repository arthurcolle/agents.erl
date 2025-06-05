-module(ai_timeout_analyzer).
-behaviour(gen_server).

%% API
-export([start_link/0,
         analyze_timeout/2,
         analyze_significant_error/2,
         enable_periodic_analysis/1,
         disable_periodic_analysis/0,
         get_analysis_report/0,
         get_timeout_patterns/0,
         subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    ai_interpreter :: pid(),
    openai_client :: pid(),
    analysis_interval :: integer(),
    analysis_timer :: reference() | undefined,
    timeout_patterns = [] :: list(),
    error_buffer = [] :: list(),
    recent_analyses = [] :: list(),
    subscribers = [] :: list(pid()),
    stats = #{} :: map()
}).

-record(timeout_analysis, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    timeout_type :: gen_server_timeout | tcp_timeout | http_timeout | custom,
    duration_ms :: integer(),
    process_info :: map(),
    stack_trace :: list(),
    ai_interpretation :: binary(),
    severity :: low | medium | high | critical,
    root_cause :: binary(),
    prevention_tips :: list(binary()),
    related_timeouts :: list(binary())
}).

-define(DEFAULT_ANALYSIS_INTERVAL, 300000). % 5 minutes
-define(MAX_ERROR_BUFFER, 1000).
-define(TIMEOUT_PATTERN_TABLE, timeout_patterns).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze_timeout(TimeoutInfo, Context) ->
    gen_server:cast(?MODULE, {analyze_timeout, TimeoutInfo, Context}).

analyze_significant_error(ErrorInfo, Context) ->
    gen_server:cast(?MODULE, {analyze_error, ErrorInfo, Context}).

enable_periodic_analysis(IntervalMs) ->
    gen_server:call(?MODULE, {enable_periodic, IntervalMs}).

disable_periodic_analysis() ->
    gen_server:call(?MODULE, disable_periodic).

get_analysis_report() ->
    gen_server:call(?MODULE, get_report).

get_timeout_patterns() ->
    gen_server:call(?MODULE, get_patterns).

subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for timeout patterns
    ets:new(?TIMEOUT_PATTERN_TABLE, [named_table, public, set]),
    
    %% Initialize known timeout patterns
    init_timeout_patterns(),
    
    %% Get or start OpenAI client
    OpenAIClient = case whereis(openai_chat) of
        undefined ->
            case openai_clients_sup:start_client(chat, #{}) of
                {ok, ClientPid} -> ClientPid;
                _ -> undefined
            end;
        ChatPid -> ChatPid
    end,
    
    %% Link to AI interpreter if available
    AIInterpreter = case whereis(ai_error_interpreter) of
        undefined -> undefined;
        InterpreterPid -> InterpreterPid
    end,
    
    %% Set up log handler for timeout detection
    logger:add_handler(timeout_analyzer_handler, 
                      ?MODULE, 
                      #{level => warning}),
    
    {ok, #state{
        ai_interpreter = AIInterpreter,
        openai_client = OpenAIClient,
        analysis_interval = ?DEFAULT_ANALYSIS_INTERVAL,
        stats = init_stats()
    }}.

handle_call({enable_periodic, IntervalMs}, _From, State) ->
    %% Cancel existing timer if any
    NewState = cancel_timer(State),
    
    %% Start new timer
    TimerRef = erlang:send_after(IntervalMs, self(), periodic_analysis),
    
    {reply, ok, NewState#state{
        analysis_interval = IntervalMs,
        analysis_timer = TimerRef
    }};

handle_call(disable_periodic, _From, State) ->
    NewState = cancel_timer(State),
    {reply, ok, NewState#state{analysis_timer = undefined}};

handle_call(get_report, _From, State) ->
    Report = generate_analysis_report(State),
    {reply, {ok, Report}, State};

handle_call(get_patterns, _From, State) ->
    Patterns = ets:tab2list(?TIMEOUT_PATTERN_TABLE),
    {reply, {ok, Patterns}, State};

handle_call({subscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subs]}};

handle_call({unsubscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({analyze_timeout, TimeoutInfo, Context}, State) ->
    spawn_link(fun() -> 
        process_timeout_analysis(TimeoutInfo, Context, State#state.openai_client)
    end),
    NewStats = update_stats(timeout, State#state.stats),
    {noreply, State#state{stats = NewStats}};

handle_cast({analyze_error, ErrorInfo, Context}, State) ->
    %% Buffer significant errors for batch analysis
    NewBuffer = add_to_buffer(ErrorInfo, Context, State#state.error_buffer),
    
    %% Trigger immediate analysis if buffer is getting full
    if
        length(NewBuffer) > 50 ->
            self() ! analyze_buffer;
        true ->
            ok
    end,
    
    NewStats = update_stats(error, State#state.stats),
    {noreply, State#state{error_buffer = NewBuffer, stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(periodic_analysis, State) ->
    %% Perform periodic analysis
    spawn_link(fun() -> perform_periodic_analysis(State) end),
    
    %% Schedule next analysis
    TimerRef = erlang:send_after(State#state.analysis_interval, self(), periodic_analysis),
    
    {noreply, State#state{analysis_timer = TimerRef}};

handle_info(analyze_buffer, State) ->
    %% Analyze buffered errors
    spawn_link(fun() -> 
        analyze_error_buffer(State#state.error_buffer, State#state.openai_client)
    end),
    
    {noreply, State#state{error_buffer = []}};

handle_info({analysis_complete, Analysis}, State) ->
    %% Store analysis result
    NewRecent = store_analysis(Analysis, State#state.recent_analyses),
    
    %% Update patterns if it's a timeout
    case Analysis of
        #timeout_analysis{} -> update_timeout_patterns(Analysis);
        _ -> ok
    end,
    
    %% Notify subscribers
    notify_subscribers({new_analysis, format_analysis(Analysis)}, State),
    
    {noreply, State#state{recent_analyses = NewRecent}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subscribers = Subs} = State) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_info({log, #{level := Level, msg := Msg, meta := Meta}}, State) ->
    %% Check for timeout patterns in logs
    case detect_timeout_in_log(Level, Msg, Meta) of
        {ok, TimeoutInfo} ->
            analyze_timeout(TimeoutInfo, Meta);
        nomatch ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:remove_handler(timeout_analyzer_handler),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_timeout_patterns() ->
    Patterns = [
        {gen_server_timeout, <<"gen_server call timeout">>, 
         #{indicators => [<<"** gen_server .* terminated">>, <<"timeout">>, <<"call">>]}},
        {tcp_timeout, <<"TCP connection timeout">>,
         #{indicators => [<<"tcp">>, <<"timeout">>, <<"connection">>]}},
        {http_timeout, <<"HTTP request timeout">>,
         #{indicators => [<<"http">>, <<"timeout">>, <<"request">>]}},
        {db_timeout, <<"Database operation timeout">>,
         #{indicators => [<<"database">>, <<"timeout">>, <<"query">>]}},
        {rpc_timeout, <<"RPC call timeout">>,
         #{indicators => [<<"rpc">>, <<"timeout">>, <<"call">>]}}
    ],
    [ets:insert(?TIMEOUT_PATTERN_TABLE, P) || P <- Patterns].

process_timeout_analysis(TimeoutInfo, Context, OpenAIClient) ->
    %% Extract timeout details
    TimeoutType = detect_timeout_type(TimeoutInfo),
    Duration = extract_duration(TimeoutInfo),
    ProcessInfo = extract_process_info(Context),
    StackTrace = extract_stack_trace(TimeoutInfo),
    
    %% Build comprehensive prompt
    Prompt = build_timeout_prompt(TimeoutType, Duration, ProcessInfo, StackTrace),
    
    Messages = [
        #{role => system, content => <<"You are an expert in distributed systems and Erlang/OTP timeout analysis. Analyze timeout issues and provide actionable solutions focusing on root causes and prevention strategies."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            response_format => #{type => json_object}
        }),
        
        %% Parse and create analysis
        Analysis = parse_timeout_analysis(Response, TimeoutInfo, Context),
        
        %% Send result back
        self() ! {analysis_complete, Analysis}
    catch
        Error:Reason ->
            ?LOG_ERROR("Timeout analysis failed: ~p:~p", [Error, Reason])
    end.

detect_timeout_type(TimeoutInfo) when is_map(TimeoutInfo) ->
    case maps:get(type, TimeoutInfo, undefined) of
        undefined -> analyze_timeout_pattern(TimeoutInfo);
        Type -> Type
    end;
detect_timeout_type(_) ->
    custom.

analyze_timeout_pattern(Info) ->
    %% Check against known patterns
    Patterns = ets:tab2list(?TIMEOUT_PATTERN_TABLE),
    case find_matching_pattern(Info, Patterns) of
        {ok, Type} -> Type;
        nomatch -> custom
    end.

find_matching_pattern(_Info, []) ->
    nomatch;
find_matching_pattern(Info, [{Type, _Desc, #{indicators := Indicators}} | Rest]) ->
    InfoStr = format_for_matching(Info),
    case all_indicators_match(InfoStr, Indicators) of
        true -> {ok, Type};
        false -> find_matching_pattern(Info, Rest)
    end.

all_indicators_match(InfoStr, Indicators) ->
    lists:all(fun(Indicator) ->
        case binary:match(InfoStr, Indicator) of
            nomatch -> false;
            _ -> true
        end
    end, Indicators).

format_for_matching(Info) when is_map(Info) ->
    iolist_to_binary(io_lib:format("~p", [Info]));
format_for_matching(Info) ->
    iolist_to_binary(io_lib:format("~p", [Info])).

extract_duration(TimeoutInfo) when is_map(TimeoutInfo) ->
    maps:get(duration_ms, TimeoutInfo, maps:get(timeout, TimeoutInfo, 5000));
extract_duration(_) ->
    unknown.

extract_process_info(Context) when is_map(Context) ->
    #{
        pid => maps:get(pid, Context, undefined),
        registered_name => maps:get(registered_name, Context, undefined),
        module => maps:get(mfa, Context, {undefined, undefined, undefined}),
        initial_call => maps:get(initial_call, Context, undefined),
        message_queue_len => maps:get(message_queue_len, Context, 0),
        dictionary => maps:get(dictionary, Context, [])
    };
extract_process_info(_) ->
    #{}.

extract_stack_trace(TimeoutInfo) when is_map(TimeoutInfo) ->
    maps:get(stacktrace, TimeoutInfo, maps:get(stack, TimeoutInfo, []));
extract_stack_trace(_) ->
    [].

build_timeout_prompt(TimeoutType, Duration, ProcessInfo, StackTrace) ->
    iolist_to_binary([
        <<"Analyze this timeout issue in an Erlang/OTP system:\n\n">>,
        <<"Timeout Type: ">>, atom_to_binary(TimeoutType, utf8), <<"\n">>,
        <<"Duration: ">>, format_duration(Duration), <<"\n">>,
        <<"Process Info: ">>, format_process_info(ProcessInfo), <<"\n">>,
        <<"Stack Trace: ">>, format_stack_trace(StackTrace), <<"\n\n">>,
        <<"Provide detailed analysis in this JSON format:\n">>,
        <<"{\n">>,
        <<"  \"interpretation\": \"Clear explanation of why this timeout occurred\",\n">>,
        <<"  \"severity\": \"low|medium|high|critical\",\n">>,
        <<"  \"root_cause\": \"Most likely root cause of the timeout\",\n">>,
        <<"  \"contributing_factors\": [\n">>,
        <<"    \"Factor that may have contributed to the timeout\"\n">>,
        <<"  ],\n">>,
        <<"  \"immediate_impact\": \"What happens when this timeout occurs\",\n">>,
        <<"  \"cascade_effects\": [\"Potential downstream effects\"],\n">>,
        <<"  \"prevention_tips\": [\n">>,
        <<"    \"Specific action to prevent this timeout\",\n">>,
        <<"    \"Configuration change or code improvement\"\n">>,
        <<"  ],\n">>,
        <<"  \"tuning_suggestions\": {\n">>,
        <<"    \"timeout_value\": \"Recommended timeout value and why\",\n">>,
        <<"    \"configuration\": [\"Config changes to consider\"]\n">>,
        <<"  },\n">>,
        <<"  \"monitoring_advice\": \"What to monitor to catch this early\",\n">>,
        <<"  \"confidence\": 0.0-1.0\n">>,
        <<"}\n">>
    ]).

format_duration(unknown) -> <<"unknown">>;
format_duration(Ms) when is_integer(Ms) ->
    iolist_to_binary(io_lib:format("~p ms", [Ms]));
format_duration(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

format_process_info(Info) ->
    iolist_to_binary(io_lib:format("~p", [Info])).

format_stack_trace([]) -> <<"No stack trace available">>;
format_stack_trace(Stack) ->
    iolist_to_binary(io_lib:format("~p", [Stack])).

parse_timeout_analysis(Response, TimeoutInfo, Context) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    JsonData = jsx:decode(Content, [return_maps]),
    
    #timeout_analysis{
        id = generate_analysis_id(),
        timestamp = erlang:timestamp(),
        timeout_type = detect_timeout_type(TimeoutInfo),
        duration_ms = extract_duration(TimeoutInfo),
        process_info = extract_process_info(Context),
        stack_trace = extract_stack_trace(TimeoutInfo),
        ai_interpretation = maps:get(<<"interpretation">>, JsonData),
        severity = binary_to_atom(maps:get(<<"severity">>, JsonData, <<"medium">>)),
        root_cause = maps:get(<<"root_cause">>, JsonData),
        prevention_tips = maps:get(<<"prevention_tips">>, JsonData, []),
        related_timeouts = []
    }.

perform_periodic_analysis(State) ->
    %% Gather system metrics
    SystemMetrics = gather_system_metrics(),
    
    %% Analyze recent errors and timeouts
    RecentIssues = gather_recent_issues(State),
    
    %% Create comprehensive analysis prompt
    Prompt = build_periodic_analysis_prompt(SystemMetrics, RecentIssues, State#state.stats),
    
    Messages = [
        #{role => system, content => <<"You are a system reliability expert. Analyze system logs and metrics to identify patterns, potential issues, and optimization opportunities. Focus on actionable insights."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(State#state.openai_client, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            max_tokens => 1000
        }),
        
        %% Broadcast analysis to subscribers
        Analysis = extract_periodic_analysis(Response),
        notify_subscribers({periodic_analysis, Analysis}, State)
    catch
        Error:Reason ->
            ?LOG_ERROR("Periodic analysis failed: ~p:~p", [Error, Reason])
    end.

gather_system_metrics() ->
    #{
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        io => erlang:statistics(io),
        reductions => erlang:statistics(reductions),
        scheduler_usage => scheduler:utilization(1)
    }.

gather_recent_issues(State) ->
    %% Get recent timeout analyses
    RecentTimeouts = lists:filter(fun(A) ->
        case A of
            #timeout_analysis{} -> true;
            _ -> false
        end
    end, State#state.recent_analyses),
    
    %% Get error buffer
    #{
        timeouts => RecentTimeouts,
        errors => State#state.error_buffer,
        stats => State#state.stats
    }.

build_periodic_analysis_prompt(Metrics, Issues, Stats) ->
    iolist_to_binary([
        <<"Analyze the system health based on the following data:\n\n">>,
        <<"System Metrics:\n">>, format_metrics(Metrics), <<"\n\n">>,
        <<"Recent Issues Summary:\n">>, format_issues_summary(Issues), <<"\n\n">>,
        <<"Statistics:\n">>, format_stats(Stats), <<"\n\n">>,
        <<"Provide a concise analysis covering:\n">>,
        <<"1. Overall system health assessment\n">>,
        <<"2. Identified patterns or trends\n">>,
        <<"3. Potential issues developing\n">>,
        <<"4. Recommended actions (prioritized)\n">>,
        <<"5. Performance optimization opportunities\n">>
    ]).

format_metrics(Metrics) ->
    iolist_to_binary(io_lib:format("~p", [Metrics])).

format_issues_summary(Issues) ->
    TimeoutCount = length(maps:get(timeouts, Issues, [])),
    ErrorCount = length(maps:get(errors, Issues, [])),
    iolist_to_binary(io_lib:format(
        "Timeouts: ~p, Errors: ~p~nStats: ~p",
        [TimeoutCount, ErrorCount, maps:get(stats, Issues, #{})]
    )).

format_stats(Stats) ->
    iolist_to_binary(io_lib:format("~p", [Stats])).

analyze_error_buffer(Buffer, OpenAIClient) when length(Buffer) > 0 ->
    %% Group errors by type
    GroupedErrors = group_errors_by_type(Buffer),
    
    %% Create analysis prompt for error patterns
    Prompt = build_error_pattern_prompt(GroupedErrors),
    
    Messages = [
        #{role => system, content => <<"You are an expert at identifying error patterns and root causes. Analyze grouped errors to find common issues and systematic problems."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            response_format => #{type => json_object}
        }),
        
        %% Process and store analysis
        Analysis = parse_error_pattern_analysis(Response, GroupedErrors),
        self() ! {analysis_complete, Analysis}
    catch
        Error:Reason ->
            ?LOG_ERROR("Error buffer analysis failed: ~p:~p", [Error, Reason])
    end;
analyze_error_buffer(_, _) ->
    ok.

group_errors_by_type(Buffer) ->
    lists:foldl(fun({Error, _Context}, Acc) ->
        Type = classify_error(Error),
        maps:update_with(Type, fun(L) -> [Error | L] end, [Error], Acc)
    end, #{}, Buffer).

classify_error(Error) when is_map(Error) ->
    maps:get(type, Error, maps:get(error_type, Error, unknown));
classify_error({error, Type, _}) ->
    Type;
classify_error(_) ->
    unknown.

build_error_pattern_prompt(GroupedErrors) ->
    ErrorSummary = maps:fold(fun(Type, Errors, Acc) ->
        [io_lib:format("~p errors of type ~p~n", [length(Errors), Type]) | Acc]
    end, [], GroupedErrors),
    
    iolist_to_binary([
        <<"Analyze these grouped errors for patterns:\n\n">>,
        ErrorSummary,
        <<"\n\nProvide analysis in JSON format:\n">>,
        <<"{\n">>,
        <<"  \"patterns_found\": [\n">>,
        <<"    {\n">>,
        <<"      \"pattern\": \"Description of the pattern\",\n">>,
        <<"      \"frequency\": \"How often it occurs\",\n">>,
        <<"      \"impact\": \"Impact on system\",\n">>,
        <<"      \"root_cause\": \"Likely root cause\"\n">>,
        <<"    }\n">>,
        <<"  ],\n">>,
        <<"  \"systemic_issues\": [\"Issues affecting multiple components\"],\n">>,
        <<"  \"recommendations\": [\n">>,
        <<"    {\n">>,
        <<"      \"priority\": \"high|medium|low\",\n">>,
        <<"      \"action\": \"Specific action to take\",\n">>,
        <<"      \"expected_impact\": \"What this will improve\"\n">>,
        <<"    }\n">>,
        <<"  ],\n">>,
        <<"  \"monitoring_gaps\": [\"What should be monitored but isn't\"]\n">>,
        <<"}\n">>
    ]).

parse_error_pattern_analysis(Response, _GroupedErrors) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    jsx:decode(Content, [return_maps]).

update_timeout_patterns(#timeout_analysis{timeout_type = Type, root_cause = Cause}) ->
    %% Update pattern statistics
    Key = {Type, pattern_stats},
    case ets:lookup(?TIMEOUT_PATTERN_TABLE, Key) of
        [] ->
            ets:insert(?TIMEOUT_PATTERN_TABLE, {Key, #{count => 1, causes => [Cause]}});
        [{Key, Stats}] ->
            NewStats = Stats#{
                count => maps:get(count, Stats, 0) + 1,
                causes => [Cause | lists:sublist(maps:get(causes, Stats, []), 9)]
            },
            ets:insert(?TIMEOUT_PATTERN_TABLE, {Key, NewStats})
    end.

detect_timeout_in_log(Level, Msg, Meta) when Level >= warning ->
    %% Look for timeout indicators in log messages
    MsgStr = format_log_msg(Msg),
    case contains_timeout_indicator(MsgStr) of
        true ->
            {ok, #{
                type => detected_from_log,
                level => Level,
                message => MsgStr,
                meta => Meta
            }};
        false ->
            nomatch
    end;
detect_timeout_in_log(_, _, _) ->
    nomatch.

format_log_msg({string, Str}) -> iolist_to_binary(Str);
format_log_msg({report, Report}) -> iolist_to_binary(io_lib:format("~p", [Report]));
format_log_msg(Msg) -> iolist_to_binary(io_lib:format("~p", [Msg])).

contains_timeout_indicator(MsgStr) ->
    Indicators = [<<"timeout">>, <<"timed out">>, <<"deadline exceeded">>, 
                  <<"took too long">>, <<"exceeded limit">>],
    lists:any(fun(Indicator) ->
        case binary:match(MsgStr, Indicator) of
            nomatch -> false;
            _ -> true
        end
    end, Indicators).

add_to_buffer(ErrorInfo, Context, Buffer) ->
    NewBuffer = [{ErrorInfo, Context} | Buffer],
    lists:sublist(NewBuffer, ?MAX_ERROR_BUFFER).

store_analysis(Analysis, Recent) ->
    [Analysis | lists:sublist(Recent, 99)].

format_analysis(#timeout_analysis{} = A) ->
    #{
        type => timeout_analysis,
        id => A#timeout_analysis.id,
        timestamp => A#timeout_analysis.timestamp,
        timeout_type => A#timeout_analysis.timeout_type,
        duration_ms => A#timeout_analysis.duration_ms,
        interpretation => A#timeout_analysis.ai_interpretation,
        severity => A#timeout_analysis.severity,
        root_cause => A#timeout_analysis.root_cause,
        prevention_tips => A#timeout_analysis.prevention_tips
    };
format_analysis(Other) ->
    Other.

generate_analysis_report(State) ->
    #{
        stats => State#state.stats,
        recent_analyses => lists:sublist(State#state.recent_analyses, 20),
        timeout_patterns => ets:tab2list(?TIMEOUT_PATTERN_TABLE),
        buffer_size => length(State#state.error_buffer),
        analysis_interval => State#state.analysis_interval,
        periodic_enabled => State#state.analysis_timer =/= undefined
    }.

init_stats() ->
    #{
        timeouts_analyzed => 0,
        errors_analyzed => 0,
        periodic_analyses => 0,
        start_time => erlang:system_time(second)
    }.

update_stats(Type, Stats) ->
    Key = case Type of
        timeout -> timeouts_analyzed;
        error -> errors_analyzed;
        periodic -> periodic_analyses
    end,
    maps:update_with(Key, fun(V) -> V + 1 end, 0, Stats).

cancel_timer(#state{analysis_timer = undefined} = State) ->
    State;
cancel_timer(#state{analysis_timer = Ref} = State) ->
    erlang:cancel_timer(Ref),
    State#state{analysis_timer = undefined}.

generate_analysis_id() ->
    list_to_binary(io_lib:format("analysis_~p_~p", 
                                 [erlang:phash2(make_ref()), 
                                  erlang:system_time(microsecond)])).

notify_subscribers(Event, #state{subscribers = Subs}) ->
    [Pid ! {timeout_analysis_event, Event} || Pid <- Subs].

extract_periodic_analysis(Response) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    #{
        timestamp => erlang:system_time(second),
        analysis => Content,
        type => periodic_system_analysis
    }.