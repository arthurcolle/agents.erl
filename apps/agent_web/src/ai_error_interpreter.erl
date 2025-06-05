-module(ai_error_interpreter).
-behaviour(gen_server).

%% API
-export([start_link/0,
         interpret_error/1,
         interpret_log_line/1,
         get_interpretation/1,
         get_recent_interpretations/0,
         subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
    openai_client :: pid(),
    interpretations = #{} :: map(),
    recent_logs = [] :: list(),
    subscribers = [] :: list(pid()),
    pattern_cache = #{} :: map()
}).

-record(error_interpretation, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    error_type :: atom(),
    original_text :: binary(),
    interpretation :: binary(),
    severity :: low | medium | high | critical,
    category :: binary(),
    tags :: list(binary()),
    suggestions :: list(binary()),
    related_errors :: list(binary()),
    confidence :: float(),
    context :: map()
}).

-define(INTERPRETATION_TABLE, error_interpretations).
-define(MAX_RECENT_LOGS, 1000).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

interpret_error(ErrorData) ->
    gen_server:cast(?MODULE, {interpret_error, ErrorData}).

interpret_log_line(LogLine) ->
    gen_server:cast(?MODULE, {interpret_log, LogLine}).

get_interpretation(Id) ->
    gen_server:call(?MODULE, {get_interpretation, Id}).

get_recent_interpretations() ->
    gen_server:call(?MODULE, get_recent).

subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for interpretations
    ets:new(?INTERPRETATION_TABLE, [named_table, public, {keypos, #error_interpretation.id}]),
    
    %% Get or start OpenAI client
    Client = case whereis(openai_chat) of
        undefined ->
            case openai_clients_sup:start_client(chat, #{}) of
                {ok, Pid} -> Pid;
                _ -> undefined
            end;
        Pid -> Pid
    end,
    
    %% Set up log monitoring - comment out for now as it may be causing issues
    %% error_logger:add_report_handler(error_log_monitor, self()),
    
    {ok, #state{openai_client = Client}}.

handle_call({get_interpretation, Id}, _From, State) ->
    case ets:lookup(?INTERPRETATION_TABLE, Id) of
        [Interp] -> {reply, {ok, format_interpretation(Interp)}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_recent, _From, State) ->
    Recent = lists:sublist(State#state.recent_logs, 50),
    Formatted = [format_interpretation(I) || I <- Recent],
    {reply, {ok, Formatted}, State};

handle_call({subscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subs]}};

handle_call({unsubscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({interpret_error, ErrorData}, State) ->
    spawn_link(fun() -> process_error_interpretation(ErrorData, State#state.openai_client) end),
    {noreply, State};

handle_cast({interpret_log, LogLine}, State) ->
    spawn_link(fun() -> process_log_interpretation(LogLine, State#state.openai_client) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({interpretation_complete, Interpretation}, State) ->
    %% Store interpretation
    ets:insert(?INTERPRETATION_TABLE, Interpretation),
    
    %% Update recent logs
    NewRecent = [Interpretation | lists:sublist(State#state.recent_logs, ?MAX_RECENT_LOGS - 1)],
    
    %% Notify subscribers
    notify_subscribers({new_interpretation, format_interpretation(Interpretation)}, State),
    
    {noreply, State#state{recent_logs = NewRecent}};

handle_info({error_report, _, Report}, State) ->
    %% Process error reports from error_logger
    interpret_error(Report),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subscribers = Subs} = State) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(error_log_monitor),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

process_error_interpretation(ErrorData, OpenAIClient) ->
    %% Extract error information
    ErrorType = extract_error_type(ErrorData),
    ErrorText = format_error_for_ai(ErrorData),
    Context = extract_context(ErrorData),
    
    %% Build AI prompt
    Prompt = build_error_interpretation_prompt(ErrorType, ErrorText, Context),
    
    Messages = [
        #{role => system, content => <<"You are an expert Erlang/OTP error interpreter. Analyze errors and provide clear, actionable interpretations with specific solutions. Focus on practical fixes and common causes."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            response_format => #{type => json_object}
        }),
        
        %% Parse AI response
        Interpretation = parse_ai_interpretation(Response, ErrorData),
        
        %% Send result back
        self() ! {interpretation_complete, Interpretation}
    catch
        Error:Reason ->
            error_logger:error_msg("AI interpretation failed: ~p:~p~n", [Error, Reason])
    end.

process_log_interpretation(LogLine, OpenAIClient) ->
    %% Quick pattern matching for known error types
    case quick_match_error_pattern(LogLine) of
        {ok, Pattern, Severity} ->
            %% Use cached interpretation for known patterns
            Interpretation = create_quick_interpretation(LogLine, Pattern, Severity),
            self() ! {interpretation_complete, Interpretation};
        nomatch ->
            %% Use AI for unknown patterns
            process_with_ai(LogLine, OpenAIClient)
    end.

process_with_ai(LogLine, OpenAIClient) ->
    Prompt = build_log_interpretation_prompt(LogLine),
    
    Messages = [
        #{role => system, content => <<"You are an expert at interpreting Erlang/OTP log messages. Provide clear explanations of what errors mean, why they occur, and how to fix them. Be concise but thorough."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            response_format => #{type => json_object}
        }),
        
        Interpretation = parse_log_interpretation(Response, LogLine),
        self() ! {interpretation_complete, Interpretation}
    catch
        Error:Reason ->
            error_logger:error_msg("Log interpretation failed: ~p:~p~n", [Error, Reason])
    end.

extract_error_type(ErrorData) when is_map(ErrorData) ->
    maps:get(error_type, ErrorData, unknown);
extract_error_type({error_report, _, {_, Type, _}}) ->
    Type;
extract_error_type(_) ->
    unknown.

format_error_for_ai(ErrorData) when is_map(ErrorData) ->
    iolist_to_binary(io_lib:format("~p", [ErrorData]));
format_error_for_ai(ErrorData) ->
    iolist_to_binary(io_lib:format("~p", [ErrorData])).

extract_context(ErrorData) when is_map(ErrorData) ->
    #{
        module => maps:get(module, ErrorData, unknown),
        function => maps:get(function, ErrorData, unknown),
        line => maps:get(line, ErrorData, 0),
        process => maps:get(registered_name, ErrorData, maps:get(pid, ErrorData, unknown))
    };
extract_context(_) ->
    #{}.

build_error_interpretation_prompt(ErrorType, ErrorText, Context) ->
    ContextStr = io_lib:format("~p", [Context]),
    iolist_to_binary([
        <<"Analyze this Erlang error and provide a detailed interpretation:\n\n">>,
        <<"Error Type: ">>, atom_to_binary(ErrorType, utf8), <<"\n">>,
        <<"Error Details: ">>, ErrorText, <<"\n">>,
        <<"Context: ">>, ContextStr, <<"\n\n">>,
        <<"Provide your analysis in this JSON format:\n">>,
        <<"{\n">>,
        <<"  \"interpretation\": \"Clear explanation of what this error means\",\n">>,
        <<"  \"severity\": \"low|medium|high|critical\",\n">>,
        <<"  \"category\": \"category of error (e.g., 'process crash', 'bad argument', 'network', etc.)\",\n">>,
        <<"  \"tags\": [\"relevant\", \"tags\"],\n">>,
        <<"  \"root_cause\": \"Most likely cause of this error\",\n">>,
        <<"  \"suggestions\": [\n">>,
        <<"    \"Specific action to fix this error\",\n">>,
        <<"    \"Another suggestion if applicable\"\n">>,
        <<"  ],\n">>,
        <<"  \"common_scenarios\": [\"When this typically happens\"],\n">>,
        <<"  \"prevention\": \"How to prevent this error in the future\",\n">>,
        <<"  \"confidence\": 0.0-1.0\n">>,
        <<"}\n">>
    ]).

build_log_interpretation_prompt(LogLine) ->
    iolist_to_binary([
        <<"Interpret this Erlang/OTP log line:\n\n">>,
        LogLine, <<"\n\n">>,
        <<"Provide interpretation in this JSON format:\n">>,
        <<"{\n">>,
        <<"  \"interpretation\": \"What this log message means\",\n">>,
        <<"  \"severity\": \"low|medium|high|critical\",\n">>,
        <<"  \"category\": \"type of message\",\n">>,
        <<"  \"tags\": [\"relevant\", \"tags\"],\n">>,
        <<"  \"is_error\": true/false,\n">>,
        <<"  \"action_required\": true/false,\n">>,
        <<"  \"suggestions\": [\"What to do about this\"],\n">>,
        <<"  \"related_components\": [\"affected parts of the system\"],\n">>,
        <<"  \"confidence\": 0.0-1.0\n">>,
        <<"}\n">>
    ]).

parse_ai_interpretation(Response, ErrorData) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    JsonData = jsx:decode(Content, [return_maps]),
    
    #error_interpretation{
        id = generate_interpretation_id(),
        timestamp = erlang:timestamp(),
        error_type = extract_error_type(ErrorData),
        original_text = format_error_for_ai(ErrorData),
        interpretation = maps:get(<<"interpretation">>, JsonData),
        severity = binary_to_atom(maps:get(<<"severity">>, JsonData, <<"medium">>)),
        category = maps:get(<<"category">>, JsonData, <<"unknown">>),
        tags = maps:get(<<"tags">>, JsonData, []),
        suggestions = maps:get(<<"suggestions">>, JsonData, []),
        related_errors = [],
        confidence = maps:get(<<"confidence">>, JsonData, 0.5),
        context = #{
            root_cause => maps:get(<<"root_cause">>, JsonData, <<"">>),
            common_scenarios => maps:get(<<"common_scenarios">>, JsonData, []),
            prevention => maps:get(<<"prevention">>, JsonData, <<"">>)
        }
    }.

parse_log_interpretation(Response, LogLine) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    JsonData = jsx:decode(Content, [return_maps]),
    
    #error_interpretation{
        id = generate_interpretation_id(),
        timestamp = erlang:timestamp(),
        error_type = case maps:get(<<"is_error">>, JsonData, false) of
            true -> error;
            false -> info
        end,
        original_text = LogLine,
        interpretation = maps:get(<<"interpretation">>, JsonData),
        severity = binary_to_atom(maps:get(<<"severity">>, JsonData, <<"low">>)),
        category = maps:get(<<"category">>, JsonData, <<"log">>),
        tags = maps:get(<<"tags">>, JsonData, []),
        suggestions = maps:get(<<"suggestions">>, JsonData, []),
        related_errors = [],
        confidence = maps:get(<<"confidence">>, JsonData, 0.5),
        context = #{
            action_required => maps:get(<<"action_required">>, JsonData, false),
            related_components => maps:get(<<"related_components">>, JsonData, [])
        }
    }.

quick_match_error_pattern(LogLine) ->
    Patterns = [
        {<<"badarg">>, <<"Argument validation error">>, high},
        {<<"badmatch">>, <<"Pattern matching failed">>, medium},
        {<<"function_clause">>, <<"No matching function clause">>, medium},
        {<<"case_clause">>, <<"No matching case clause">>, medium},
        {<<"timeout">>, <<"Operation timed out">>, medium},
        {<<"noproc">>, <<"Process not found">>, high},
        {<<"normal">>, <<"Normal termination">>, low},
        {<<"shutdown">>, <<"Controlled shutdown">>, low},
        {<<"killed">>, <<"Process was killed">>, high},
        {<<"noconnection">>, <<"No connection to node">>, high},
        {<<"system_limit">>, <<"System limit reached">>, critical},
        {<<"out of memory">>, <<"Memory exhausted">>, critical},
        {<<"port_limit">>, <<"Port limit reached">>, critical}
    ],
    
    case find_pattern(LogLine, Patterns) of
        {ok, Pattern, Severity} -> {ok, Pattern, Severity};
        nomatch -> nomatch
    end.

find_pattern(_LogLine, []) ->
    nomatch;
find_pattern(LogLine, [{Pattern, Description, Severity} | Rest]) ->
    case binary:match(LogLine, Pattern) of
        nomatch -> find_pattern(LogLine, Rest);
        _ -> {ok, Description, Severity}
    end.

create_quick_interpretation(LogLine, Pattern, Severity) ->
    #error_interpretation{
        id = generate_interpretation_id(),
        timestamp = erlang:timestamp(),
        error_type = error,
        original_text = LogLine,
        interpretation = Pattern,
        severity = Severity,
        category = <<"known_pattern">>,
        tags = [<<"quick_match">>],
        suggestions = get_pattern_suggestions(Pattern),
        related_errors = [],
        confidence = 0.9,
        context = #{}
    }.

get_pattern_suggestions(<<"Argument validation error">>) ->
    [<<"Check function arguments for correct types">>,
     <<"Verify data format matches expected structure">>,
     <<"Add input validation before function calls">>];
get_pattern_suggestions(<<"Pattern matching failed">>) ->
    [<<"Review pattern match expressions">>,
     <<"Add catch-all clause to handle unexpected values">>,
     <<"Log the actual value that failed to match">>];
get_pattern_suggestions(<<"Process not found">>) ->
    [<<"Ensure process is started before sending messages">>,
     <<"Use process monitoring to detect crashes">>,
     <<"Consider using registered names for critical processes">>];
get_pattern_suggestions(<<"Memory exhausted">>) ->
    [<<"Check for memory leaks in processes">>,
     <<"Reduce process heap size with hibernation">>,
     <<"Implement pagination for large data sets">>];
get_pattern_suggestions(_) ->
    [<<"Review error details for specific issues">>,
     <<"Check system logs for related errors">>].

format_interpretation(#error_interpretation{} = Interp) ->
    #{
        id => Interp#error_interpretation.id,
        timestamp => format_timestamp(Interp#error_interpretation.timestamp),
        error_type => Interp#error_interpretation.error_type,
        original_text => Interp#error_interpretation.original_text,
        interpretation => Interp#error_interpretation.interpretation,
        severity => Interp#error_interpretation.severity,
        category => Interp#error_interpretation.category,
        tags => Interp#error_interpretation.tags,
        suggestions => Interp#error_interpretation.suggestions,
        related_errors => Interp#error_interpretation.related_errors,
        confidence => Interp#error_interpretation.confidence,
        context => Interp#error_interpretation.context
    }.

format_timestamp({MegaSec, Sec, MicroSec}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:now_to_datetime({MegaSec, Sec, MicroSec}),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                      [Year, Month, Day, Hour, Min, Sec])
    ).

generate_interpretation_id() ->
    list_to_binary(io_lib:format("interp_~p_~p", 
                                 [erlang:phash2(make_ref()), 
                                  erlang:system_time(microsecond)])).

notify_subscribers(Event, #state{subscribers = Subs}) ->
    [Pid ! {error_interpretation_event, Event} || Pid <- Subs].