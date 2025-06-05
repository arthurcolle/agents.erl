-module(error_resilience).

%% API
-export([
    safe_call/3,
    safe_call/4,
    safe_cast/2,
    safe_apply/3,
    with_fallback/2,
    with_retry/3,
    with_circuit_breaker/3,
    protect_division/2,
    protect_memory_calc/2,
    auto_healing_call/3,
    auto_healing_call/4,
    register_error_handler/2,
    learn_from_success/3
]).

%% Circuit breaker state
-record(circuit_state, {
    failures = 0 :: integer(),
    last_failure :: undefined | erlang:timestamp(),
    state = closed :: closed | open | half_open
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Safe gen_server:call with automatic error handling
safe_call(Server, Request, Timeout) ->
    safe_call(Server, Request, Timeout, {error, unavailable}).

safe_call(Server, Request, Timeout, Default) ->
    try
        case whereis_safe(Server) of
            undefined ->
                error_logger:warning_msg("[RESILIENCE] Process ~p not found, returning default~n", [Server]),
                Default;
            Pid when is_pid(Pid) ->
                gen_server:call(Server, Request, Timeout)
        end
    catch
        exit:{noproc, _} ->
            error_logger:warning_msg("[RESILIENCE] Process ~p died during call, returning default~n", [Server]),
            Default;
        exit:{timeout, _} ->
            error_logger:warning_msg("[RESILIENCE] Call to ~p timed out, returning default~n", [Server]),
            Default;
        Type:Reason ->
            error_logger:error_msg("[RESILIENCE] Unexpected error calling ~p: ~p:~p~n", [Server, Type, Reason]),
            Default
    end.

%% Safe gen_server:cast
safe_cast(Server, Message) ->
    try
        case whereis_safe(Server) of
            undefined ->
                error_logger:warning_msg("[RESILIENCE] Process ~p not found for cast~n", [Server]),
                ok;
            Pid when is_pid(Pid) ->
                gen_server:cast(Server, Message)
        end
    catch
        _:Reason ->
            error_logger:warning_msg("[RESILIENCE] Failed to cast to ~p: ~p~n", [Server, Reason]),
            ok
    end.

%% Safe function application with error handling
safe_apply(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch
        error:undef ->
            error_logger:error_msg("[RESILIENCE] Function ~p:~p/~p undefined~n", 
                                 [Module, Function, length(Args)]),
            {error, function_undefined};
        Type:Reason ->
            error_logger:error_msg("[RESILIENCE] Error in ~p:~p: ~p:~p~n", 
                                 [Module, Function, Type, Reason]),
            {error, {Type, Reason}}
    end.

%% Execute function with fallback on error
with_fallback(Fun, Fallback) when is_function(Fun, 0), is_function(Fallback, 0) ->
    try
        Fun()
    catch
        _:_ -> Fallback()
    end;
with_fallback(Fun, FallbackValue) when is_function(Fun, 0) ->
    try
        Fun()
    catch
        _:_ -> FallbackValue
    end.

%% Execute function with retries
with_retry(Fun, MaxRetries, Delay) ->
    with_retry(Fun, MaxRetries, Delay, 0).

with_retry(Fun, MaxRetries, _Delay, Attempt) when Attempt >= MaxRetries ->
    error_logger:error_msg("[RESILIENCE] Max retries (~p) exceeded~n", [MaxRetries]),
    {error, max_retries_exceeded};
with_retry(Fun, MaxRetries, Delay, Attempt) ->
    try
        Fun()
    catch
        Type:Reason when Attempt < MaxRetries - 1 ->
            error_logger:warning_msg("[RESILIENCE] Attempt ~p failed: ~p:~p, retrying...~n", 
                                   [Attempt + 1, Type, Reason]),
            timer:sleep(Delay * (Attempt + 1)), % Exponential backoff
            with_retry(Fun, MaxRetries, Delay, Attempt + 1);
        Type:Reason ->
            error_logger:error_msg("[RESILIENCE] Final attempt failed: ~p:~p~n", [Type, Reason]),
            {error, {Type, Reason}}
    end.

%% Circuit breaker pattern
with_circuit_breaker(Name, Fun, Options) ->
    State = get_circuit_state(Name),
    case State#circuit_state.state of
        open ->
            case should_attempt_reset(State) of
                true ->
                    attempt_with_circuit_breaker(Name, Fun, Options, 
                                               State#circuit_state{state = half_open});
                false ->
                    {error, circuit_open}
            end;
        closed ->
            attempt_with_circuit_breaker(Name, Fun, Options, State);
        half_open ->
            attempt_with_circuit_breaker(Name, Fun, Options, State)
    end.

%% Protected division to prevent badarith
protect_division(Numerator, 0) ->
    error_logger:warning_msg("[RESILIENCE] Division by zero prevented~n"),
    0.0;
protect_division(Numerator, Denominator) when is_number(Numerator), is_number(Denominator) ->
    Numerator / Denominator;
protect_division(Numerator, Denominator) ->
    try
        Num = to_number(Numerator),
        Den = to_number(Denominator),
        protect_division(Num, Den)
    catch
        _:_ ->
            error_logger:error_msg("[RESILIENCE] Invalid division arguments: ~p / ~p~n", 
                                 [Numerator, Denominator]),
            0.0
    end.

%% Protected memory calculation
protect_memory_calc(Used, Total) when Total =:= 0 ->
    error_logger:warning_msg("[RESILIENCE] Total memory is 0, returning 0%~n"),
    0.0;
protect_memory_calc(Used, Total) when is_number(Used), is_number(Total), Total > 0 ->
    (Used / Total) * 100;
protect_memory_calc(Used, Total) ->
    try
        UsedNum = to_number(Used),
        TotalNum = to_number(Total),
        protect_memory_calc(UsedNum, TotalNum)
    catch
        _:_ ->
            error_logger:error_msg("[RESILIENCE] Invalid memory calculation arguments: ~p / ~p~n", 
                                 [Used, Total]),
            0.0
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

whereis_safe(Name) when is_atom(Name) ->
    whereis(Name);
whereis_safe(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> Pid;
        false -> undefined
    end;
whereis_safe({global, Name}) ->
    global:whereis_name(Name);
whereis_safe({via, Module, Name}) ->
    Module:whereis_name(Name);
whereis_safe(_) ->
    undefined.

get_circuit_state(Name) ->
    case get({circuit_breaker, Name}) of
        undefined -> #circuit_state{};
        State -> State
    end.

put_circuit_state(Name, State) ->
    put({circuit_breaker, Name}, State).

should_attempt_reset(#circuit_state{last_failure = undefined}) ->
    true;
should_attempt_reset(#circuit_state{last_failure = LastFailure}) ->
    %% Reset after 30 seconds
    timer:now_diff(os:timestamp(), LastFailure) > 30000000.

attempt_with_circuit_breaker(Name, Fun, Options, State) ->
    try
        Result = Fun(),
        %% Success - reset circuit
        put_circuit_state(Name, #circuit_state{state = closed}),
        Result
    catch
        Type:Reason ->
            NewState = handle_circuit_failure(State, Options),
            put_circuit_state(Name, NewState),
            error_logger:error_msg("[RESILIENCE] Circuit breaker ~p caught error: ~p:~p~n", 
                                 [Name, Type, Reason]),
            {error, {circuit_breaker, {Type, Reason}}}
    end.

handle_circuit_failure(State = #circuit_state{failures = Failures}, Options) ->
    MaxFailures = proplists:get_value(max_failures, Options, 5),
    NewFailures = Failures + 1,
    if
        NewFailures >= MaxFailures ->
            error_logger:warning_msg("[RESILIENCE] Circuit breaker opened after ~p failures~n", 
                                   [NewFailures]),
            State#circuit_state{
                failures = NewFailures,
                last_failure = os:timestamp(),
                state = open
            };
        true ->
            State#circuit_state{failures = NewFailures}
    end.

to_number(N) when is_number(N) -> N;
to_number(N) when is_binary(N) -> binary_to_number(N);
to_number(N) when is_list(N) -> list_to_number(N);
to_number(_) -> throw(not_a_number).

binary_to_number(B) ->
    try binary_to_integer(B)
    catch error:badarg ->
        try binary_to_float(B)
        catch error:badarg -> throw(not_a_number)
        end
    end.

list_to_number(L) ->
    try list_to_integer(L)
    catch error:badarg ->
        try list_to_float(L)
        catch error:badarg -> throw(not_a_number)
        end
    end.

%% Auto-healing call that learns from errors and applies fixes
auto_healing_call(Server, Request, Timeout) ->
    auto_healing_call(Server, Request, Timeout, {error, unavailable}).

auto_healing_call(Server, Request, Timeout, Default) ->
    try
        case whereis_safe(Server) of
            undefined ->
                %% Report error for learning
                auto_error_fixer:report_error(error_resilience, auto_healing_call, 
                                            {noproc, {server_not_found, Server}}),
                %% Try to apply learned fix
                case auto_error_fixer:apply_learned_fix(error_resilience, auto_healing_call) of
                    {ok, FixType} ->
                        error_logger:info_msg("[AUTO-HEAL] Applied fix type ~p for ~p~n", 
                                            [FixType, Server]),
                        %% Retry after fix
                        safe_call(Server, Request, Timeout, Default);
                    _ ->
                        Default
                end;
            Pid when is_pid(Pid) ->
                Result = gen_server:call(Server, Request, Timeout),
                %% Learn from successful calls
                learn_from_success(Server, Request, Result),
                Result
        end
    catch
        exit:{noproc, _} = Error ->
            auto_error_fixer:report_error(error_resilience, auto_healing_call, Error),
            handle_auto_heal_error(Server, Request, Timeout, Default, Error);
        exit:{timeout, _} = Error ->
            auto_error_fixer:report_error(error_resilience, auto_healing_call, Error),
            handle_auto_heal_error(Server, Request, Timeout, Default, Error);
        Type:Reason ->
            auto_error_fixer:report_error(error_resilience, auto_healing_call, {Type, Reason}),
            handle_auto_heal_error(Server, Request, Timeout, Default, {Type, Reason})
    end.

%% Register custom error handler for specific patterns
register_error_handler(Pattern, Handler) when is_function(Handler, 2) ->
    put({error_handler, Pattern}, Handler),
    ok.

%% Learn from successful operations
learn_from_success(Server, Request, Result) ->
    %% Store successful patterns for future reference
    SuccessKey = {success_pattern, Server, element(1, Request)},
    case get(SuccessKey) of
        undefined ->
            put(SuccessKey, {1, os:timestamp()});
        {Count, _LastTime} ->
            put(SuccessKey, {Count + 1, os:timestamp()})
    end,
    Result.

%% Handle errors with auto-healing
handle_auto_heal_error(Server, Request, Timeout, Default, Error) ->
    %% Check for custom error handlers
    case find_error_handler(Error) of
        {ok, Handler} ->
            try
                Handler(Error, {Server, Request, Timeout})
            catch
                _:_ -> Default
            end;
        error ->
            %% Apply automatic fixes based on error type
            case Error of
                {noproc, _} ->
                    maybe_restart_server(Server, Default);
                {timeout, _} ->
                    %% Increase timeout and retry
                    NewTimeout = min(Timeout * 2, 30000),
                    error_logger:info_msg("[AUTO-HEAL] Retrying with increased timeout ~p~n", 
                                        [NewTimeout]),
                    with_retry(fun() -> 
                        gen_server:call(Server, Request, NewTimeout)
                    end, 2, 500);
                {badarg, _} ->
                    %% Try to fix bad arguments
                    maybe_fix_arguments(Server, Request, Timeout, Default);
                _ ->
                    Default
            end
    end.

%% Find registered error handler for pattern
find_error_handler(Error) ->
    Handlers = [K || {error_handler, _} = K <- get()],
    find_matching_handler(Error, Handlers).

find_matching_handler(_Error, []) -> error;
find_matching_handler(Error, [{error_handler, Pattern} = Key | Rest]) ->
    case match_error_pattern(Error, Pattern) of
        true -> {ok, get(Key)};
        false -> find_matching_handler(Error, Rest)
    end.

match_error_pattern({Type, _}, Type) -> true;
match_error_pattern({Type, {SubType, _}}, {Type, SubType}) -> true;
match_error_pattern(_, '_') -> true;
match_error_pattern(_, _) -> false.

%% Try to restart a dead server
maybe_restart_server(Server, Default) when is_atom(Server) ->
    %% Check if we know how to start this server
    case get_server_start_spec(Server) of
        {ok, {Module, Function, Args}} ->
            error_logger:info_msg("[AUTO-HEAL] Attempting to restart ~p~n", [Server]),
            try
                apply(Module, Function, Args),
                timer:sleep(100),  %% Give it time to start
                Default
            catch
                _:_ -> Default
            end;
        error ->
            Default
    end;
maybe_restart_server(_Server, Default) ->
    Default.

%% Get server start specification
get_server_start_spec(mcp_manager) ->
    {ok, {mcp_manager, start_link, []}};
get_server_start_spec(agent_instance_manager) ->
    {ok, {agent_instance_manager, start_link, []}};
get_server_start_spec(_) ->
    error.

%% Try to fix bad arguments
maybe_fix_arguments(Server, Request, Timeout, Default) ->
    %% Analyze request and try common fixes
    FixedRequest = case Request of
        {Command, undefined} -> {Command, []};
        {Command, null} -> {Command, []};
        {Command, Args} when is_list(Args) -> 
            %% Ensure all args are valid
            {Command, [fix_arg(Arg) || Arg <- Args]};
        _ -> Request
    end,
    
    if FixedRequest =/= Request ->
        error_logger:info_msg("[AUTO-HEAL] Fixed arguments from ~p to ~p~n", 
                            [Request, FixedRequest]),
        safe_call(Server, FixedRequest, Timeout, Default);
    true ->
        Default
    end.

fix_arg(undefined) -> [];
fix_arg(null) -> [];
fix_arg(Arg) when is_binary(Arg) -> binary_to_list(Arg);
fix_arg(Arg) -> Arg.