%%%-------------------------------------------------------------------
%%% @doc
%%% Test Helpers
%%% Common utilities and helpers for test modules
%%% @end
%%%-------------------------------------------------------------------
-module(test_helpers).

-export([
    % Setup and teardown
    setup_test_env/0,
    cleanup_test_env/0,
    with_test_env/1,
    
    % Agent helpers
    create_test_agent/1,
    create_test_agent/2,
    create_test_agents/2,
    stop_test_agents/1,
    
    % Message helpers
    send_and_wait/3,
    send_and_wait/4,
    expect_message/1,
    expect_message/2,
    drain_messages/0,
    
    % Tool helpers
    register_test_tool/3,
    create_echo_tool/1,
    create_transform_tool/2,
    
    % Timing helpers
    measure_time/1,
    repeat_timed/2,
    benchmark/3,
    
    % Assertion helpers
    assert_eventually/2,
    assert_eventually/3,
    assert_within/3,
    
    % Data generators
    random_id/0,
    random_id/1,
    random_capability/0,
    random_capabilities/1,
    random_message/0,
    random_binary/1,
    
    % Concurrent test helpers
    run_concurrent/2,
    run_concurrent/3,
    parallel_map/2,
    
    % State management
    create_test_state/0,
    get_test_state/2,
    set_test_state/3,
    update_test_state/3,
    delete_test_state/1
]).

-define(DEFAULT_TIMEOUT, 5000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup_test_env() ->
    % Ensure applications are started
    {ok, _} = application:ensure_all_started(myapp),
    
    % Clear any existing state
    agent_registry:clear_all(),
    agent_discovery:clear_all(),
    agent_tools:clear_all(),
    
    % Create test state storage
    create_test_state(),
    
    ok.

cleanup_test_env() ->
    % Stop all test agents
    AllAgents = agent_registry:list_agents(),
    lists:foreach(fun(Id) ->
        catch agent:stop_agent(Id)
    end, AllAgents),
    
    % Clear all state
    agent_registry:clear_all(),
    agent_discovery:clear_all(),
    agent_tools:clear_all(),
    
    % Clean up test state
    delete_test_state(test_state),
    
    ok.

with_test_env(Fun) ->
    setup_test_env(),
    try
        Fun()
    after
        cleanup_test_env()
    end.

%%%===================================================================
%%% Agent Helpers
%%%===================================================================

create_test_agent(Id) ->
    create_test_agent(Id, #{}).

create_test_agent(Id, Options) ->
    DefaultSpec = #{
        id => Id,
        name => <<"Test Agent ", Id/binary>>,
        capabilities => maps:get(capabilities, Options, [<<"test">>]),
        tools => maps:get(tools, Options, []),
        handlers => maps:get(handlers, Options, #{})
    },
    
    Spec = maps:merge(DefaultSpec, Options),
    {ok, Pid} = agent:start_agent(Spec),
    {ok, Id, Pid}.

create_test_agents(Prefix, Count) ->
    lists:map(fun(N) ->
        Id = <<Prefix/binary, "_", (integer_to_binary(N))/binary>>,
        {ok, Id, _Pid} = create_test_agent(Id),
        Id
    end, lists:seq(1, Count)).

stop_test_agents(AgentIds) ->
    lists:foreach(fun(Id) ->
        catch agent:stop_agent(Id)
    end, AgentIds).

%%%===================================================================
%%% Message Helpers
%%%===================================================================

send_and_wait(From, To, Message) ->
    send_and_wait(From, To, Message, ?DEFAULT_TIMEOUT).

send_and_wait(From, To, Message, Timeout) ->
    Ref = make_ref(),
    Parent = self(),
    
    % Register temporary handler
    TempId = random_id(<<"temp_receiver_">>),
    agent_messenger:register_handler(TempId, fun(Msg) ->
        Parent ! {Ref, Msg}
    end),
    
    % Send message
    {ok, MsgId} = agent_messenger:send_message(From, To, Message),
    
    % Wait for response
    receive
        {Ref, ReceivedMsg} ->
            {ok, MsgId, ReceivedMsg}
    after Timeout ->
        {error, timeout}
    end.

expect_message(Pattern) ->
    expect_message(Pattern, ?DEFAULT_TIMEOUT).

expect_message(Pattern, Timeout) ->
    receive
        Pattern -> ok
    after Timeout ->
        error({timeout, {expected, Pattern}})
    end.

drain_messages() ->
    receive
        _ -> drain_messages()
    after 0 ->
        ok
    end.

%%%===================================================================
%%% Tool Helpers
%%%===================================================================

register_test_tool(Name, Parameters, Handler) ->
    ToolSpec = #{
        name => Name,
        description => <<"Test tool">>,
        parameters => Parameters,
        handler => Handler
    },
    agent_tools:register_tool(ToolSpec).

create_echo_tool(Name) ->
    #{
        name => Name,
        description => <<"Echoes input">>,
        parameters => [#{name => <<"input">>, type => any, required => true}],
        handler => fun(#{<<"input">> := Input}) -> {ok, Input} end
    }.

create_transform_tool(Name, TransformFun) ->
    #{
        name => Name,
        description => <<"Transforms input">>,
        parameters => [#{name => <<"input">>, type => any, required => true}],
        handler => fun(#{<<"input">> := Input}) -> 
            {ok, TransformFun(Input)} 
        end
    }.

%%%===================================================================
%%% Timing Helpers
%%%===================================================================

measure_time(Fun) ->
    {Time, Result} = timer:tc(Fun),
    {Time, Result}.

repeat_timed(N, Fun) ->
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(Fun),
        Time
    end, lists:seq(1, N)),
    
    Avg = lists:sum(Times) / N,
    Min = lists:min(Times),
    Max = lists:max(Times),
    
    #{
        count => N,
        total => lists:sum(Times),
        avg => Avg,
        min => Min,
        max => Max,
        times => Times
    }.

benchmark(Name, Iterations, Fun) ->
    io:format("Benchmarking ~s (~p iterations)...~n", [Name, Iterations]),
    
    % Warmup
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, min(100, Iterations div 10))),
    
    % Actual benchmark
    StartTime = erlang:monotonic_time(),
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, Iterations)),
    EndTime = erlang:monotonic_time(),
    
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, microsecond),
    Rate = Iterations * 1000000 / Duration,
    
    io:format("  Total time: ~.2f ms~n", [Duration / 1000]),
    io:format("  Rate: ~.2f ops/sec~n", [Rate]),
    io:format("  Avg time: ~.2f μs/op~n", [Duration / Iterations]),
    
    #{
        name => Name,
        iterations => Iterations,
        duration => Duration,
        rate => Rate,
        avg_time => Duration / Iterations
    }.

%%%===================================================================
%%% Assertion Helpers
%%%===================================================================

assert_eventually(Condition, Timeout) ->
    assert_eventually(Condition, Timeout, 100).

assert_eventually(Condition, Timeout, CheckInterval) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    assert_eventually_loop(Condition, Deadline, CheckInterval).

assert_eventually_loop(Condition, Deadline, CheckInterval) ->
    case Condition() of
        true -> 
            ok;
        false ->
            Now = erlang:monotonic_time(millisecond),
            if
                Now >= Deadline ->
                    error({assertion_failed, timeout});
                true ->
                    timer:sleep(CheckInterval),
                    assert_eventually_loop(Condition, Deadline, CheckInterval)
            end
    end.

assert_within(Fun, Expected, Tolerance) ->
    Result = Fun(),
    Diff = abs(Result - Expected),
    case Diff =< Tolerance of
        true -> ok;
        false -> error({assertion_failed, 
                       {expected, Expected}, 
                       {got, Result}, 
                       {tolerance, Tolerance}})
    end.

%%%===================================================================
%%% Data Generators
%%%===================================================================

random_id() ->
    random_id(<<"test_">>).

random_id(Prefix) ->
    Suffix = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, Suffix/binary>>.

random_capability() ->
    Capabilities = [
        <<"compute">>, <<"storage">>, <<"network">>, 
        <<"analytics">>, <<"ml">>, <<"transform">>
    ],
    lists:nth(rand:uniform(length(Capabilities)), Capabilities).

random_capabilities(N) ->
    lists:usort([random_capability() || _ <- lists:seq(1, N)]).

random_message() ->
    #{
        id => random_id(<<"msg_">>),
        type => lists:nth(rand:uniform(3), [<<"request">>, <<"response">>, <<"notification">>]),
        timestamp => erlang:system_time(millisecond),
        data => random_binary(rand:uniform(1000))
    }.

random_binary(Size) ->
    crypto:strong_rand_bytes(Size).

%%%===================================================================
%%% Concurrent Test Helpers
%%%===================================================================

run_concurrent(Fun, N) ->
    run_concurrent(Fun, N, ?DEFAULT_TIMEOUT).

run_concurrent(Fun, N, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    
    Pids = lists:map(fun(I) ->
        spawn(fun() ->
            Result = try
                {ok, Fun(I)}
            catch
                Class:Reason:Stack ->
                    {error, {Class, Reason, Stack}}
            end,
            Parent ! {Ref, self(), I, Result}
        end)
    end, lists:seq(1, N)),
    
    Results = lists:map(fun(Pid) ->
        receive
            {Ref, Pid, I, Result} -> {I, Result}
        after Timeout ->
            {error, {timeout, Pid}}
        end
    end, Pids),
    
    % Check for errors
    Errors = [{I, E} || {I, {error, E}} <- Results],
    case Errors of
        [] -> {ok, Results};
        _ -> {error, Errors}
    end.

parallel_map(Fun, List) ->
    Parent = self(),
    Ref = make_ref(),
    
    Pids = lists:map(fun({Index, Item}) ->
        spawn(fun() ->
            Result = Fun(Item),
            Parent ! {Ref, Index, Result}
        end)
    end, lists:zip(lists:seq(1, length(List)), List)),
    
    Results = lists:map(fun(_) ->
        receive
            {Ref, Index, Result} -> {Index, Result}
        after ?DEFAULT_TIMEOUT ->
            error(timeout)
        end
    end, Pids),
    
    % Sort by index to maintain order
    [R || {_, R} <- lists:sort(Results)].

%%%===================================================================
%%% State Management
%%%===================================================================

create_test_state() ->
    case ets:info(test_state) of
        undefined ->
            ets:new(test_state, [named_table, public, set]);
        _ ->
            test_state
    end.

get_test_state(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Value}] -> {ok, Value};
        [] -> {error, not_found}
    end.

set_test_state(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    ok.

update_test_state(Table, Key, Fun) ->
    case get_test_state(Table, Key) of
        {ok, OldValue} ->
            NewValue = Fun(OldValue),
            set_test_state(Table, Key, NewValue),
            {ok, NewValue};
        {error, not_found} ->
            {error, not_found}
    end.

delete_test_state(Table) ->
    catch ets:delete(Table),
    ok.