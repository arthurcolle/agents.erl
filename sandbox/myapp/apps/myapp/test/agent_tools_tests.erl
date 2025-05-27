%%%-------------------------------------------------------------------
%%% @doc
%%% Agent Tools Tests
%%% Tests for tool registration and execution
%%% @end
%%%-------------------------------------------------------------------
-module(agent_tools_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(myapp),
    % Clear any existing tools
    agent_tools:clear_all(),
    ok.

teardown(_) ->
    agent_tools:clear_all(),
    application:stop(myapp),
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

agent_tools_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Basic tool registration", fun test_basic_registration/0},
      {"Tool execution", fun test_tool_execution/0},
      {"Tool validation", fun test_tool_validation/0},
      {"Tool chaining", fun test_tool_chaining/0},
      {"Async tool execution", fun test_async_execution/0},
      {"Tool versioning", fun test_tool_versioning/0},
      {"Error handling", fun test_error_handling/0},
      {"Concurrent tool execution", fun test_concurrent_execution/0},
      {"Tool metadata", fun test_tool_metadata/0},
      {"Performance", fun test_performance/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_registration() ->
    ToolName = <<"calculator">>,
    ToolSpec = #{
        name => ToolName,
        description => <<"Basic arithmetic calculator">>,
        parameters => [
            #{name => <<"operation">>, type => string, required => true},
            #{name => <<"a">>, type => number, required => true},
            #{name => <<"b">>, type => number, required => true}
        ],
        handler => fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}) ->
            case Op of
                <<"add">> -> {ok, A + B};
                <<"subtract">> -> {ok, A - B};
                <<"multiply">> -> {ok, A * B};
                <<"divide">> when B =/= 0 -> {ok, A / B};
                <<"divide">> -> {error, division_by_zero};
                _ -> {error, unknown_operation}
            end
        end
    },
    
    % Register tool
    ?assertEqual(ok, agent_tools:register_tool(ToolSpec)),
    
    % Verify registration
    {ok, RegisteredTool} = agent_tools:get_tool(ToolName),
    ?assertEqual(ToolName, maps:get(name, RegisteredTool)),
    ?assertEqual(maps:get(description, ToolSpec), maps:get(description, RegisteredTool)),
    
    % List tools
    Tools = agent_tools:list_tools(),
    ?assert(lists:member(ToolName, Tools)).

test_tool_execution() ->
    % Register calculator tool
    register_calculator_tool(),
    
    % Test various operations
    ?assertEqual({ok, 7}, agent_tools:execute_tool(<<"calculator">>, 
        #{<<"operation">> => <<"add">>, <<"a">> => 3, <<"b">> => 4})),
    
    ?assertEqual({ok, 10}, agent_tools:execute_tool(<<"calculator">>,
        #{<<"operation">> => <<"multiply">>, <<"a">> => 2, <<"b">> => 5})),
    
    ?assertEqual({ok, 2.5}, agent_tools:execute_tool(<<"calculator">>,
        #{<<"operation">> => <<"divide">>, <<"a">> => 5, <<"b">> => 2})),
    
    % Test error cases
    ?assertEqual({error, division_by_zero}, agent_tools:execute_tool(<<"calculator">>,
        #{<<"operation">> => <<"divide">>, <<"a">> => 5, <<"b">> => 0})),
    
    ?assertEqual({error, unknown_operation}, agent_tools:execute_tool(<<"calculator">>,
        #{<<"operation">> => <<"modulo">>, <<"a">> => 5, <<"b">> => 2})).

test_tool_validation() ->
    % Register tool with strict validation
    ToolSpec = #{
        name => <<"validator_tool">>,
        description => <<"Tool with parameter validation">>,
        parameters => [
            #{name => <<"email">>, type => string, required => true, 
              validator => fun(V) -> validate_email(V) end},
            #{name => <<"age">>, type => number, required => true,
              validator => fun(V) -> V >= 0 andalso V =< 150 end},
            #{name => <<"tags">>, type => list, required => false,
              validator => fun(V) -> length(V) =< 10 end}
        ],
        handler => fun(Params) -> {ok, Params} end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(ToolSpec)),
    
    % Valid parameters
    ValidParams = #{
        <<"email">> => <<"test@example.com">>,
        <<"age">> => 25,
        <<"tags">> => [<<"erlang">>, <<"testing">>]
    },
    ?assertMatch({ok, _}, agent_tools:execute_tool(<<"validator_tool">>, ValidParams)),
    
    % Invalid email
    ?assertMatch({error, {validation_failed, _}}, 
        agent_tools:execute_tool(<<"validator_tool">>, 
            ValidParams#{<<"email">> => <<"not-an-email">>})),
    
    % Invalid age
    ?assertMatch({error, {validation_failed, _}},
        agent_tools:execute_tool(<<"validator_tool">>,
            ValidParams#{<<"age">> => 200})),
    
    % Too many tags
    ?assertMatch({error, {validation_failed, _}},
        agent_tools:execute_tool(<<"validator_tool">>,
            ValidParams#{<<"tags">> => lists:duplicate(11, <<"tag">>)})),
    
    % Missing required parameter
    ?assertMatch({error, {missing_required_parameter, _}},
        agent_tools:execute_tool(<<"validator_tool">>,
            maps:remove(<<"email">>, ValidParams))).

test_tool_chaining() ->
    % Register tools that can be chained
    register_text_tools(),
    
    % Execute chain: uppercase -> reverse -> word_count
    Text = <<"hello world">>,
    
    {ok, Upper} = agent_tools:execute_tool(<<"uppercase">>, #{<<"text">> => Text}),
    ?assertEqual(<<"HELLO WORLD">>, Upper),
    
    {ok, Reversed} = agent_tools:execute_tool(<<"reverse">>, #{<<"text">> => Upper}),
    ?assertEqual(<<"DLROW OLLEH">>, Reversed),
    
    {ok, WordCount} = agent_tools:execute_tool(<<"word_count">>, #{<<"text">> => Reversed}),
    ?assertEqual(2, WordCount),
    
    % Test tool pipeline
    Pipeline = [
        {<<"uppercase">>, #{<<"text">> => Text}},
        {<<"reverse">>, #{<<"text">> => <<"$result">>}},
        {<<"word_count">>, #{<<"text">> => <<"$result">>}}
    ],
    
    {ok, FinalResult} = agent_tools:execute_pipeline(Pipeline),
    ?assertEqual(2, FinalResult).

test_async_execution() ->
    % Register async tool
    Parent = self(),
    AsyncTool = #{
        name => <<"async_processor">>,
        description => <<"Async processing tool">>,
        async => true,
        parameters => [
            #{name => <<"delay">>, type => number, required => true},
            #{name => <<"value">>, type => any, required => true}
        ],
        handler => fun(#{<<"delay">> := Delay, <<"value">> := Value}) ->
            spawn(fun() ->
                timer:sleep(Delay),
                Parent ! {async_result, Value * 2}
            end),
            {ok, processing}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(AsyncTool)),
    
    % Execute async
    {ok, processing} = agent_tools:execute_tool(<<"async_processor">>, 
        #{<<"delay">> => 100, <<"value">> => 21}),
    
    % Wait for result
    receive
        {async_result, Result} ->
            ?assertEqual(42, Result)
    after 500 ->
        ?assert(false)
    end,
    
    % Test async with callback
    CallbackRef = make_ref(),
    AsyncWithCallback = #{
        name => <<"async_with_callback">>,
        description => <<"Async tool with callback">>,
        async => true,
        parameters => [#{name => <<"data">>, type => any, required => true}],
        handler => fun(#{<<"data">> := Data}, Callback) ->
            spawn(fun() ->
                timer:sleep(50),
                ProcessedData = {processed, Data},
                Callback({ok, ProcessedData})
            end),
            {ok, async_started}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(AsyncWithCallback)),
    
    {ok, async_started} = agent_tools:execute_tool_async(<<"async_with_callback">>,
        #{<<"data">> => CallbackRef},
        fun(Result) ->
            Parent ! {callback_executed, Result}
        end),
    
    receive
        {callback_executed, {ok, {processed, CallbackRef}}} ->
            ok
    after 500 ->
        ?assert(false)
    end.

test_tool_versioning() ->
    ToolName = <<"versioned_tool">>,
    
    % Register version 1.0
    ToolV1 = #{
        name => ToolName,
        version => <<"1.0">>,
        description => <<"Version 1.0">>,
        parameters => [#{name => <<"input">>, type => string, required => true}],
        handler => fun(#{<<"input">> := Input}) ->
            {ok, <<"v1: ", Input/binary>>}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(ToolV1)),
    
    % Register version 2.0
    ToolV2 = #{
        name => ToolName,
        version => <<"2.0">>,
        description => <<"Version 2.0 with improvements">>,
        parameters => [
            #{name => <<"input">>, type => string, required => true},
            #{name => <<"format">>, type => string, required => false}
        ],
        handler => fun(#{<<"input">> := Input} = Params) ->
            Format = maps:get(<<"format">>, Params, <<"default">>),
            {ok, <<"v2 (", Format/binary, "): ", Input/binary>>}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(ToolV2)),
    
    % Execute default (latest) version
    {ok, Result1} = agent_tools:execute_tool(ToolName, #{<<"input">> => <<"test">>}),
    ?assertMatch(<<"v2 (default): test">>, Result1),
    
    % Execute specific version
    {ok, Result2} = agent_tools:execute_tool(ToolName, #{<<"input">> => <<"test">>}, 
                                           #{version => <<"1.0">>}),
    ?assertEqual(<<"v1: test">>, Result2),
    
    % List versions
    Versions = agent_tools:list_tool_versions(ToolName),
    ?assertEqual([<<"1.0">>, <<"2.0">>], lists:sort(Versions)).

test_error_handling() ->
    % Register tool that can fail
    ErrorTool = #{
        name => <<"error_tool">>,
        description => <<"Tool for testing error handling">>,
        parameters => [#{name => <<"scenario">>, type => string, required => true}],
        handler => fun(#{<<"scenario">> := Scenario}) ->
            case Scenario of
                <<"success">> -> {ok, success};
                <<"error">> -> {error, deliberate_error};
                <<"crash">> -> error(deliberate_crash);
                <<"throw">> -> throw(deliberate_throw);
                <<"exit">> -> exit(deliberate_exit);
                <<"timeout">> -> 
                    timer:sleep(10000),
                    {ok, should_timeout}
            end
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(ErrorTool)),
    
    % Test success
    ?assertEqual({ok, success}, agent_tools:execute_tool(<<"error_tool">>, 
                                                       #{<<"scenario">> => <<"success">>})),
    
    % Test controlled error
    ?assertEqual({error, deliberate_error}, agent_tools:execute_tool(<<"error_tool">>,
                                                                   #{<<"scenario">> => <<"error">>})),
    
    % Test crash handling
    Result1 = agent_tools:execute_tool(<<"error_tool">>, #{<<"scenario">> => <<"crash">>}),
    ?assertMatch({error, {tool_crashed, _}}, Result1),
    
    % Test throw handling
    Result2 = agent_tools:execute_tool(<<"error_tool">>, #{<<"scenario">> => <<"throw">>}),
    ?assertMatch({error, {tool_threw, _}}, Result2),
    
    % Test exit handling  
    Result3 = agent_tools:execute_tool(<<"error_tool">>, #{<<"scenario">> => <<"exit">>}),
    ?assertMatch({error, {tool_exited, _}}, Result3),
    
    % Test timeout (with shorter timeout)
    Result4 = agent_tools:execute_tool(<<"error_tool">>, 
                                     #{<<"scenario">> => <<"timeout">>},
                                     #{timeout => 100}),
    ?assertMatch({error, timeout}, Result4),
    
    % Test non-existent tool
    ?assertEqual({error, tool_not_found}, 
                 agent_tools:execute_tool(<<"non_existent_tool">>, #{})).

test_concurrent_execution() ->
    % Register CPU-bound tool
    CPUTool = #{
        name => <<"cpu_tool">>,
        description => <<"CPU intensive tool">>,
        parameters => [
            #{name => <<"iterations">>, type => number, required => true},
            #{name => <<"id">>, type => number, required => true}
        ],
        handler => fun(#{<<"iterations">> := N, <<"id">> := Id}) ->
            % Simulate CPU work
            Result = lists:foldl(fun(I, Acc) ->
                math:sin(I) * math:cos(I) + Acc
            end, 0, lists:seq(1, N)),
            {ok, {Id, Result}}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(CPUTool)),
    
    % Execute concurrently
    NumWorkers = 20,
    Iterations = 10000,
    
    Parent = self(),
    Workers = lists:map(fun(Id) ->
        spawn(fun() ->
            Result = agent_tools:execute_tool(<<"cpu_tool">>, 
                #{<<"iterations">> => Iterations, <<"id">> => Id}),
            Parent ! {worker_done, Id, Result}
        end)
    end, lists:seq(1, NumWorkers)),
    
    % Collect results
    Results = lists:map(fun(_) ->
        receive
            {worker_done, Id, Result} -> {Id, Result}
        after ?TIMEOUT ->
            timeout
        end
    end, Workers),
    
    % Verify all completed
    ?assertEqual(NumWorkers, length([R || R <- Results, R =/= timeout])),
    
    % Verify results are consistent
    [{_, {ok, {1, Expected}}} | Rest] = lists:sort(Results),
    lists:foreach(fun({_, {ok, {_, Value}}}) ->
        ?assert(abs(Value - Expected) < 0.001) % Float comparison tolerance
    end, Rest).

test_tool_metadata() ->
    % Register tool with rich metadata
    MetaTool = #{
        name => <<"meta_tool">>,
        description => <<"Tool with metadata">>,
        version => <<"1.2.3">>,
        author => <<"Test Author">>,
        tags => [<<"utility">>, <<"text">>, <<"processing">>],
        examples => [
            #{input => #{<<"text">> => <<"hello">>}, output => <<"HELLO">>}
        ],
        deprecated => false,
        replacement => none,
        parameters => [#{name => <<"text">>, type => string, required => true}],
        handler => fun(#{<<"text">> := Text}) ->
            {ok, string:uppercase(Text)}
        end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(MetaTool)),
    
    % Query metadata
    {ok, Tool} = agent_tools:get_tool(<<"meta_tool">>),
    ?assertEqual(<<"1.2.3">>, maps:get(version, Tool)),
    ?assertEqual(<<"Test Author">>, maps:get(author, Tool)),
    ?assertEqual([<<"utility">>, <<"text">>, <<"processing">>], maps:get(tags, Tool)),
    
    % Search by tags
    TextTools = agent_tools:find_tools_by_tag(<<"text">>),
    ?assert(lists:member(<<"meta_tool">>, TextTools)),
    
    % Test deprecation
    DeprecatedTool = MetaTool#{
        name => <<"old_tool">>,
        deprecated => true,
        replacement => <<"meta_tool">>
    },
    
    ?assertEqual(ok, agent_tools:register_tool(DeprecatedTool)),
    
    % Executing deprecated tool should warn but work
    {ok, <<"TEST">>} = agent_tools:execute_tool(<<"old_tool">>, #{<<"text">> => <<"test">>}).

test_performance() ->
    % Register lightweight tool
    FastTool = #{
        name => <<"fast_tool">>,
        description => <<"Performance test tool">>,
        parameters => [#{name => <<"value">>, type => number, required => true}],
        handler => fun(#{<<"value">> := V}) -> {ok, V * 2} end
    },
    
    ?assertEqual(ok, agent_tools:register_tool(FastTool)),
    
    % Measure single execution time
    {Time1, {ok, 84}} = timer:tc(fun() ->
        agent_tools:execute_tool(<<"fast_tool">>, #{<<"value">> => 42})
    end),
    
    ?assert(Time1 < 1000), % Should be under 1ms
    
    % Measure bulk execution
    NumExecutions = 10000,
    {TimeBulk, Results} = timer:tc(fun() ->
        lists:map(fun(N) ->
            agent_tools:execute_tool(<<"fast_tool">>, #{<<"value">> => N})
        end, lists:seq(1, NumExecutions))
    end),
    
    ?assertEqual(NumExecutions, length(Results)),
    AvgTime = TimeBulk / NumExecutions,
    ?assert(AvgTime < 1000), % Average should be under 1ms
    
    io:format("Performance: Single execution ~p μs, Average ~p μs over ~p executions~n",
              [Time1, AvgTime, NumExecutions]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

register_calculator_tool() ->
    ToolSpec = #{
        name => <<"calculator">>,
        description => <<"Basic arithmetic calculator">>,
        parameters => [
            #{name => <<"operation">>, type => string, required => true},
            #{name => <<"a">>, type => number, required => true},
            #{name => <<"b">>, type => number, required => true}
        ],
        handler => fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}) ->
            case Op of
                <<"add">> -> {ok, A + B};
                <<"subtract">> -> {ok, A - B};
                <<"multiply">> -> {ok, A * B};
                <<"divide">> when B =/= 0 -> {ok, A / B};
                <<"divide">> -> {error, division_by_zero};
                _ -> {error, unknown_operation}
            end
        end
    },
    agent_tools:register_tool(ToolSpec).

register_text_tools() ->
    UppercaseTool = #{
        name => <<"uppercase">>,
        description => <<"Convert text to uppercase">>,
        parameters => [#{name => <<"text">>, type => string, required => true}],
        handler => fun(#{<<"text">> := Text}) ->
            {ok, string:uppercase(Text)}
        end
    },
    
    ReverseTool = #{
        name => <<"reverse">>,
        description => <<"Reverse text">>,
        parameters => [#{name => <<"text">>, type => string, required => true}],
        handler => fun(#{<<"text">> := Text}) ->
            {ok, list_to_binary(lists:reverse(binary_to_list(Text)))}
        end
    },
    
    WordCountTool = #{
        name => <<"word_count">>,
        description => <<"Count words in text">>,
        parameters => [#{name => <<"text">>, type => string, required => true}],
        handler => fun(#{<<"text">> := Text}) ->
            Words = string:tokens(binary_to_list(Text), " \t\n"),
            {ok, length(Words)}
        end
    },
    
    agent_tools:register_tool(UppercaseTool),
    agent_tools:register_tool(ReverseTool),
    agent_tools:register_tool(WordCountTool).

validate_email(Email) when is_binary(Email) ->
    case re:run(Email, "^[^@]+@[^@]+\\.[^@]+$") of
        {match, _} -> true;
        nomatch -> false
    end;
validate_email(_) -> false.