%%%-------------------------------------------------------------------
%%% @doc
%%% Performance Tests
%%% Performance and load tests for the agent system
%%% @end
%%%-------------------------------------------------------------------
-module(performance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 60000). % 60 seconds for performance tests
-define(WARMUP_ITERATIONS, 100).
-define(BENCH_ITERATIONS, 10000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start applications
    application:ensure_all_started(myapp),
    
    % Clear state
    agent_registry:clear_all(),
    agent_discovery:clear_all(),
    agent_tools:clear_all(),
    
    % Set up performance monitoring
    ets:new(perf_metrics, [named_table, public, set]),
    
    ok.

teardown(_) ->
    % Clean up
    agent_registry:clear_all(),
    agent_discovery:clear_all(),
    agent_tools:clear_all(),
    
    ets:delete(perf_metrics),
    
    application:stop(myapp),
    
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

performance_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, ?TIMEOUT,
      [
       {"UUID generation performance", fun test_uuid_generation/0},
       {"Agent registration performance", fun test_agent_registration/0},
       {"Message throughput", fun test_message_throughput/0},
       {"Tool execution performance", fun test_tool_execution/0},
       {"Concurrent agent operations", fun test_concurrent_operations/0},
       {"Memory usage", fun test_memory_usage/0},
       {"Protocol encoding/decoding", fun test_protocol_performance/0},
       {"Discovery performance", fun test_discovery_performance/0},
       {"Large message handling", fun test_large_messages/0},
       {"System scalability", fun test_system_scalability/0}
      ]}}.

%%%===================================================================
%%% Performance Tests
%%%===================================================================

test_uuid_generation() ->
    io:format("~n=== UUID Generation Performance ===~n"),
    
    % Warm up
    lists:foreach(fun(_) -> uuid:v4() end, lists:seq(1, ?WARMUP_ITERATIONS)),
    
    % Test different UUID versions
    Versions = [
        {v1, fun uuid:v1/0},
        {v4, fun uuid:v4/0}
    ],
    
    lists:foreach(fun({Version, GenFun}) ->
        {Time, UUIDs} = timer:tc(fun() ->
            [GenFun() || _ <- lists:seq(1, ?BENCH_ITERATIONS)]
        end),
        
        Rate = ?BENCH_ITERATIONS * 1000000 / Time,
        io:format("UUID ~p: ~.2f generations/sec (~p μs/op)~n", 
                  [Version, Rate, Time div ?BENCH_ITERATIONS]),
        
        % Verify uniqueness
        ?assertEqual(?BENCH_ITERATIONS, length(lists:usort(UUIDs))),
        
        % Store metrics
        ets:insert(perf_metrics, {{uuid_rate, Version}, Rate})
    end, Versions),
    
    % Test concurrent generation
    NumProcs = erlang:system_info(schedulers),
    Parent = self(),
    
    {ConcurrentTime, _} = timer:tc(fun() ->
        Pids = lists:map(fun(_) ->
            spawn(fun() ->
                UUIDs = [uuid:v4() || _ <- lists:seq(1, ?BENCH_ITERATIONS div NumProcs)],
                Parent ! {self(), length(lists:usort(UUIDs))}
            end)
        end, lists:seq(1, NumProcs)),
        
        lists:foreach(fun(Pid) ->
            receive
                {Pid, Count} ->
                    ?assertEqual(?BENCH_ITERATIONS div NumProcs, Count)
            after ?TIMEOUT ->
                ?assert(false)
            end
        end, Pids)
    end),
    
    ConcurrentRate = ?BENCH_ITERATIONS * 1000000 / ConcurrentTime,
    io:format("Concurrent UUID v4 (~p procs): ~.2f generations/sec~n", 
              [NumProcs, ConcurrentRate]),
    
    % Should scale well
    [{uuid_rate, v4}, SingleRate] = ets:lookup(perf_metrics, {uuid_rate, v4}),
    ?assert(ConcurrentRate > SingleRate * 0.7).

test_agent_registration() ->
    io:format("~n=== Agent Registration Performance ===~n"),
    
    % Sequential registration
    AgentIds = [list_to_binary("perf_agent_" ++ integer_to_list(N)) 
                || N <- lists:seq(1, ?BENCH_ITERATIONS)],
    
    {RegTime, _} = timer:tc(fun() ->
        lists:foreach(fun(Id) ->
            agent_registry:register(Id, self())
        end, AgentIds)
    end),
    
    RegRate = ?BENCH_ITERATIONS * 1000000 / RegTime,
    io:format("Sequential registration: ~.2f ops/sec~n", [RegRate]),
    
    % Lookup performance
    {LookupTime, _} = timer:tc(fun() ->
        lists:foreach(fun(Id) ->
            {ok, _} = agent_registry:lookup(Id)
        end, AgentIds)
    end),
    
    LookupRate = ?BENCH_ITERATIONS * 1000000 / LookupTime,
    io:format("Lookup performance: ~.2f ops/sec~n", [LookupRate]),
    
    % Concurrent registration (after cleanup)
    agent_registry:clear_all(),
    
    NumProcs = 10,
    BatchSize = ?BENCH_ITERATIONS div NumProcs,
    Parent = self(),
    
    {ConcRegTime, _} = timer:tc(fun() ->
        Pids = lists:map(fun(ProcNum) ->
            spawn(fun() ->
                Start = (ProcNum - 1) * BatchSize + 1,
                End = ProcNum * BatchSize,
                lists:foreach(fun(N) ->
                    Id = list_to_binary("conc_agent_" ++ integer_to_list(N)),
                    agent_registry:register(Id, self())
                end, lists:seq(Start, End)),
                Parent ! {done, self()}
            end)
        end, lists:seq(1, NumProcs)),
        
        lists:foreach(fun(Pid) ->
            receive {done, Pid} -> ok end
        end, Pids)
    end),
    
    ConcRegRate = ?BENCH_ITERATIONS * 1000000 / ConcRegTime,
    io:format("Concurrent registration (~p procs): ~.2f ops/sec~n", 
              [NumProcs, ConcRegRate]),
    
    % Verify all registered
    AllAgents = agent_registry:list_agents(),
    ?assert(length(AllAgents) >= ?BENCH_ITERATIONS).

test_message_throughput() ->
    io:format("~n=== Message Throughput Performance ===~n"),
    
    % Set up test agents
    SenderId = <<"perf_sender">>,
    ReceiverId = <<"perf_receiver">>,
    
    ReceivedCount = ets:new(received_count, [public, set]),
    ets:insert(ReceivedCount, {count, 0}),
    
    agent_messenger:register_handler(ReceiverId, fun(_Msg) ->
        ets:update_counter(ReceivedCount, count, 1)
    end),
    
    % Test single-threaded throughput
    Messages = [#{id => N, data => <<"test">>} || N <- lists:seq(1, ?BENCH_ITERATIONS)],
    
    {SendTime, _} = timer:tc(fun() ->
        lists:foreach(fun(Msg) ->
            {ok, _} = agent_messenger:send_message(SenderId, ReceiverId, Msg)
        end, Messages)
    end),
    
    % Wait for all messages to be processed
    timer:sleep(100),
    [{count, Received}] = ets:lookup(ReceivedCount, count),
    
    SendRate = ?BENCH_ITERATIONS * 1000000 / SendTime,
    io:format("Single-threaded send rate: ~.2f msg/sec~n", [SendRate]),
    io:format("Messages received: ~p/~p~n", [Received, ?BENCH_ITERATIONS]),
    
    % Reset counter
    ets:insert(ReceivedCount, {count, 0}),
    
    % Test concurrent message sending
    NumSenders = 10,
    BatchSize = 1000,
    Parent = self(),
    
    {ConcSendTime, _} = timer:tc(fun() ->
        Pids = lists:map(fun(SenderNum) ->
            spawn(fun() ->
                SenderId2 = list_to_binary("sender_" ++ integer_to_list(SenderNum)),
                lists:foreach(fun(N) ->
                    Msg = #{sender => SenderNum, seq => N},
                    agent_messenger:send_message(SenderId2, ReceiverId, Msg)
                end, lists:seq(1, BatchSize)),
                Parent ! {done, self()}
            end)
        end, lists:seq(1, NumSenders)),
        
        lists:foreach(fun(Pid) ->
            receive {done, Pid} -> ok end
        end, Pids)
    end),
    
    timer:sleep(500), % Wait for processing
    [{count, ConcReceived}] = ets:lookup(ReceivedCount, count),
    
    ConcSendRate = (NumSenders * BatchSize) * 1000000 / ConcSendTime,
    io:format("Concurrent send rate (~p senders): ~.2f msg/sec~n", 
              [NumSenders, ConcSendRate]),
    io:format("Messages received: ~p/~p~n", [ConcReceived, NumSenders * BatchSize]),
    
    ets:delete(ReceivedCount).

test_tool_execution() ->
    io:format("~n=== Tool Execution Performance ===~n"),
    
    % Register test tools
    Tools = [
        #{
            name => <<"simple_tool">>,
            handler => fun(#{<<"x">> := X}) -> {ok, X * 2} end,
            parameters => [#{name => <<"x">>, type => number, required => true}]
        },
        #{
            name => <<"complex_tool">>,
            handler => fun(#{<<"data">> := Data}) ->
                % Simulate complex processing
                Result = lists:foldl(fun(X, Acc) -> 
                    math:sin(X) * math:cos(X) + Acc 
                end, 0, Data),
                {ok, Result}
            end,
            parameters => [#{name => <<"data">>, type => list, required => true}]
        }
    ],
    
    lists:foreach(fun(Tool) ->
        agent_tools:register_tool(Tool)
    end, Tools),
    
    % Test simple tool
    {SimpleTime, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            {ok, _} = agent_tools:execute_tool(<<"simple_tool">>, #{<<"x">> => N})
        end, lists:seq(1, ?BENCH_ITERATIONS))
    end),
    
    SimpleRate = ?BENCH_ITERATIONS * 1000000 / SimpleTime,
    io:format("Simple tool execution: ~.2f ops/sec~n", [SimpleRate]),
    
    % Test complex tool
    ComplexData = lists:seq(1, 100),
    ComplexIterations = 1000,
    
    {ComplexTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            {ok, _} = agent_tools:execute_tool(<<"complex_tool">>, 
                                             #{<<"data">> => ComplexData})
        end, lists:seq(1, ComplexIterations))
    end),
    
    ComplexRate = ComplexIterations * 1000000 / ComplexTime,
    io:format("Complex tool execution: ~.2f ops/sec~n", [ComplexRate]),
    
    % Test concurrent tool execution
    NumWorkers = erlang:system_info(schedulers),
    WorkerIterations = 1000,
    Parent = self(),
    
    {ConcToolTime, _} = timer:tc(fun() ->
        Pids = lists:map(fun(WorkerNum) ->
            spawn(fun() ->
                lists:foreach(fun(N) ->
                    {ok, _} = agent_tools:execute_tool(<<"simple_tool">>, 
                                                     #{<<"x">> => N + WorkerNum})
                end, lists:seq(1, WorkerIterations)),
                Parent ! {done, self()}
            end)
        end, lists:seq(1, NumWorkers)),
        
        lists:foreach(fun(Pid) ->
            receive {done, Pid} -> ok end
        end, Pids)
    end),
    
    ConcToolRate = (NumWorkers * WorkerIterations) * 1000000 / ConcToolTime,
    io:format("Concurrent tool execution (~p workers): ~.2f ops/sec~n", 
              [NumWorkers, ConcToolRate]).

test_concurrent_operations() ->
    io:format("~n=== Concurrent Operations Performance ===~n"),
    
    % Mixed workload test
    NumWorkers = 20,
    OpsPerWorker = 500,
    Parent = self(),
    
    % Create some agents and tools
    lists:foreach(fun(N) ->
        Id = list_to_binary("concurrent_agent_" ++ integer_to_list(N)),
        {ok, _} = agent:start_agent(#{
            id => Id,
            name => Id,
            capabilities => [<<"test">>, <<"concurrent">>]
        })
    end, lists:seq(1, 10)),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    Pids = lists:map(fun(WorkerNum) ->
        spawn(fun() ->
            random:seed(erlang:phash2([node()]), 
                       erlang:monotonic_time(), 
                       erlang:unique_integer()),
            
            Results = lists:map(fun(_) ->
                Op = rand:uniform(4),
                try
                    case Op of
                        1 -> % UUID generation
                            uuid:v4(),
                            {uuid, ok};
                        2 -> % Agent lookup
                            Id = list_to_binary("concurrent_agent_" ++ 
                                              integer_to_list(rand:uniform(10))),
                            agent_registry:lookup(Id),
                            {lookup, ok};
                        3 -> % Discovery
                            agent_discovery:find_agents_by_capability(<<"test">>),
                            {discovery, ok};
                        4 -> % Message send
                            From = list_to_binary("worker_" ++ 
                                                integer_to_list(WorkerNum)),
                            To = list_to_binary("concurrent_agent_" ++ 
                                              integer_to_list(rand:uniform(10))),
                            agent_messenger:send_message(From, To, #{test => true}),
                            {message, ok}
                    end
                catch
                    _:_ -> {error, failed}
                end
            end, lists:seq(1, OpsPerWorker)),
            
            Parent ! {results, self(), Results}
        end)
    end, lists:seq(1, NumWorkers)),
    
    % Collect results
    AllResults = lists:flatten(
        lists:map(fun(Pid) ->
            receive
                {results, Pid, Results} -> Results
            after ?TIMEOUT ->
                []
            end
        end, Pids)
    ),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    % Analyze results
    TotalOps = length(AllResults),
    SuccessOps = length([R || R <- AllResults, element(2, R) =:= ok]),
    
    OpCounts = lists:foldl(fun({Type, _}, Acc) ->
        maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, AllResults),
    
    io:format("Total operations: ~p in ~p ms (~.2f ops/sec)~n", 
              [TotalOps, Duration, TotalOps * 1000 / Duration]),
    io:format("Success rate: ~.2f%~n", [SuccessOps * 100 / TotalOps]),
    io:format("Operation breakdown: ~p~n", [OpCounts]),
    
    ?assert(SuccessOps / TotalOps > 0.95). % 95% success rate

test_memory_usage() ->
    io:format("~n=== Memory Usage Analysis ===~n"),
    
    % Get baseline memory
    erlang:garbage_collect(),
    BaselineMemory = erlang:memory(total),
    
    % Test memory usage with many agents
    NumAgents = 1000,
    
    io:format("Creating ~p agents...~n", [NumAgents]),
    AgentIds = lists:map(fun(N) ->
        Id = list_to_binary("mem_agent_" ++ integer_to_list(N)),
        {ok, _} = agent:start_agent(#{
            id => Id,
            name => Id,
            capabilities => [<<"memory_test">>],
            metadata => #{
                index => N,
                data => crypto:strong_rand_bytes(1024) % 1KB per agent
            }
        }),
        Id
    end, lists:seq(1, NumAgents)),
    
    erlang:garbage_collect(),
    AfterAgentsMemory = erlang:memory(total),
    AgentMemoryUsage = (AfterAgentsMemory - BaselineMemory) / 1024 / 1024,
    
    io:format("Memory after creating agents: ~.2f MB (~.2f KB/agent)~n", 
              [AgentMemoryUsage, AgentMemoryUsage * 1024 / NumAgents]),
    
    % Test memory with messages
    io:format("Sending messages...~n"),
    lists:foreach(fun(N) ->
        From = list_to_binary("sender_" ++ integer_to_list(N)),
        To = lists:nth(rand:uniform(NumAgents), AgentIds),
        Msg = #{
            index => N,
            data => crypto:strong_rand_bytes(512) % 512B per message
        },
        agent_messenger:send_message(From, To, Msg)
    end, lists:seq(1, 10000)),
    
    erlang:garbage_collect(),
    AfterMessagesMemory = erlang:memory(total),
    MessageMemoryUsage = (AfterMessagesMemory - AfterAgentsMemory) / 1024 / 1024,
    
    io:format("Additional memory for messages: ~.2f MB~n", [MessageMemoryUsage]),
    
    % Clean up and measure
    lists:foreach(fun(Id) -> agent:stop_agent(Id) end, AgentIds),
    erlang:garbage_collect(),
    
    FinalMemory = erlang:memory(total),
    io:format("Memory after cleanup: ~.2f MB~n", 
              [(FinalMemory - BaselineMemory) / 1024 / 1024]),
    
    % Memory should be mostly reclaimed
    ?assert((FinalMemory - BaselineMemory) < (AfterMessagesMemory - BaselineMemory) * 0.1).

test_protocol_performance() ->
    io:format("~n=== Protocol Performance ===~n"),
    
    % Test different message sizes
    MessageSizes = [
        {small, 100},
        {medium, 1024},
        {large, 10240},
        {xlarge, 102400}
    ],
    
    lists:foreach(fun({Label, Size}) ->
        Msg = #{
            id => uuid:v4(),
            type => <<"benchmark">>,
            payload => crypto:strong_rand_bytes(Size),
            metadata => #{size => Size}
        },
        
        % Encoding benchmark
        {EncTime, Encoded} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                agent_protocol:encode(Msg)
            end, lists:seq(1, 1000))
        end),
        
        EncRate = 1000 * 1000000 / EncTime,
        EncThroughput = Size * EncRate / 1024 / 1024,
        
        % Decoding benchmark
        {DecTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                agent_protocol:decode(Encoded)
            end, lists:seq(1, 1000))
        end),
        
        DecRate = 1000 * 1000000 / DecTime,
        DecThroughput = Size * DecRate / 1024 / 1024,
        
        io:format("~p messages (~p bytes):~n", [Label, Size]),
        io:format("  Encode: ~.2f msg/sec (~.2f MB/sec)~n", 
                  [EncRate, EncThroughput]),
        io:format("  Decode: ~.2f msg/sec (~.2f MB/sec)~n", 
                  [DecRate, DecThroughput])
    end, MessageSizes),
    
    % Test compression performance
    LargeData = crypto:strong_rand_bytes(100000),
    CompressMsg = #{
        id => uuid:v4(),
        type => <<"compress_test">>,
        payload => LargeData
    },
    
    {NoCompTime, NoCompEncoded} = timer:tc(fun() ->
        agent_protocol:encode(CompressMsg)
    end),
    
    {CompTime, CompEncoded} = timer:tc(fun() ->
        agent_protocol:encode(CompressMsg, #{compress => true})
    end),
    
    Ratio = byte_size(NoCompEncoded) / byte_size(CompEncoded),
    io:format("~nCompression: ~.2fx reduction, ~p μs overhead~n", 
              [Ratio, CompTime - NoCompTime]).

test_discovery_performance() ->
    io:format("~n=== Discovery Performance ===~n"),
    
    % Register many agents with various capabilities
    AllCapabilities = [
        <<"compute">>, <<"storage">>, <<"network">>, <<"security">>,
        <<"analytics">>, <<"ml">>, <<"nlp">>, <<"vision">>,
        <<"audio">>, <<"video">>, <<"iot">>, <<"blockchain">>
    ],
    
    NumAgents = 5000,
    io:format("Registering ~p agents...~n", [NumAgents]),
    
    {RegTime, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Id = list_to_binary("discovery_agent_" ++ integer_to_list(N)),
            % Each agent has 2-5 random capabilities
            NumCaps = 2 + rand:uniform(3),
            Caps = lists:sublist(
                lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, AllCapabilities),
                NumCaps
            ),
            agent_discovery:register_agent(Id, Caps, #{index => N})
        end, lists:seq(1, NumAgents))
    end),
    
    io:format("Registration time: ~p ms (~.2f agents/sec)~n", 
              [RegTime div 1000, NumAgents * 1000000 / RegTime]),
    
    % Test single capability lookup
    lists:foreach(fun(Cap) ->
        {LookupTime, Results} = timer:tc(fun() ->
            agent_discovery:find_agents_by_capability(Cap)
        end),
        
        io:format("Lookup '~s': ~p agents in ~p μs~n", 
                  [Cap, length(Results), LookupTime])
    end, lists:sublist(AllCapabilities, 4)),
    
    % Test multi-capability lookup
    MultiCaps = [<<"compute">>, <<"analytics">>],
    {MultiTime, MultiResults} = timer:tc(fun() ->
        agent_discovery:find_agents_with_all_capabilities(MultiCaps)
    end),
    
    io:format("Multi-capability lookup ~p: ~p agents in ~p μs~n", 
              [MultiCaps, length(MultiResults), MultiTime]),
    
    % Benchmark repeated lookups
    BenchCap = <<"compute">>,
    {BenchTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            agent_discovery:find_agents_by_capability(BenchCap)
        end, lists:seq(1, 1000))
    end),
    
    LookupRate = 1000 * 1000000 / BenchTime,
    io:format("Lookup rate: ~.2f lookups/sec~n", [LookupRate]).

test_large_messages() ->
    io:format("~n=== Large Message Performance ===~n"),
    
    % Set up sender and receiver
    SenderId = <<"large_sender">>,
    ReceiverId = <<"large_receiver">>,
    ReceivedSizes = ets:new(received_sizes, [public, bag]),
    
    agent_messenger:register_handler(ReceiverId, fun(Msg) ->
        Data = maps:get(data, maps:get(message, Msg)),
        ets:insert(ReceivedSizes, {size, byte_size(Data)})
    end),
    
    % Test various message sizes
    Sizes = [
        {<<"1KB">>, 1024},
        {<<"10KB">>, 10 * 1024},
        {<<"100KB">>, 100 * 1024},
        {<<"1MB">>, 1024 * 1024},
        {<<"10MB">>, 10 * 1024 * 1024}
    ],
    
    lists:foreach(fun({Label, Size}) ->
        Data = crypto:strong_rand_bytes(Size),
        Msg = #{data => Data, size => Size},
        
        {SendTime, {ok, _}} = timer:tc(fun() ->
            agent_messenger:send_message(SenderId, ReceiverId, Msg)
        end),
        
        timer:sleep(100), % Wait for delivery
        
        Throughput = Size / SendTime * 1000000 / 1024 / 1024,
        io:format("~s message: ~p ms (~.2f MB/sec)~n", 
                  [Label, SendTime div 1000, Throughput])
    end, Sizes),
    
    % Verify all received
    ReceivedList = ets:tab2list(ReceivedSizes),
    io:format("Messages received: ~p~n", [length(ReceivedList)]),
    
    ets:delete(ReceivedSizes).

test_system_scalability() ->
    io:format("~n=== System Scalability Test ===~n"),
    
    % Test with increasing numbers of agents
    Configurations = [
        {10, 100},    % 10 agents, 100 operations each
        {100, 100},   % 100 agents, 100 operations each  
        {1000, 10}    % 1000 agents, 10 operations each
    ],
    
    lists:foreach(fun({NumAgents, OpsPerAgent}) ->
        io:format("~nTesting with ~p agents, ~p ops each...~n", 
                  [NumAgents, OpsPerAgent]),
        
        % Clear previous state
        agent_registry:clear_all(),
        agent_discovery:clear_all(),
        
        % Create agents
        {CreateTime, AgentIds} = timer:tc(fun() ->
            lists:map(fun(N) ->
                Id = list_to_binary("scale_agent_" ++ integer_to_list(N)),
                {ok, _} = agent:start_agent(#{
                    id => Id,
                    name => Id,
                    capabilities => [<<"scale_test">>]
                }),
                Id
            end, lists:seq(1, NumAgents))
        end),
        
        io:format("Agent creation: ~p ms (~.2f agents/sec)~n", 
                  [CreateTime div 1000, NumAgents * 1000000 / CreateTime]),
        
        % Run operations
        Parent = self(),
        {OpsTime, _} = timer:tc(fun() ->
            Pids = lists:map(fun(AgentId) ->
                spawn(fun() ->
                    lists:foreach(fun(OpNum) ->
                        % Mix of operations
                        case OpNum rem 3 of
                            0 ->
                                agent_registry:lookup(AgentId);
                            1 ->
                                agent_discovery:find_agents_by_capability(<<"scale_test">>);
                            2 ->
                                From = AgentId,
                                To = lists:nth(rand:uniform(NumAgents), AgentIds),
                                agent_messenger:send_message(From, To, #{op => OpNum})
                        end
                    end, lists:seq(1, OpsPerAgent)),
                    Parent ! {done, self()}
                end)
            end, AgentIds),
            
            lists:foreach(fun(Pid) ->
                receive {done, Pid} -> ok end
            end, Pids)
        end),
        
        TotalOps = NumAgents * OpsPerAgent,
        OpsRate = TotalOps * 1000000 / OpsTime,
        
        io:format("Operations completed: ~p ms (~.2f ops/sec)~n", 
                  [OpsTime div 1000, OpsRate]),
        
        % Cleanup
        {CleanupTime, _} = timer:tc(fun() ->
            lists:foreach(fun(Id) -> agent:stop_agent(Id) end, AgentIds)
        end),
        
        io:format("Cleanup: ~p ms~n", [CleanupTime div 1000])
    end, Configurations),
    
    io:format("~nScalability test completed.~n").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

calculate_percentiles(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    P50 = lists:nth(round(Len * 0.50), Sorted),
    P95 = lists:nth(round(Len * 0.95), Sorted),
    P99 = lists:nth(round(Len * 0.99), Sorted),
    {P50, P95, P99}.