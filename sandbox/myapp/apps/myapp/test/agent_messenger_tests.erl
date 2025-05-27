%%%-------------------------------------------------------------------
%%% @doc
%%% Agent Messenger Tests
%%% Tests for inter-agent messaging
%%% @end
%%%-------------------------------------------------------------------
-module(agent_messenger_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(myapp),
    % Start test agents
    {ok, Agent1} = test_agent:start_link(<<"test_agent_1">>),
    {ok, Agent2} = test_agent:start_link(<<"test_agent_2">>),
    {ok, Agent3} = test_agent:start_link(<<"test_agent_3">>),
    #{agent1 => Agent1, agent2 => Agent2, agent3 => Agent3}.

teardown(#{agent1 := Agent1, agent2 := Agent2, agent3 := Agent3}) ->
    test_agent:stop(Agent1),
    test_agent:stop(Agent2),
    test_agent:stop(Agent3),
    application:stop(myapp),
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

agent_messenger_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Basic message sending", fun test_basic_messaging/0},
      {"Broadcast messaging", fun test_broadcast_messaging/0},
      {"Request-response pattern", fun test_request_response/0},
      {"Message routing", fun test_message_routing/0},
      {"Error handling", fun test_error_handling/0},
      {"Message queueing", fun test_message_queueing/0},
      {"Concurrent messaging", fun test_concurrent_messaging/0},
      {"Message priorities", fun test_message_priorities/0},
      {"Message timeouts", fun test_message_timeouts/0},
      {"Large messages", fun test_large_messages/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_messaging() ->
    From = <<"sender_agent">>,
    To = <<"receiver_agent">>,
    Message = #{type => <<"greeting">>, content => <<"Hello, Agent!">>},
    
    % Register receiver
    ReceiverPid = self(),
    agent_messenger:register_handler(To, fun(Msg) ->
        ReceiverPid ! {received, Msg}
    end),
    
    % Send message
    {ok, MsgId} = agent_messenger:send_message(From, To, Message),
    ?assert(is_binary(MsgId)),
    
    % Verify receipt
    receive
        {received, ReceivedMsg} ->
            ?assertEqual(From, maps:get(from, ReceivedMsg)),
            ?assertEqual(To, maps:get(to, ReceivedMsg)),
            ?assertEqual(Message, maps:get(message, ReceivedMsg)),
            ?assertEqual(MsgId, maps:get(id, ReceivedMsg))
    after ?TIMEOUT ->
        ?assert(false)
    end.

test_broadcast_messaging() ->
    Sender = <<"broadcast_sender">>,
    Recipients = [<<"recipient_1">>, <<"recipient_2">>, <<"recipient_3">>],
    Message = #{type => <<"announcement">>, content => <<"System update">>},
    
    % Register all recipients
    Parent = self(),
    lists:foreach(fun(Recipient) ->
        agent_messenger:register_handler(Recipient, fun(Msg) ->
            Parent ! {received, Recipient, Msg}
        end)
    end, Recipients),
    
    % Broadcast message
    {ok, MsgIds} = agent_messenger:broadcast_message(Sender, Recipients, Message),
    ?assertEqual(length(Recipients), length(MsgIds)),
    
    % Verify all recipients received the message
    ReceivedBy = lists:map(fun(_) ->
        receive
            {received, Recipient, _Msg} -> Recipient
        after ?TIMEOUT ->
            timeout
        end
    end, Recipients),
    
    ?assertEqual(lists:sort(Recipients), lists:sort(ReceivedBy)).

test_request_response() ->
    Client = <<"client_agent">>,
    Server = <<"server_agent">>,
    Request = #{type => <<"query">>, question => <<"What is 2+2?">>},
    
    % Register server handler
    agent_messenger:register_handler(Server, fun(Msg) ->
        case maps:get(message, Msg) of
            #{type := <<"query">>, question := Q} ->
                Response = #{type => <<"response">>, answer => <<"4">>, question => Q},
                agent_messenger:reply_to_message(maps:get(id, Msg), Response);
            _ ->
                ok
        end
    end),
    
    % Send request and wait for response
    {ok, Response} = agent_messenger:request(Client, Server, Request, ?TIMEOUT),
    ?assertEqual(<<"response">>, maps:get(type, Response)),
    ?assertEqual(<<"4">>, maps:get(answer, Response)).

test_message_routing() ->
    % Test routing through intermediary
    Source = <<"source_agent">>,
    Router = <<"router_agent">>,
    Destination = <<"destination_agent">>,
    
    Parent = self(),
    
    % Set up destination handler
    agent_messenger:register_handler(Destination, fun(Msg) ->
        Parent ! {destination_received, Msg}
    end),
    
    % Set up router to forward messages
    agent_messenger:register_handler(Router, fun(Msg) ->
        case maps:get(message, Msg) of
            #{forward_to := FinalDest} = OrigMsg ->
                ForwardMsg = maps:remove(forward_to, OrigMsg),
                agent_messenger:send_message(Router, FinalDest, ForwardMsg);
            _ ->
                ok
        end
    end),
    
    % Send message through router
    RoutedMessage = #{
        type => <<"routed">>,
        content => <<"Secret message">>,
        forward_to => Destination
    },
    {ok, _} = agent_messenger:send_message(Source, Router, RoutedMessage),
    
    % Verify destination received it
    receive
        {destination_received, RecvMsg} ->
            InnerMsg = maps:get(message, RecvMsg),
            ?assertEqual(<<"routed">>, maps:get(type, InnerMsg)),
            ?assertEqual(<<"Secret message">>, maps:get(content, InnerMsg)),
            ?assertNot(maps:is_key(forward_to, InnerMsg))
    after ?TIMEOUT ->
        ?assert(false)
    end.

test_error_handling() ->
    Sender = <<"error_sender">>,
    
    % Send to non-existent agent
    Result1 = agent_messenger:send_message(Sender, <<"non_existent">>, #{}),
    ?assertMatch({error, _}, Result1),
    
    % Invalid message format
    ?assertError(badarg, agent_messenger:send_message(not_binary, <<"target">>, #{})),
    ?assertError(badarg, agent_messenger:send_message(Sender, not_binary, #{})),
    ?assertError(badarg, agent_messenger:send_message(Sender, <<"target">>, not_a_map)),
    
    % Handler that crashes
    CrashAgent = <<"crash_agent">>,
    agent_messenger:register_handler(CrashAgent, fun(_) ->
        error(deliberate_crash)
    end),
    
    % Message should still be delivered (handler crash shouldn't affect sender)
    {ok, _} = agent_messenger:send_message(Sender, CrashAgent, #{test => <<"crash">>}),
    
    % Timeout on request
    NoReplyAgent = <<"no_reply_agent">>,
    agent_messenger:register_handler(NoReplyAgent, fun(_) -> ok end),
    
    Result2 = agent_messenger:request(Sender, NoReplyAgent, #{}, 100),
    ?assertMatch({error, timeout}, Result2).

test_message_queueing() ->
    QueueAgent = <<"queue_agent">>,
    NumMessages = 100,
    
    % Set up slow handler
    ReceivedRef = make_ref(),
    Parent = self(),
    agent_messenger:register_handler(QueueAgent, fun(Msg) ->
        timer:sleep(10), % Simulate slow processing
        Parent ! {ReceivedRef, maps:get(message, Msg)}
    end),
    
    % Send many messages quickly
    Sender = <<"queue_sender">>,
    Messages = [#{index => N, data => <<"test">>} || N <- lists:seq(1, NumMessages)],
    
    lists:foreach(fun(Msg) ->
        {ok, _} = agent_messenger:send_message(Sender, QueueAgent, Msg)
    end, Messages),
    
    % Collect all messages
    Received = lists:map(fun(_) ->
        receive
            {ReceivedRef, Msg} -> Msg
        after ?TIMEOUT ->
            timeout
        end
    end, lists:seq(1, NumMessages)),
    
    % Verify all messages received in order
    ?assertEqual(NumMessages, length(Received)),
    ReceivedIndices = [maps:get(index, M) || M <- Received],
    ?assertEqual(lists:seq(1, NumMessages), ReceivedIndices).

test_concurrent_messaging() ->
    NumSenders = 50,
    NumMessages = 10,
    Target = <<"concurrent_target">>,
    
    % Set up counting handler
    Counter = ets:new(counter, [public, set]),
    ets:insert(Counter, {count, 0}),
    
    agent_messenger:register_handler(Target, fun(_Msg) ->
        ets:update_counter(Counter, count, 1)
    end),
    
    % Spawn concurrent senders
    Parent = self(),
    Pids = lists:map(fun(SenderId) ->
        spawn(fun() ->
            SenderName = list_to_binary("sender_" ++ integer_to_list(SenderId)),
            lists:foreach(fun(MsgNum) ->
                Msg = #{sender => SenderId, msg_num => MsgNum},
                {ok, _} = agent_messenger:send_message(SenderName, Target, Msg)
            end, lists:seq(1, NumMessages)),
            Parent ! {done, self()}
        end)
    end, lists:seq(1, NumSenders)),
    
    % Wait for all senders
    lists:foreach(fun(Pid) ->
        receive
            {done, Pid} -> ok
        after ?TIMEOUT ->
            ?assert(false)
        end
    end, Pids),
    
    % Give time for all messages to be processed
    timer:sleep(500),
    
    % Verify all messages were received
    [{count, TotalReceived}] = ets:lookup(Counter, count),
    ?assertEqual(NumSenders * NumMessages, TotalReceived),
    
    ets:delete(Counter).

test_message_priorities() ->
    PriorityAgent = <<"priority_agent">>,
    ReceivedOrder = ets:new(received_order, [public, ordered_set]),
    Counter = ets:new(counter, [public, set]),
    ets:insert(Counter, {next, 1}),
    
    % Handler that records receipt order
    agent_messenger:register_handler(PriorityAgent, fun(Msg) ->
        Order = ets:update_counter(Counter, next, 1) - 1,
        Priority = maps:get(priority, maps:get(message, Msg), normal),
        ets:insert(ReceivedOrder, {Order, Priority})
    end),
    
    % Send messages with different priorities
    Sender = <<"priority_sender">>,
    Messages = [
        #{content => <<"low_1">>, priority => low},
        #{content => <<"high_1">>, priority => high},
        #{content => <<"normal_1">>, priority => normal},
        #{content => <<"high_2">>, priority => high},
        #{content => <<"critical_1">>, priority => critical},
        #{content => <<"low_2">>, priority => low},
        #{content => <<"normal_2">>, priority => normal}
    ],
    
    lists:foreach(fun(Msg) ->
        agent_messenger:send_message(Sender, PriorityAgent, Msg)
    end, Messages),
    
    % Wait for processing
    timer:sleep(500),
    
    % Verify priority ordering (critical > high > normal > low)
    AllReceived = ets:tab2list(ReceivedOrder),
    Priorities = [P || {_, P} <- lists:sort(AllReceived)],
    
    % Critical should be first
    ?assertEqual(critical, hd(Priorities)),
    
    ets:delete(ReceivedOrder),
    ets:delete(Counter).

test_message_timeouts() ->
    TimeoutAgent = <<"timeout_agent">>,
    Parent = self(),
    
    % Handler with configurable delay
    agent_messenger:register_handler(TimeoutAgent, fun(Msg) ->
        Delay = maps:get(delay, maps:get(message, Msg), 0),
        timer:sleep(Delay),
        Parent ! {processed, maps:get(id, Msg)}
    end),
    
    Sender = <<"timeout_sender">>,
    
    % Test message with TTL
    {ok, MsgId1} = agent_messenger:send_message(
        Sender, TimeoutAgent, 
        #{delay => 100}, 
        #{ttl => 200}  % Message expires in 200ms
    ),
    
    receive
        {processed, MsgId1} -> ok
    after 300 ->
        ?assert(false)  % Should have been processed
    end,
    
    % Test expired message
    {ok, MsgId2} = agent_messenger:send_message(
        Sender, TimeoutAgent,
        #{delay => 300},
        #{ttl => 100}  % Message expires before processing
    ),
    
    receive
        {processed, MsgId2} -> 
            ?assert(false)  % Should not be processed
    after 400 ->
        ok  % Expected - message should have expired
    end.

test_large_messages() ->
    LargeAgent = <<"large_agent">>,
    Parent = self(),
    
    % Handler that verifies message integrity
    agent_messenger:register_handler(LargeAgent, fun(Msg) ->
        Content = maps:get(message, Msg),
        Size = maps:get(size, Content),
        Data = maps:get(data, Content),
        Hash = maps:get(hash, Content),
        
        % Verify size
        ?assertEqual(Size, byte_size(Data)),
        
        % Verify hash
        ComputedHash = crypto:hash(sha256, Data),
        ?assertEqual(Hash, ComputedHash),
        
        Parent ! {verified, Size}
    end),
    
    Sender = <<"large_sender">>,
    
    % Test various message sizes
    Sizes = [1024, 10240, 102400, 1024000],  % 1KB, 10KB, 100KB, 1MB
    
    lists:foreach(fun(Size) ->
        Data = crypto:strong_rand_bytes(Size),
        Hash = crypto:hash(sha256, Data),
        
        Message = #{
            size => Size,
            data => Data,
            hash => Hash
        },
        
        {ok, _} = agent_messenger:send_message(Sender, LargeAgent, Message),
        
        receive
            {verified, Size} -> ok
        after ?TIMEOUT ->
            ?assert(false)
        end
    end, Sizes).