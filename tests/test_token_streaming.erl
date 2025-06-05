#!/usr/bin/env escript
%% test_token_streaming.erl
%% Comprehensive test and demo for token streaming system using gen_statem

-define(LOG(Msg), io:format("[TOKEN_DEMO] ~ts~n", [Msg])).
-define(LOG(Msg, Args), io:format("[TOKEN_DEMO] " ++ Msg ++ "~n", Args)).

%% Token processing record
-record(token_event, {
    stream_id :: term(),
    tokens :: [binary()],
    timestamp :: non_neg_integer(),
    metadata = #{} :: map()
}).

main(_) ->
    ?LOG("🌊 Token Streaming System Demo Starting..."),
    
    % Start the applications
    ?LOG("Starting required applications..."),
    start_applications(),
    
    % Demo 1: Basic Token Stream FSM
    ?LOG(""),
    ?LOG("=== DEMO 1: Basic Token Stream FSM ==="),
    demo_basic_token_fsm(),
    
    % Demo 2: Token Stream Handler
    ?LOG(""),
    ?LOG("=== DEMO 2: Token Stream Handler ==="),
    demo_token_handler(),
    
    % Demo 3: Integration with OpenAI Streaming
    ?LOG(""),
    ?LOG("=== DEMO 3: OpenAI Integration ==="),
    demo_openai_integration(),
    
    % Demo 4: Advanced Token Processing
    ?LOG(""),
    ?LOG("=== DEMO 4: Advanced Token Processing ==="),
    demo_advanced_processing(),
    
    % Demo 5: Real-world Scenario
    ?LOG(""),
    ?LOG("=== DEMO 5: Real-world Scenario ==="),
    demo_real_world_scenario(),
    
    ?LOG(""),
    ?LOG("✅ Token streaming demo completed successfully!"),
    ok.

start_applications() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    
    % Add code paths for the project
    code:add_patha("_build/default/lib/openai/ebin"),
    code:add_patha("_build/default/lib/agents/ebin"),
    code:add_patha("_build/default/lib/agent_web/ebin"),
    code:add_patha("apps/openai/ebin"),
    code:add_patha("apps/agents/ebin"),
    code:add_patha("apps/agent_web/ebin"),
    
    % Check if modules are available
    check_module_availability().

check_module_availability() ->
    Modules = [
        {token_stream_fsm, "Token Stream FSM"},
        {token_stream_handler, "Token Stream Handler"},
        {token_stream_integration, "Token Stream Integration"},
        {openai_streaming, "OpenAI Streaming"},
        {colored_logger, "Colored Logger"}
    ],
    
    lists:foreach(fun({Module, Name}) ->
        case code:which(Module) of
            non_existing ->
                ?LOG("⚠️  ~s module not found - run 'make compile' first", [Name]);
            _ ->
                ?LOG("✅ ~s module found", [Name])
        end
    end, Modules).

demo_basic_token_fsm() ->
    ?LOG("Testing basic token stream FSM functionality..."),
    
    % Start a simple token handler process
    HandlerPid = spawn(fun() -> simple_token_handler() end),
    
    % Start token stream FSM
    Options = #{
        max_buffer_size => 5,
        batch_size => 3,
        flush_interval => 100,
        rate_limit => 10
    },
    
    case token_stream_fsm:start_link(HandlerPid, Options) of
        {ok, StreamPid} ->
            ?LOG("✅ Token stream FSM started: ~p", [StreamPid]),
            
            % Test single token processing
            ?LOG("Processing single tokens..."),
            token_stream_fsm:process_token(StreamPid, <<"Hello">>),
            token_stream_fsm:process_token(StreamPid, <<"world">>),
            token_stream_fsm:process_token(StreamPid, <<"from">>),
            
            timer:sleep(200), % Let it process
            
            % Test batch token processing
            ?LOG("Processing token batch..."),
            Tokens = [<<"token">>, <<"streaming">>, <<"system">>, <<"demo">>],
            token_stream_fsm:process_tokens(StreamPid, Tokens),
            
            timer:sleep(200), % Let it process
            
            % Get state and statistics
            case token_stream_fsm:get_state(StreamPid) of
                {ok, State} ->
                    ?LOG("📊 Current state: ~p", [State]);
                {error, StateReason} ->
                    ?LOG("❌ Failed to get state: ~p", [StateReason])
            end,
            
            case token_stream_fsm:get_statistics(StreamPid) of
                {ok, Stats} ->
                    ?LOG("📈 Statistics: ~p", [Stats]);
                {error, StatsReason} ->
                    ?LOG("❌ Failed to get statistics: ~p", [StatsReason])
            end,
            
            % Force flush
            ?LOG("Forcing buffer flush..."),
            token_stream_fsm:flush_buffer(StreamPid),
            
            timer:sleep(100),
            
            % Stop the FSM
            token_stream_fsm:stop(StreamPid),
            HandlerPid ! stop,
            ?LOG("✅ Basic FSM demo completed");
        {error, Reason} ->
            ?LOG("❌ Failed to start token stream FSM: ~p", [Reason]),
            HandlerPid ! stop
    end.

demo_token_handler() ->
    ?LOG("Testing token stream handler functionality..."),
    
    % Start token stream handler
    case token_stream_handler:start_link(test_handler, #{}) of
        {ok, HandlerPid} ->
            ?LOG("✅ Token stream handler started: ~p", [HandlerPid]),
            
            % Create a token stream
            StreamId = <<"test_stream_1">>,
            StreamOptions = #{
                max_buffer_size => 8,
                batch_size => 4,
                flush_interval => 150
            },
            
            case token_stream_handler:create_token_stream(HandlerPid, StreamId, StreamOptions) of
                {ok, StreamPid} ->
                    ?LOG("✅ Token stream created: ~p", [StreamId]),
                    
                    % Subscribe to the stream
                    SubscriberPid = spawn(fun() -> token_subscriber() end),
                    token_stream_handler:subscribe(HandlerPid, StreamId),
                    
                    % Process some tokens
                    ?LOG("Processing tokens through handler..."),
                    token_stream_fsm:process_tokens(StreamPid, [<<"Advanced">>, <<"token">>, <<"processing">>]),
                    
                    timer:sleep(200),
                    
                    % Get active streams
                    case token_stream_handler:get_active_streams(HandlerPid) of
                        {ok, Streams} ->
                            ?LOG("📋 Active streams: ~p", [length(Streams)]);
                        {error, StreamsReason} ->
                            ?LOG("❌ Failed to get active streams: ~p", [StreamsReason])
                    end,
                    
                    % Get stream info
                    case token_stream_handler:get_stream_info(HandlerPid, StreamId) of
                        {ok, Info} ->
                            ?LOG("ℹ️  Stream info: ~p", [Info]);
                        {error, InfoReason} ->
                            ?LOG("❌ Failed to get stream info: ~p", [InfoReason])
                    end,
                    
                    timer:sleep(200),
                    
                    % Cleanup
                    token_stream_handler:unsubscribe(HandlerPid, StreamId),
                    SubscriberPid ! stop,
                    
                    ?LOG("✅ Token handler demo completed");
                {error, Reason} ->
                    ?LOG("❌ Failed to create token stream: ~p", [Reason])
            end,
            
            % Stop handler
            token_stream_handler:stop(HandlerPid);
        {error, Reason} ->
            ?LOG("❌ Failed to start token stream handler: ~p", [Reason])
    end.

demo_openai_integration() ->
    ?LOG("Testing OpenAI streaming integration..."),
    
    % Check if API key is available
    case os:getenv("OPENAI_API_KEY") of
        false ->
            ?LOG("⚠️  OPENAI_API_KEY not set - running mock integration demo"),
            demo_mock_integration();
        ApiKey when length(ApiKey) > 10 ->
            ?LOG("✅ API key found, running real integration demo"),
            demo_real_integration()
    end.

demo_mock_integration() ->
    ?LOG("🎭 Running mock OpenAI integration..."),
    
    % Start token stream handler
    case token_stream_handler:start_link(integration_handler, #{}) of
        {ok, HandlerPid} ->
            StreamId = <<"mock_integration_stream">>,
            
            case token_stream_handler:create_token_stream(HandlerPid, StreamId, #{}) of
                {ok, StreamPid} ->
                    ?LOG("✅ Integration stream created"),
                    
                    % Subscribe to stream events
                    SubscriberPid = spawn(fun() -> integration_subscriber() end),
                    token_stream_handler:subscribe(HandlerPid, StreamId),
                    
                    % Simulate OpenAI streaming events
                    ?LOG("Simulating OpenAI streaming events..."),
                    MockEvents = [
                        #{type => content, data => <<"Hello">>},
                        #{type => content, data => <<" there!">>},
                        #{type => content, data => <<" How can">>},
                        #{type => content, data => <<" I help">>},
                        #{type => content, data => <<" you today?">>}
                    ],
                    
                    lists:foreach(fun(Event) ->
                        % Extract and process tokens
                        Tokens = token_stream_integration:extract_tokens_from_event(Event),
                        case Tokens of
                            [] -> ok;
                            _ -> 
                                ?LOG("📥 Processing tokens: ~p", [Tokens]),
                                token_stream_fsm:process_tokens(StreamPid, Tokens)
                        end,
                        timer:sleep(100)
                    end, MockEvents),
                    
                    timer:sleep(300),
                    
                    % Cleanup
                    SubscriberPid ! stop,
                    ?LOG("✅ Mock integration completed");
                {error, StreamReason} ->
                    ?LOG("❌ Failed to create integration stream: ~p", [StreamReason])
            end,
            
            token_stream_handler:stop(HandlerPid);
        {error, HandlerReason} ->
            ?LOG("❌ Failed to start integration handler: ~p", [HandlerReason])
    end.

demo_real_integration() ->
    ?LOG("🚀 Running real OpenAI integration..."),
    
    Model = <<"gpt-4.1-mini">>,
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Count from 1 to 10 slowly, one number per response.">>}
    ],
    Options = #{
        stream => true,
        max_tokens => 50,
        token_buffer_size => 10,
        token_batch_size => 3,
        token_flush_interval => 100
    },
    
    case token_stream_integration:create_chat_token_stream(Model, Messages, Options) of
        {ok, StreamInfo} ->
            ?LOG("✅ Real integration stream started"),
            ?LOG("📊 Stream info: ~p", [StreamInfo]),
            
            % Subscribe to token events
            HandlerPid = maps:get(handler_pid, StreamInfo),
            StreamId = maps:get(stream_id, StreamInfo),
            
            SubscriberPid = spawn(fun() -> real_integration_subscriber() end),
            token_stream_handler:subscribe(HandlerPid, StreamId),
            
            % Wait for completion
            timer:sleep(10000),
            
            % Get final statistics
            case token_stream_handler:get_stream_info(HandlerPid, StreamId) of
                {ok, Info} ->
                    ?LOG("📈 Final stream info: ~p", [Info]);
                {error, Reason} ->
                    ?LOG("❌ Failed to get final info: ~p", [Reason])
            end,
            
            SubscriberPid ! stop,
            ?LOG("✅ Real integration completed");
        {error, Reason} ->
            ?LOG("❌ Real integration failed: ~p", [Reason])
    end.

demo_advanced_processing() ->
    ?LOG("Testing advanced token processing features..."),
    
    % Start handler with custom options
    HandlerOptions = #{
        default_buffer_size => 15,
        default_batch_size => 5,
        default_rate_limit => 20
    },
    
    case token_stream_handler:start_link(advanced_handler, HandlerOptions) of
        {ok, HandlerPid} ->
            % Create multiple streams with different characteristics
            Streams = [
                {<<"fast_stream">>, #{batch_size => 2, flush_interval => 50}},
                {<<"slow_stream">>, #{batch_size => 10, flush_interval => 300}},
                {<<"rate_limited_stream">>, #{rate_limit => 5, max_buffer_size => 20}}
            ],
            
            StreamPids = lists:map(fun({StreamId, Options}) ->
                case token_stream_handler:create_token_stream(HandlerPid, StreamId, Options) of
                    {ok, StreamPid} ->
                        ?LOG("✅ Created ~s: ~p", [StreamId, StreamPid]),
                        {StreamId, StreamPid};
                    {error, Reason} ->
                        ?LOG("❌ Failed to create ~s: ~p", [StreamId, Reason]),
                        {StreamId, undefined}
                end
            end, Streams),
            
            % Process different types of content
            ?LOG("Processing different content types..."),
            
            % Fast stream - quick bursts
            case lists:keyfind(<<"fast_stream">>, 1, StreamPids) of
                {_, FastPid} when FastPid =/= undefined ->
                    FastTokens = [<<"Quick">>, <<"burst">>, <<"of">>, <<"tokens">>],
                    token_stream_fsm:process_tokens(FastPid, FastTokens);
                _ -> ok
            end,
            
            % Slow stream - larger batches
            case lists:keyfind(<<"slow_stream">>, 1, StreamPids) of
                {_, SlowPid} when SlowPid =/= undefined ->
                    SlowTokens = [<<"This">>, <<"is">>, <<"a">>, <<"longer">>, <<"sequence">>, 
                                 <<"of">>, <<"tokens">>, <<"for">>, <<"testing">>, <<"batching">>],
                    token_stream_fsm:process_tokens(SlowPid, SlowTokens);
                _ -> ok
            end,
            
            % Rate limited stream - continuous feeding
            case lists:keyfind(<<"rate_limited_stream">>, 1, StreamPids) of
                {_, RatePid} when RatePid =/= undefined ->
                    spawn(fun() ->
                        lists:foreach(fun(N) ->
                            Token = iolist_to_binary(io_lib:format("token_~p", [N])),
                            token_stream_fsm:process_token(RatePid, Token),
                            timer:sleep(50)
                        end, lists:seq(1, 15))
                    end);
                _ -> ok
            end,
            
            timer:sleep(2000), % Let processing complete
            
            % Get statistics for all streams
            lists:foreach(fun({StreamId, _}) ->
                case token_stream_handler:get_stream_info(HandlerPid, StreamId) of
                    {ok, Info} ->
                        Stats = maps:get(statistics, Info, #{}),
                        ?LOG("📊 ~s stats: ~p", [StreamId, Stats]);
                    {error, Reason} ->
                        ?LOG("❌ Failed to get ~s stats: ~p", [StreamId, Reason])
                end
            end, StreamPids),
            
            token_stream_handler:stop(HandlerPid),
            ?LOG("✅ Advanced processing demo completed");
        {error, Reason} ->
            ?LOG("❌ Failed to start advanced handler: ~p", [Reason])
    end.

demo_real_world_scenario() ->
    ?LOG("Testing real-world token streaming scenario..."),
    
    % Scenario: Processing a document with multiple sections
    Document = [
        <<"Introduction: This document demonstrates advanced token streaming capabilities.">>,
        <<"Section 1: Token streaming enables real-time processing of text content.">>,
        <<"Section 2: The system uses gen_statem for robust state management.">>,
        <<"Section 3: Integration with OpenAI APIs provides seamless streaming.">>,
        <<"Conclusion: Token streaming enhances user experience significantly.">>
    ],
    
    case token_stream_handler:start_link(document_handler, #{}) of
        {ok, HandlerPid} ->
            StreamId = <<"document_processing_stream">>,
            StreamOptions = #{
                max_buffer_size => 20,
                batch_size => 8,
                flush_interval => 200,
                rate_limit => 15
            },
            
            case token_stream_handler:create_token_stream(HandlerPid, StreamId, StreamOptions) of
                {ok, StreamPid} ->
                    ?LOG("✅ Document processing stream created"),
                    
                    % Subscribe to events
                    SubscriberPid = spawn(fun() -> document_subscriber() end),
                    token_stream_handler:subscribe(HandlerPid, StreamId),
                    
                    % Process document sections sequentially
                    lists:foreach(fun(Section) ->
                        ?LOG("📄 Processing section: ~s", [binary:part(Section, 0, min(30, byte_size(Section)))]),
                        
                        % Parse section into tokens
                        Tokens = token_stream_integration:parse_stream_tokens(Section),
                        
                        % Process tokens in chunks to simulate streaming
                        ChunkSize = 3,
                        TokenChunks = chunk_list(Tokens, ChunkSize),
                        
                        lists:foreach(fun(Chunk) ->
                            token_stream_fsm:process_tokens(StreamPid, Chunk),
                            timer:sleep(100) % Simulate processing delay
                        end, TokenChunks),
                        
                        timer:sleep(300) % Section boundary
                    end, Document),
                    
                    % Force final flush
                    token_stream_fsm:flush_buffer(StreamPid),
                    timer:sleep(500),
                    
                    % Get final statistics
                    case token_stream_handler:get_stream_info(HandlerPid, StreamId) of
                        {ok, Info} ->
                            Stats = maps:get(statistics, Info, #{}),
                            TokensProcessed = maps:get(tokens_processed, Stats, 0),
                            BatchesFlushed = maps:get(batches_flushed, Stats, 0),
                            ?LOG("📈 Document processing completed:"),
                            ?LOG("   - Tokens processed: ~p", [TokensProcessed]),
                            ?LOG("   - Batches flushed: ~p", [BatchesFlushed]);
                        {error, Reason} ->
                            ?LOG("❌ Failed to get final stats: ~p", [Reason])
                    end,
                    
                    SubscriberPid ! stop,
                    ?LOG("✅ Real-world scenario completed");
                {error, Reason} ->
                    ?LOG("❌ Failed to create document stream: ~p", [Reason])
            end,
            
            token_stream_handler:stop(HandlerPid);
        {error, Reason} ->
            ?LOG("❌ Failed to start document handler: ~p", [Reason])
    end.

%% Helper processes and functions

simple_token_handler() ->
    ?LOG("🤖 Simple token handler started"),
    simple_token_handler_loop(0).

simple_token_handler_loop(Count) ->
    receive
        {token_batch, Tokens} ->
            ?LOG("📦 Received batch of ~p tokens: ~p", [length(Tokens), Tokens]),
            simple_token_handler_loop(Count + length(Tokens));
        stop ->
            ?LOG("🛑 Simple token handler stopping (processed ~p tokens)", [Count]),
            ok;
        Other ->
            ?LOG("❓ Unknown message: ~p", [Other]),
            simple_token_handler_loop(Count)
    after 5000 ->
        ?LOG("⏰ Simple token handler timeout"),
        ok
    end.

token_subscriber() ->
    ?LOG("👂 Token subscriber started"),
    token_subscriber_loop(0).

token_subscriber_loop(Count) ->
    receive
        {token_stream_event, Event} ->
            ?LOG("🎯 Token event received: ~p", [Event]),
            token_subscriber_loop(Count + 1);
        stop ->
            ?LOG("🛑 Token subscriber stopping (received ~p events)", [Count]),
            ok;
        Other ->
            ?LOG("❓ Unknown message: ~p", [Other]),
            token_subscriber_loop(Count)
    after 3000 ->
        ?LOG("⏰ Token subscriber timeout"),
        ok
    end.

integration_subscriber() ->
    ?LOG("🔗 Integration subscriber started"),
    integration_subscriber_loop(0).

integration_subscriber_loop(Count) ->
    receive
        {token_stream_event, Event} ->
            ?LOG("🔗 Integration event: ~p", [Event]),
            integration_subscriber_loop(Count + 1);
        stop ->
            ?LOG("🛑 Integration subscriber stopping (received ~p events)", [Count]),
            ok;
        Other ->
            ?LOG("❓ Unknown message: ~p", [Other]),
            integration_subscriber_loop(Count)
    after 2000 ->
        ?LOG("⏰ Integration subscriber timeout"),
        ok
    end.

real_integration_subscriber() ->
    ?LOG("🌐 Real integration subscriber started"),
    real_integration_subscriber_loop(0).

real_integration_subscriber_loop(Count) ->
    receive
        {token_stream_event, Event} ->
            ?LOG("🌐 Real event: ~p", [Event]),
            real_integration_subscriber_loop(Count + 1);
        stop ->
            ?LOG("🛑 Real integration subscriber stopping (received ~p events)", [Count]),
            ok;
        Other ->
            ?LOG("❓ Unknown message: ~p", [Other]),
            real_integration_subscriber_loop(Count)
    after 12000 ->
        ?LOG("⏰ Real integration subscriber timeout"),
        ok
    end.

document_subscriber() ->
    ?LOG("📄 Document subscriber started"),
    document_subscriber_loop(0, []).

document_subscriber_loop(Count, Acc) ->
    receive
        {token_stream_event, Event} ->
            Tokens = case Event of
                #token_event{tokens = TokenList} -> TokenList;
                #{tokens := TokenList} -> TokenList;
                _ -> []
            end,
            NewAcc = Acc ++ Tokens,
            ?LOG("📄 Document tokens (~p total): ~p", [length(NewAcc), Tokens]),
            document_subscriber_loop(Count + 1, NewAcc);
        stop ->
            ?LOG("🛑 Document subscriber stopping:"),
            ?LOG("   - Events received: ~p", [Count]),
            ?LOG("   - Total tokens: ~p", [length(Acc)]),
            ok;
        Other ->
            ?LOG("❓ Unknown message: ~p", [Other]),
            document_subscriber_loop(Count, Acc)
    after 5000 ->
        ?LOG("⏰ Document subscriber timeout"),
        ok
    end.

%% Utility functions

chunk_list(List, Size) ->
    chunk_list(List, Size, []).

chunk_list([], _Size, Acc) ->
    lists:reverse(Acc);
chunk_list(List, Size, Acc) when length(List) =< Size ->
    lists:reverse([List | Acc]);
chunk_list(List, Size, Acc) ->
    {Chunk, Rest} = lists:split(Size, List),
    chunk_list(Rest, Size, [Chunk | Acc]).