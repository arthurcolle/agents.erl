%% token_streaming_example.erl
%% Simple example showing how to use the token streaming system

-module(token_streaming_example).

-export([
    simple_token_stream/0,
    chat_with_token_streaming/0,
    document_processor/1,
    real_time_chat/1
]).

%% Simple token stream example
simple_token_stream() ->
    io:format("🌊 Simple Token Stream Example~n"),
    
    % Start a simple token handler
    HandlerPid = spawn(fun() -> simple_handler_loop() end),
    
    % Create token stream FSM
    Options = #{
        max_buffer_size => 10,
        batch_size => 3,
        flush_interval => 200
    },
    
    {ok, StreamPid} = token_stream_fsm:start_link(HandlerPid, Options),
    
    % Process some tokens
    Tokens = [<<"Hello">>, <<"world">>, <<"this">>, <<"is">>, <<"token">>, <<"streaming">>],
    
    io:format("Processing tokens: ~p~n", [Tokens]),
    lists:foreach(fun(Token) ->
        token_stream_fsm:process_token(StreamPid, Token),
        timer:sleep(100)
    end, Tokens),
    
    % Force flush and cleanup
    timer:sleep(500),
    token_stream_fsm:flush_buffer(StreamPid),
    timer:sleep(200),
    
    token_stream_fsm:stop(StreamPid),
    HandlerPid ! stop,
    
    io:format("✅ Simple example completed~n").

%% Chat with token streaming
chat_with_token_streaming() ->
    io:format("💬 Chat with Token Streaming Example~n"),
    
    case os:getenv("OPENAI_API_KEY") of
        false ->
            io:format("⚠️  OPENAI_API_KEY not set - running mock version~n"),
            mock_chat_streaming();
        _ApiKey ->
            io:format("🚀 Starting real chat with token streaming~n"),
            real_chat_streaming()
    end.

%% Document processor with token streaming
document_processor(FilePath) ->
    io:format("📄 Document Processor Example: ~s~n", [FilePath]),
    
    case file:read_file(FilePath) of
        {ok, Content} ->
            process_document_content(Content);
        {error, Reason} ->
            io:format("❌ Failed to read file: ~p~n", [Reason]),
            % Use sample content instead
            SampleContent = <<"This is a sample document for token streaming demonstration. "
                             "It contains multiple sentences and will be processed token by token. "
                             "The system will demonstrate real-time token processing capabilities.">>,
            process_document_content(SampleContent)
    end.

%% Real-time chat interface
real_time_chat(Prompt) ->
    io:format("💬 Real-time Chat Example~n"),
    io:format("Prompt: ~s~n", [Prompt]),
    
    case token_stream_integration:create_chat_token_stream(
        <<"gpt-4.1-mini">>, 
        [#{<<"role">> => <<"user">>, <<"content">> => Prompt}],
        #{
            stream => true,
            max_tokens => 150,
            token_buffer_size => 5,
            token_batch_size => 2,
            token_flush_interval => 100
        }
    ) of
        {ok, StreamInfo} ->
            io:format("✅ Chat stream started~n"),
            
            % Subscribe to token events
            HandlerPid = maps:get(handler_pid, StreamInfo),
            StreamId = maps:get(stream_id, StreamInfo),
            
            % Start a subscriber to display tokens in real-time
            SubscriberPid = spawn(fun() -> real_time_display_loop() end),
            token_stream_handler:subscribe(HandlerPid, StreamId),
            
            % Wait for completion
            timer:sleep(10000),
            
            SubscriberPid ! stop,
            io:format("~n✅ Real-time chat completed~n");
        {error, Reason} ->
            io:format("❌ Failed to start chat stream: ~p~n", [Reason])
    end.

%% Internal helper functions

%% Simple handler loop
simple_handler_loop() ->
    receive
        {token_batch, Tokens} ->
            io:format("📦 Received token batch: ~p~n", [Tokens]),
            simple_handler_loop();
        stop ->
            io:format("🛑 Handler stopped~n"),
            ok;
        Other ->
            io:format("❓ Unknown message: ~p~n", [Other]),
            simple_handler_loop()
    end.

%% Mock chat streaming
mock_chat_streaming() ->
    % Start token stream handler
    {ok, HandlerPid} = token_stream_handler:start_link(mock_chat_handler, #{}),
    
    StreamId = <<"mock_chat_stream">>,
    {ok, StreamPid} = token_stream_handler:create_token_stream(HandlerPid, StreamId, #{}),
    
    % Subscribe to events
    SubscriberPid = spawn(fun() -> chat_display_loop() end),
    token_stream_handler:subscribe(HandlerPid, StreamId),
    
    % Simulate streaming response
    MockResponse = <<"Hello! I'm a mock AI assistant. This response is being streamed token by token to demonstrate the token streaming system capabilities.">>,
    
    Tokens = token_stream_integration:parse_stream_tokens(MockResponse),
    
    io:format("🤖 Assistant: "),
    lists:foreach(fun(Token) ->
        token_stream_fsm:process_token(StreamPid, Token),
        timer:sleep(150) % Simulate streaming delay
    end, Tokens),
    
    timer:sleep(500),
    token_stream_fsm:flush_buffer(StreamPid),
    timer:sleep(200),
    
    SubscriberPid ! stop,
    token_stream_handler:stop(HandlerPid),
    io:format("~n").

%% Real chat streaming
real_chat_streaming() ->
    Model = <<"gpt-4.1-mini">>,
    Messages = [#{<<"role">> => <<"user">>, <<"content">> => <<"Explain token streaming in simple terms.">>}],
    Options = #{
        stream => true,
        max_tokens => 100,
        token_buffer_size => 5,
        token_batch_size => 2
    },
    
    case token_stream_integration:create_chat_token_stream(Model, Messages, Options) of
        {ok, StreamInfo} ->
            io:format("🤖 Assistant: "),
            
            % Subscribe to see tokens in real-time
            HandlerPid = maps:get(handler_pid, StreamInfo),
            StreamId = maps:get(stream_id, StreamInfo),
            
            SubscriberPid = spawn(fun() -> chat_display_loop() end),
            token_stream_handler:subscribe(HandlerPid, StreamId),
            
            % Wait for completion
            timer:sleep(15000),
            
            SubscriberPid ! stop,
            io:format("~n");
        {error, Reason} ->
            io:format("❌ Real chat streaming failed: ~p~n", [Reason])
    end.

%% Process document content
process_document_content(Content) ->
    {ok, HandlerPid} = token_stream_handler:start_link(doc_handler, #{}),
    
    StreamId = <<"document_stream">>,
    StreamOptions = #{
        max_buffer_size => 15,
        batch_size => 5,
        flush_interval => 300
    },
    
    {ok, StreamPid} = token_stream_handler:create_token_stream(HandlerPid, StreamId, StreamOptions),
    
    % Subscribe to events
    SubscriberPid = spawn(fun() -> document_display_loop(0, []) end),
    token_stream_handler:subscribe(HandlerPid, StreamId),
    
    % Parse content into tokens
    Tokens = token_stream_integration:parse_stream_tokens(Content),
    
    io:format("📄 Processing ~p tokens from document...~n", [length(Tokens)]),
    
    % Process tokens in chunks
    ChunkSize = 8,
    TokenChunks = chunk_tokens(Tokens, ChunkSize),
    
    lists:foreach(fun(Chunk) ->
        io:format("📝 Processing chunk: ~p~n", [Chunk]),
        token_stream_fsm:process_tokens(StreamPid, Chunk),
        timer:sleep(500)
    end, TokenChunks),
    
    % Force final flush
    timer:sleep(1000),
    token_stream_fsm:flush_buffer(StreamPid),
    timer:sleep(500),
    
    SubscriberPid ! stop,
    token_stream_handler:stop(HandlerPid),
    
    io:format("✅ Document processing completed~n").

%% Display loops for different scenarios

%% Chat display loop
chat_display_loop() ->
    receive
        {token_stream_event, Event} ->
            Tokens = extract_tokens_from_event(Event),
            lists:foreach(fun(Token) ->
                io:format("~s ", [Token])
            end, Tokens),
            chat_display_loop();
        stop ->
            ok;
        Other ->
            io:format("[Unknown: ~p] ", [Other]),
            chat_display_loop()
    end.

%% Real-time display loop
real_time_display_loop() ->
    receive
        {token_stream_event, Event} ->
            Tokens = extract_tokens_from_event(Event),
            lists:foreach(fun(Token) ->
                io:format("~s ", [Token]),
                timer:sleep(50) % Real-time effect
            end, Tokens),
            real_time_display_loop();
        stop ->
            ok;
        Other ->
            io:format("[~p] ", [Other]),
            real_time_display_loop()
    end.

%% Document display loop
document_display_loop(Count, Acc) ->
    receive
        {token_stream_event, Event} ->
            Tokens = extract_tokens_from_event(Event),
            NewAcc = Acc ++ Tokens,
            io:format("📊 Batch ~p: ~p tokens (total: ~p)~n", [Count + 1, length(Tokens), length(NewAcc)]),
            document_display_loop(Count + 1, NewAcc);
        stop ->
            io:format("📈 Final stats: ~p batches, ~p total tokens~n", [Count, length(Acc)]),
            ok;
        Other ->
            io:format("❓ Unknown: ~p~n", [Other]),
            document_display_loop(Count, Acc)
    end.

%% Utility functions

%% Extract tokens from event
extract_tokens_from_event(Event) ->
    case Event of
        #{tokens := Tokens} -> Tokens;
        _ when is_record(Event, token_event) -> Event#token_event.tokens;
        _ -> []
    end.

%% Chunk tokens into smaller lists
chunk_tokens(Tokens, Size) ->
    chunk_tokens(Tokens, Size, []).

chunk_tokens([], _Size, Acc) ->
    lists:reverse(Acc);
chunk_tokens(Tokens, Size, Acc) when length(Tokens) =< Size ->
    lists:reverse([Tokens | Acc]);
chunk_tokens(Tokens, Size, Acc) ->
    {Chunk, Rest} = lists:split(Size, Tokens),
    chunk_tokens(Rest, Size, [Chunk | Acc]).