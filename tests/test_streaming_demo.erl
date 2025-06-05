#!/usr/bin/env escript
%% test_streaming_demo.erl
%% Demo script showing OpenAI streaming functionality

-define(LOG(Msg), io:format("[DEMO] ~ts~n", [Msg])).
-define(LOG(Msg, Args), io:format("[DEMO] " ++ Msg ++ "~n", Args)).

main(_) ->
    ?LOG("🌊 OpenAI Streaming Demo Starting..."),
    
    % Start the applications
    ?LOG("Starting required applications..."),
    start_applications(),
    
    % Demo 1: Chat Completions Streaming
    ?LOG(""),
    ?LOG("=== DEMO 1: Chat Completions Streaming ==="),
    demo_chat_streaming(),
    
    % Demo 2: Responses API Streaming
    ?LOG(""),
    ?LOG("=== DEMO 2: Responses API Streaming ==="),
    demo_responses_streaming(),
    
    ?LOG(""),
    ?LOG("✅ Demo completed successfully!"),
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
    case code:which(openai_chat) of
        non_existing ->
            ?LOG("⚠️  OpenAI modules not compiled - run 'make compile' first");
        _ ->
            ?LOG("✅ OpenAI modules found")
    end,
    
    case code:which(openai_streaming) of
        non_existing ->
            ?LOG("⚠️  OpenAI streaming module not found");
        _ ->
            ?LOG("✅ OpenAI streaming module found")
    end.

demo_chat_streaming() ->
    ?LOG("Testing Chat Completions with streaming..."),
    
    % Check if API key is set
    case os:getenv("OPENAI_API_KEY") of
        false ->
            ?LOG("⚠️  OPENAI_API_KEY not set - skipping actual API calls"),
            demo_mock_streaming();
        ApiKey when length(ApiKey) > 10 ->
            ?LOG("✅ API key found, making real streaming request"),
            demo_real_streaming()
    end.

demo_responses_streaming() ->
    ?LOG("Testing Responses API with streaming..."),
    
    % Check if API key is set
    case os:getenv("OPENAI_API_KEY") of
        false ->
            ?LOG("⚠️  OPENAI_API_KEY not set - skipping actual API calls"),
            demo_mock_responses_streaming();
        ApiKey when length(ApiKey) > 10 ->
            ?LOG("✅ API key found, making real streaming request"),
            demo_real_responses_streaming()
    end.

demo_mock_streaming() ->
    ?LOG("🎭 Running mock streaming demo..."),
    
    % Simulate streaming events
    Events = [
        #{type => content, data => <<"Hello">>},
        #{type => content, data => <<" there">>},
        #{type => content, data => <<"! How">>},
        #{type => content, data => <<" can I">>},
        #{type => content, data => <<" help you">>},
        #{type => content, data => <<" today?">>},
        #{type => finish, reason => stop}
    ],
    
    ?LOG("Simulating streaming events..."),
    lists:foreach(fun(Event) ->
        FormattedEvent = openai_streaming:format_stream_event(Event),
        ?LOG("📥 Event: ~p", [FormattedEvent]),
        timer:sleep(100)  % Simulate streaming delay
    end, Events),
    
    ?LOG("✅ Mock streaming completed").

demo_mock_responses_streaming() ->
    ?LOG("🎭 Running mock Responses API streaming demo..."),
    
    % Simulate semantic events
    Events = [
        #{<<"type">> => <<"response.created">>, <<"id">> => <<"resp_123">>, <<"object">> => <<"response">>},
        #{<<"type">> => <<"response.in_progress">>, <<"id">> => <<"resp_123">>},
        #{<<"type">> => <<"response.output_item.added">>, <<"output_item">> => #{<<"type">> => <<"text">>}},
        #{<<"type">> => <<"response.content_part.added">>, <<"content_part">> => #{<<"type">> => <<"text">>}},
        #{<<"type">> => <<"response.output_text.delta">>, <<"delta">> => <<"Hello from the Responses API!">>},
        #{<<"type">> => <<"response.output_text.done">>, <<"output_text">> => <<"Hello from the Responses API!">>},
        #{<<"type">> => <<"response.output_item.done">>, <<"output_item">> => #{<<"type">> => <<"text">>}},
        #{<<"type">> => <<"response.completed">>, <<"id">> => <<"resp_123">>}
    ],
    
    ?LOG("Simulating semantic streaming events..."),
    lists:foreach(fun(Event) ->
        FormattedEvent = openai_streaming:format_stream_event(Event),
        ?LOG("📥 Semantic Event: ~p", [FormattedEvent]),
        timer:sleep(200)  % Simulate streaming delay
    end, Events),
    
    ?LOG("✅ Mock Responses API streaming completed").

demo_real_streaming() ->
    ?LOG("🚀 Making real streaming request to OpenAI..."),
    
    Model = <<"gpt-4.1-mini">>,
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Say 'hello world' and explain why it's a common first program.">>}
    ],
    Options = #{stream => true, max_tokens => 100},
    
    % Create event handler
    EventHandler = fun(Event) ->
        FormattedEvent = openai_streaming:format_stream_event(Event),
        case maps:get(type, FormattedEvent, undefined) of
            <<"text_delta">> ->
                Content = maps:get(content, FormattedEvent, <<>>),
                io:format("~s", [Content]),
                continue;
            <<"completion_finished">> ->
                io:format("~n"),
                ?LOG("🏁 Completion finished: ~p", [maps:get(finish_reason, FormattedEvent)]),
                stop;
            _ ->
                ?LOG("📥 Event: ~p", [maps:get(type, FormattedEvent, unknown)]),
                continue
        end
    end,
    
    % Start streaming
    case openai_streaming:create_streaming_chat(Model, Messages, Options) of
        {ok, _StreamPid} ->
            ?LOG("🌊 Streaming started, processing events..."),
            io:format("Response: "),
            case openai_streaming:handle_stream_events(EventHandler, 30000) of
                {ok, Events} ->
                    ?LOG("✅ Streaming completed with ~p events", [length(Events)]);
                {error, Reason} ->
                    ?LOG("❌ Streaming failed: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG("❌ Failed to start streaming: ~p", [Reason])
    end.

demo_real_responses_streaming() ->
    ?LOG("🚀 Making real streaming request to Responses API..."),
    
    Input = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Explain what streaming APIs are and why they're useful.">>}
    ],
    Model = <<"gpt-4.1-mini">>,
    Options = #{stream => true, max_output_tokens => 150},
    
    % Create event handler for semantic events
    EventHandler = fun(Event) ->
        case maps:get(<<"type">>, Event, undefined) of
            <<"response.output_text.delta">> ->
                Delta = maps:get(<<"delta">>, Event, <<>>),
                io:format("~s", [Delta]),
                continue;
            <<"response.completed">> ->
                io:format("~n"),
                ?LOG("🏁 Response completed: ~p", [maps:get(<<"id">>, Event)]),
                stop;
            <<"response.failed">> ->
                ?LOG("❌ Response failed: ~p", [Event]),
                {error, response_failed};
            <<"error">> ->
                ?LOG("❌ Error event: ~p", [Event]),
                {error, api_error};
            Type when Type =/= undefined ->
                ?LOG("📥 Semantic Event: ~s", [Type]),
                continue;
            _ ->
                ?LOG("📥 Unknown Event: ~p", [Event]),
                continue
        end
    end,
    
    % Start streaming
    case openai_streaming:create_streaming_response(Input, Model, Options) of
        {ok, _StreamPid} ->
            ?LOG("🌊 Responses API streaming started, processing events..."),
            io:format("Response: "),
            case openai_streaming:handle_stream_events(EventHandler, 60000) of
                {ok, Events} ->
                    ?LOG("✅ Responses API streaming completed with ~p events", [length(Events)]);
                {error, Reason} ->
                    ?LOG("❌ Responses API streaming failed: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG("❌ Failed to start Responses API streaming: ~p", [Reason])
    end.