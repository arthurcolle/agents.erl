#!/usr/bin/env escript
%% Test multi-turn function calling with enhanced logging

-mode(compile).

main(_) ->
    % Start necessary applications
    io:format("🚀 Starting multi-turn function calling test with enhanced logging~n"),
    
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(jsx),
    application:start(gproc),
    
    % Start the agents application
    {ok, _} = application:ensure_all_started(agents),
    {ok, _} = application:ensure_all_started(openai),
    
    % Initialize an agent with tools
    io:format("📝 Creating agent with weather search capabilities...~n"),
    {ok, Agent} = agent:start_agent(#{
        id => <<"test_multi_turn">>,
        name => <<"Multi-Turn Test Agent">>,
        model => <<"gpt-4o-mini">>,
        system_prompt => <<"You are a helpful assistant that can search for weather information. When asked about weather, always use the jina_search tool to find current weather information.">>,
        tools => [jina_search],
        max_history => 50
    }),
    
    % Test multi-turn conversation
    io:format("~n💬 Starting multi-turn conversation...~n"),
    io:format("==========================================~n"),
    
    % First turn - ask about weather
    Query1 = <<"What's the weather like in San Francisco today?">>,
    io:format("👤 User: ~s~n", [Query1]),
    
    case agent:chat(Agent, Query1) of
        {ok, Response1} ->
            io:format("~n🤖 Agent: ~s~n", [maps:get(message, Response1, <<"No response">>)]),
            io:format("~n📊 First turn complete~n"),
            
            % Second turn - follow-up question
            timer:sleep(1000),
            io:format("~n==========================================~n"),
            Query2 = <<"What about New York? Compare it to San Francisco.">>,
            io:format("👤 User: ~s~n", [Query2]),
            
            case agent:chat(Agent, Query2) of
                {ok, Response2} ->
                    io:format("~n🤖 Agent: ~s~n", [maps:get(message, Response2, <<"No response">>)]),
                    io:format("~n📊 Second turn complete~n"),
                    
                    % Third turn - another follow-up
                    timer:sleep(1000),
                    io:format("~n==========================================~n"),
                    Query3 = <<"Which city would be better for outdoor activities today?">>,
                    io:format("👤 User: ~s~n", [Query3]),
                    
                    case agent:chat(Agent, Query3) of
                        {ok, Response3} ->
                            io:format("~n🤖 Agent: ~s~n", [maps:get(message, Response3, <<"No response">>)]),
                            io:format("~n✅ Multi-turn conversation successful!~n");
                        {error, Error3} ->
                            io:format("~n❌ Error in third turn: ~p~n", [Error3])
                    end;
                {error, Error2} ->
                    io:format("~n❌ Error in second turn: ~p~n", [Error2])
            end;
        {error, Error1} ->
            io:format("~n❌ Error in first turn: ~p~n", [Error1])
    end,
    
    % Get agent state for debugging
    io:format("~n📈 Final agent state:~n"),
    State = agent:get_state(Agent),
    ConvHistory = maps:get(conversation_history, State, []),
    io:format("   Conversation history length: ~p~n", [length(ConvHistory)]),
    io:format("   Model: ~p~n", [maps:get(model, State)]),
    io:format("   Metrics: ~p~n", [maps:get(metrics, State)]),
    
    % Clean up
    agent:stop_agent(Agent),
    io:format("~n🏁 Test complete~n"),
    halt(0).