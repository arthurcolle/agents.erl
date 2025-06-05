%% Test module for multi-turn function calling
-module(test_multi_turn_shell).
-export([test/0]).

test() ->
    io:format("~n🚀 Starting multi-turn function calling test with enhanced logging~n"),
    
    % Initialize an agent with tools
    io:format("📝 Creating agent with weather search capabilities...~n"),
    {ok, Agent} = agent_supervisor:start_agent(#{
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
    Query1 = <<"What's the weather like in Tokyo today?">>,
    io:format("👤 User: ~s~n", [Query1]),
    
    case agent:chat(Agent, Query1) of
        {ok, Response1} ->
            io:format("~n🤖 Agent: ~s~n", [maps:get(message, Response1, <<"No response">>)]),
            io:format("~n📊 First turn complete~n"),
            
            % Second turn - follow-up question
            timer:sleep(1000),
            io:format("~n==========================================~n"),
            Query2 = <<"Based on that weather, should I bring an umbrella?">>,
            io:format("👤 User: ~s~n", [Query2]),
            
            case agent:chat(Agent, Query2) of
                {ok, Response2} ->
                    io:format("~n🤖 Agent: ~s~n", [maps:get(message, Response2, <<"No response">>)]),
                    io:format("~n✅ Multi-turn conversation successful!~n");
                {error, Error2} ->
                    io:format("~n❌ Error in second turn: ~p~n", [Error2])
            end;
        {error, Error1} ->
            io:format("~n❌ Error in first turn: ~p~n", [Error1])
    end,
    
    % Clean up
    agent:stop_agent(Agent),
    io:format("~n🏁 Test complete~n"),
    ok.