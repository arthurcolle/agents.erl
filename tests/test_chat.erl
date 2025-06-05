-module(test_chat).
-export([test/0]).

test() ->
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Create an agent
    Config = #{
        agent_id => <<"test_agent">>,
        model => <<"gpt-4o-mini">>,
        name => <<"Test Agent">>
    },
    
    case agent_supervisor:start_agent(Config) of
        {ok, Pid} ->
            io:format("Agent started with PID: ~p~n", [Pid]),
            
            % Send a chat message
            Result = gen_server:call(Pid, {chat, <<"Hello, can you hear me?">>}, 30000),
            io:format("Agent response: ~p~n", [Result]),
            
            % Stop the agent
            gen_server:stop(Pid);
        Error ->
            io:format("Failed to start agent: ~p~n", [Error])
    end,
    
    ok.
EOF < /dev/null