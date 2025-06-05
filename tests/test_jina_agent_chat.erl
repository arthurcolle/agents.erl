-module(test_jina_agent_chat).
-export([test/0]).

test() ->
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    io:format("~n=== Testing Agent with All Jina Tools ===~n"),
    
    % List all available Jina tools
    AllTools = agent_tools:list_tools(),
    JinaTools = [T || T <- AllTools, 
                      case atom_to_list(T) of
                          "jina_" ++ _ -> true;
                          _ -> false
                      end],
    io:format("Available Jina tools: ~p~n", [JinaTools]),
    
    % Create an agent with all Jina tools
    Config = #{
        agent_id => <<"jina_power_agent">>,
        model => <<"gpt-4o-mini">>,
        name => <<"Jina Power Agent">>,
        tools => JinaTools,  % Include all Jina tools
        system_prompt => <<"You are an AI assistant with access to powerful Jina AI tools. 
                           Use these tools to help users with web search, content extraction, 
                           fact-checking, embeddings, document ranking, classification, and more.
                           Always use the appropriate Jina tool when relevant to the user's request.">>
    },
    
    case agent_supervisor:start_agent(Config) of
        {ok, Pid} ->
            io:format("Agent started successfully with PID: ~p~n", [Pid]),
            io:format("Agent has access to ~p Jina tools~n", [length(JinaTools)]),
            
            % Test 1: Web search
            test_chat(Pid, <<"Search for information about 'Erlang OTP 28 release' using Jina search.">>),
            
            % Test 2: Webpage reading
            test_chat(Pid, <<"Read the content from https://www.erlang.org/ using Jina reader.">>),
            
            % Test 3: Fact checking
            test_chat(Pid, <<"Fact-check this statement: 'Erlang was created in 1986 at Ericsson'.">>),
            
            % Stop the agent
            gen_server:stop(Pid),
            io:format("~nAgent stopped successfully.~n");
        Error ->
            io:format("Failed to start agent: ~p~n", [Error])
    end,
    
    ok.

test_chat(Pid, Message) ->
    io:format("~n--- Testing: ~s ---~n", [Message]),
    case gen_server:call(Pid, {chat, Message}, 60000) of
        {ok, Response} ->
            io:format("Response: ~s~n", [Response]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.