-module(check_app).
-export([main/1, check/0]).

main(_) ->
    check().

check() ->
    io:format("Checking application status...~n"),
    
    % Print applications
    Apps = application:which_applications(),
    io:format("Running applications: ~p~n", [Apps]),
    
    % Check if our apps are running
    IsAgentRunning = lists:keymember(agent, 1, Apps),
    IsOpenAIRunning = lists:keymember(openai, 1, Apps),
    
    io:format("agent app running: ~p~n", [IsAgentRunning]),
    io:format("openai app running: ~p~n", [IsOpenAIRunning]),
    
    % Check key processes
    AgentRegistry = whereis(agent_registry),
    AgentTools = whereis(agent_tools),
    OpenAISup = whereis(openai_sup),
    
    io:format("agent_registry: ~p~n", [AgentRegistry =/= undefined]),
    io:format("agent_tools: ~p~n", [AgentTools =/= undefined]),
    io:format("openai_sup: ~p~n", [OpenAISup =/= undefined]),
    
    ok.