-module(agent_initializer).
-export([init_default_agents/0]).

init_default_agents() ->
    io:format("Initializing default agents...~n"),
    
    %% Check if agents already exist
    ExistingAgents = agent_registry:list_agents(),
    ExistingNames = lists:map(fun({_, _, Meta}) ->
        maps:get(name, Meta, undefined)
    end, ExistingAgents),
    
    %% List of default agents to create
    DefaultAgents = [
        #{name => <<"Research Assistant">>, type => researcher, template => <<"researcher">>},
        #{name => <<"Code Helper">>, type => coder, template => <<"coder">>},
        #{name => <<"Data Analyst">>, type => analyst, template => <<"analyst">>},
        #{name => <<"Project Orchestrator">>, type => orchestrator, template => <<"orchestrator">>},
        #{name => <<"System Monitor">>, type => monitor, template => <<"monitor">>},
        #{name => <<"Language Translator">>, type => translator, template => <<"translator">>},
        #{name => <<"AI Teacher">>, type => teacher, template => <<"teacher">>},
        #{name => <<"Debug Assistant">>, type => debugger, template => <<"debugger">>}
    ],
    
    %% Create each agent
    Results = lists:map(fun(Agent) ->
        #{name := Name, template := Template} = Agent,
        case lists:member(Name, ExistingNames) of
            true ->
                io:format("Agent ~s already exists, skipping~n", [Name]),
                {skipped, Name, already_exists};
            false ->
                case agent_templates:create_from_template(Template, #{name => Name}) of
            {ok, Pid} ->
                AgentId = list_to_binary(uuid:to_string(uuid:uuid4())),
                agent_registry:register_agent(AgentId, Pid, #{
                    type => template,
                    name => Name,
                    template_id => Template
                }),
                io:format("Created agent ~s (ID: ~s)~n", [Name, AgentId]),
                {ok, AgentId, Name};
            {error, Reason} ->
                io:format("Failed to create agent ~s: ~p~n", [Name, Reason]),
                {error, Name, Reason}
                end
        end
    end, DefaultAgents),
    
    %% Summary
    Ok = [R || R = {ok, _, _} <- Results],
    Errors = [R || R = {error, _, _} <- Results],
    Skipped = [R || R = {skipped, _, _} <- Results],
    io:format("~nAgent initialization complete: ~p successful, ~p failed, ~p skipped~n", 
              [length(Ok), length(Errors), length(Skipped)]),
    
    Results.