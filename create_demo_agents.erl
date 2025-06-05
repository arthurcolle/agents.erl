#!/usr/bin/env escript
%% Demo script to create diverse agents and enable model chat

main(_) ->
    io:format("Starting Distributed Agent System...~n"),
    
    % Ensure applications are loaded
    application:load(crypto),
    application:load(ssl),
    application:load(inets),
    application:load(openai),
    application:load(agents),
    application:load(agent_web),
    
    % Start applications
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(openai),
    {ok, _} = application:ensure_all_started(agents),
    {ok, _} = application:ensure_all_started(agent_web),
    
    io:format("Applications started successfully!~n"),
    
    % Wait a moment for system initialization
    timer:sleep(2000),
    
    % Create diverse demo agents
    create_demo_agents(),
    
    io:format("Demo agents created! Web interface available at http://localhost:8080~n"),
    io:format("Press Ctrl+C to stop.~n"),
    
    % Keep the system running
    receive
        _ -> ok
    end.

create_demo_agents() ->
    io:format("Creating demo agents...~n"),
    
    % Create agents using templates
    Agents = [
        {<<"Research Assistant">>, <<"researcher">>, <<"A sophisticated research agent with search capabilities">>},
        {<<"Code Helper">>, <<"coder">>, <<"An expert coding assistant for software development">>},
        {<<"Data Analyst">>, <<"analyst">>, <<"Specializes in data analysis and visualization">>},
        {<<"Debug Expert">>, <<"debugger">>, <<"Troubleshooting and problem-solving specialist">>},
        {<<"Educational Assistant">>, <<"teacher">>, <<"Patient tutor for learning and education">>},
        {<<"Weather Assistant">>, <<"researcher">>, <<"Get current weather information for any location">>},
        {<<"Advanced AI">>, <<"advanced_researcher">>, <<"High-performance agent with advanced reasoning">>},
        {<<"Quick Helper">>, <<"lightweight_helper">>, <<"Fast responses for simple tasks">>},
        {<<"Security Analyst">>, <<"security_analyst">>, <<"Cybersecurity expert for threat analysis">>},
        {<<"ML Engineer">>, <<"ml_engineer">>, <<"Machine learning and AI model specialist">>}
    ],
    
    lists:foreach(fun({Name, TemplateId, Description}) ->
        case create_agent_from_template(Name, TemplateId, Description) of
            {ok, AgentId} ->
                io:format("✓ Created ~s (ID: ~s)~n", [Name, AgentId]);
            {error, Reason} ->
                io:format("✗ Failed to create ~s: ~p~n", [Name, Reason])
        end
    end, Agents),
    
    % List all created agents
    timer:sleep(1000),
    list_agents().

create_agent_from_template(Name, TemplateId, Description) ->
    try
        % Generate unique ID
        AgentId = generate_agent_id(Name),
        
        % Get template configuration
        case agent_templates:get_template(TemplateId) of
            {ok, Template} ->
                % Create agent config
                Config = Template#{
                    id => AgentId,
                    name => Name,
                    description => Description,
                    template_id => TemplateId
                },
                
                % Start agent instance
                case agent_instance:start_link(Config) of
                    {ok, Pid} ->
                        % Register with agent registry
                        Meta = #{
                            type => template,
                            name => Name,
                            label => Name,
                            description => Description,
                            template_id => TemplateId,
                            model => maps:get(model, Template, <<"gpt-4o">>),
                            tools => maps:get(tools, Template, []),
                            capabilities => maps:get(tools, Template, []),
                            created_at => erlang:system_time(millisecond),
                            conversation_length => 0,
                            status => <<"active">>
                        },
                        agent_registry:register_agent(AgentId, Pid, Meta),
                        {ok, AgentId};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, {template_not_found, Reason}}
        end
    catch
        Error:Reason2:Stack ->
            io:format("Exception creating agent ~s: ~p:~p~n~p~n", [Name, Error, Reason2, Stack]),
            {error, {exception, Error, Reason2}}
    end.

generate_agent_id(Name) ->
    % Create a shorter, readable ID based on name
    CleanName = re:replace(binary_to_list(Name), "[^a-zA-Z0-9]", "", [global, {return, list}]),
    Timestamp = integer_to_list(erlang:system_time(millisecond) rem 100000),
    list_to_binary(string:lowercase(CleanName) ++ "_" ++ Timestamp).

list_agents() ->
    try
        Agents = agent_registry:list_agents(),
        io:format("~nActive Agents (~p total):~n", [length(Agents)]),
        lists:foreach(fun({Id, Pid, Meta}) ->
            Name = maps:get(name, Meta, Id),
            Type = maps:get(template_id, Meta, maps:get(type, Meta, <<"unknown">>)),
            Model = maps:get(model, Meta, <<"unknown">>),
            Status = case is_process_alive(Pid) of
                true -> <<"running">>;
                false -> <<"stopped">>
            end,
            io:format("  • ~s (~s) - ~s [~s]~n", [Name, Type, Model, Status])
        end, Agents)
    catch
        Error:Reason3 ->
            io:format("Error listing agents: ~p:~p~n", [Error, Reason3])
    end.