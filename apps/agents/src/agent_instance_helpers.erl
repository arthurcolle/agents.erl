-module(agent_instance_helpers).
-export([
    %% Helper functions for agent instances
    format_agent_info/1,
    validate_agent_config/1
]).

%% Format agent information for display
format_agent_info(Agent) when is_map(Agent) ->
    #{
        id => maps:get(id, Agent, undefined),
        name => maps:get(name, Agent, <<"Unknown">>),
        status => maps:get(status, Agent, idle),
        tools => maps:get(tools, Agent, [])
    };
format_agent_info(_) ->
    #{id => undefined, name => <<"Invalid">>, status => error, tools => []}.

%% Validate agent configuration
validate_agent_config(Config) when is_map(Config) ->
    case maps:get(name, Config, undefined) of
        undefined -> {error, missing_name};
        Name when is_binary(Name) ->
            case maps:get(model, Config, undefined) of
                undefined -> {error, missing_model};
                Model when is_binary(Model) -> ok;
                _ -> {error, invalid_model}
            end;
        _ -> {error, invalid_name}
    end;
validate_agent_config(_) ->
    {error, invalid_config}.