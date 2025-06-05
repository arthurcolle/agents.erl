%% test_enhanced_system.erl
%% Test script for the enhanced agent system with new tools and model variety

-module(test_enhanced_system).
-export([run_tests/0]).

run_tests() ->
    io:format("🧪 Testing Enhanced Agent System~n"),
    io:format("================================~n~n"),
    
    % Test 1: Verify model registry has new models
    test_model_registry(),
    
    % Test 2: Test new tools functionality
    test_new_tools(),
    
    % Test 3: Test model selection strategy
    test_model_selection(),
    
    % Test 4: Test specialized agent creation
    test_specialized_agents(),
    
    io:format("~n✅ All tests completed!~n").

%% Test model registry
test_model_registry() ->
    io:format("📋 Testing Model Registry...~n"),
    
    % Test the new models are available
    Models = [<<"o4-mini">>, <<"gpt-4.1">>, <<"gpt-4.1-mini">>, <<"gpt-4.1-nano">>, <<"o3">>],
    
    lists:foreach(fun(Model) ->
        case model_registry:validate_model(Model) of
            true ->
                io:format("  ✅ Model ~s is registered~n", [Model]);
            false ->
                io:format("  ❌ Model ~s is NOT registered~n", [Model])
        end
    end, Models),
    
    % Get model info
    case model_registry:get_model_info(<<"o3">>) of
        {ok, ModelInfo} ->
            Category = maps:get(category, ModelInfo),
            io:format("  📊 o3 model category: ~p~n", [Category]);
        {error, _} ->
            io:format("  ❌ Could not get o3 model info~n")
    end,
    
    io:format("~n").

%% Test new tools
test_new_tools() ->
    io:format("🔧 Testing New Tools...~n"),
    
    % Start agent tools if not already started
    case whereis(agent_tools) of
        undefined ->
            io:format("  Starting agent_tools...~n"),
            case agent_tools:start_link(#{}) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, StartError} -> 
                    io:format("  ❌ Failed to start agent_tools: ~p~n", [StartError]),
                    exit(StartError)
            end;
        _ -> 
            io:format("  ✅ agent_tools already running~n")
    end,
    
    % Test some new tools
    NewTools = [web_search, calculate, datetime_info, generate_uuid, hash_generate],
    
    lists:foreach(fun(Tool) ->
        case test_tool_execution(Tool) of
            ok ->
                io:format("  ✅ Tool ~p works correctly~n", [Tool]);
            {error, ToolError} ->
                io:format("  ❌ Tool ~p failed: ~p~n", [Tool, ToolError])
        end
    end, NewTools),
    
    io:format("~n").

%% Test individual tool execution
test_tool_execution(web_search) ->
    try
        Result = agent_tools:execute_tool(web_search, #{
            <<"query">> => <<"Erlang programming language">>,
            <<"num_results">> => 3
        }),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end;

test_tool_execution(calculate) ->
    try
        Result = agent_tools:execute_tool(calculate, #{
            <<"expression">> => <<"2 + 3">>,
            <<"precision">> => 2
        }),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end;

test_tool_execution(datetime_info) ->
    try
        Result = agent_tools:execute_tool(datetime_info, #{
            <<"timezone">> => <<"UTC">>,
            <<"format">> => <<"iso8601">>
        }),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end;

test_tool_execution(generate_uuid) ->
    try
        Result = agent_tools:execute_tool(generate_uuid, #{
            <<"version">> => 4,
            <<"format">> => <<"standard">>
        }),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end;

test_tool_execution(hash_generate) ->
    try
        Result = agent_tools:execute_tool(hash_generate, #{
            <<"input">> => <<"test string">>,
            <<"algorithm">> => <<"sha256">>,
            <<"output_format">> => <<"hex">>
        }),
        case Result of
            {ok, _} -> ok;
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%% Test model selection strategy
test_model_selection() ->
    io:format("🎯 Testing Model Selection Strategy...~n"),
    
    % Start model selection strategy if not already started
    case whereis(model_selection_strategy) of
        undefined ->
            io:format("  Starting model_selection_strategy...~n"),
            case model_selection_strategy:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, StartError} ->
                    io:format("  ❌ Failed to start model_selection_strategy: ~p~n", [StartError]),
                    exit(StartError)
            end;
        _ ->
            io:format("  ✅ model_selection_strategy already running~n")
    end,
    
    % Test model selection for different agent types
    AgentTypes = [reasoning_specialist, fast_responder, data_analyst, code_developer],
    
    lists:foreach(fun(AgentType) ->
        case model_selection_strategy:select_model_for_agent(AgentType, #{}) of
            {ok, Model} ->
                io:format("  ✅ Selected model for ~p: ~s~n", [AgentType, Model]);
            {error, SelectError} ->
                io:format("  ❌ Failed to select model for ~p: ~p~n", [AgentType, SelectError])
        end
    end, AgentTypes),
    
    % Test task-specific model selection
    TaskTypes = [<<"reasoning">>, <<"fast_response">>, <<"cost_optimized">>],
    
    lists:foreach(fun(TaskType) ->
        case model_selection_strategy:select_model_for_task(TaskType, #{}, <<"test_agent">>) of
            {ok, Model} ->
                io:format("  ✅ Selected model for task ~s: ~s~n", [TaskType, Model]);
            Error ->
                io:format("  ❌ Failed to select model for task ~s: ~p~n", [TaskType, Error])
        end
    end, TaskTypes),
    
    io:format("~n").

%% Test specialized agent creation
test_specialized_agents() ->
    io:format("🤖 Testing Specialized Agents...~n"),
    
    % Start specialized agent factory if not already started
    case whereis(specialized_agent_factory) of
        undefined ->
            io:format("  Starting specialized_agent_factory...~n"),
            case specialized_agent_factory:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, StartError} ->
                    io:format("  ❌ Failed to start specialized_agent_factory: ~p~n", [StartError]),
                    exit(StartError)
            end;
        _ ->
            io:format("  ✅ specialized_agent_factory already running~n")
    end,
    
    % Get available agent types
    case specialized_agent_factory:get_agent_types() of
        {ok, Types} ->
            io:format("  📊 Available agent types: ~p~n", [length(Types)]),
            lists:foreach(fun(#{name := Name, description := Desc}) ->
                io:format("    - ~s: ~s~n", [Name, Desc])
            end, lists:sublist(Types, 5)); % Show first 5 types
        {error, GetError} ->
            io:format("  ❌ Failed to get agent types: ~p~n", [GetError])
    end,
    
    % Test creating a specialized agent
    case specialized_agent_factory:create_specialized_agent(<<"reasoning_specialist">>, #{
        name => <<"test_reasoning_agent">>,
        system_prompt => <<"You are a reasoning specialist for testing purposes.">>
    }) of
        {ok, AgentPid} ->
            io:format("  ✅ Created reasoning specialist agent: ~p~n", [AgentPid]),
            % Clean up
            exit(AgentPid, normal);
        Error ->
            io:format("  ❌ Failed to create reasoning specialist: ~p~n", [Error])
    end,
    
    io:format("~n").