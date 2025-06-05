#!/usr/bin/env escript
%% hot_reload_demo.erl
%% Demonstration script for hot code reloading in the agents.erl system

main(_Args) ->
    io:format("🔥 Hot Code Reload Demo for Agents.erl System~n"),
    io:format("========================================~n~n"),
    
    %% Step 1: Show current system status
    io:format("📊 STEP 1: Current System Status~n"),
    io:format("--------------------------------~n"),
    show_system_status(),
    
    %% Step 2: Test the streaming function before fix
    io:format("~n🐛 STEP 2: Testing Stream Token Processing (Before Fix)~n"),
    io:format("------------------------------------------------------~n"),
    test_token_processing(),
    
    %% Step 3: Hot reload the streaming_function_handler module
    io:format("~n🔄 STEP 3: Hot Reloading streaming_function_handler~n"),
    io:format("------------------------------------------------~n"),
    hot_reload_module(streaming_function_handler),
    
    %% Step 4: Test the streaming function after hot reload
    io:format("~n✅ STEP 4: Testing Stream Token Processing (After Hot Reload)~n"),
    io:format("------------------------------------------------------------~n"),
    test_token_processing(),
    
    %% Step 5: Demonstrate modular architecture
    io:format("~n🏗️  STEP 5: Demonstrating Modular Architecture~n"),
    io:format("---------------------------------------------~n"),
    show_modular_architecture(),
    
    %% Step 6: Show hot reload capabilities for other modules
    io:format("~n⚡ STEP 6: Additional Hot Reload Capabilities~n"),
    io:format("--------------------------------------------~n"),
    demonstrate_additional_hot_reload(),
    
    io:format("~n🎉 Demo completed! The system demonstrates:~n"),
    io:format("   • Hot code reloading without downtime~n"),
    io:format("   • Modular architecture with OTP supervision~n"),
    io:format("   • Real-time fixes to production systems~n"),
    io:format("   • Zero-downtime updates~n~n").

%% Show current system status
show_system_status() ->
    %% Check if applications are running
    RunningApps = application:which_applications(),
    io:format("📱 Running Applications:~n"),
    lists:foreach(fun({App, Desc, Ver}) ->
        case lists:member(App, [openai, agents, agent_web]) of
            true ->
                io:format("   ✅ ~p (~s) - v~s~n", [App, Desc, Ver]);
            false ->
                ok
        end
    end, RunningApps),
    
    %% Show loaded modules
    OurModules = [
        streaming_function_handler,
        agent_instance,
        agent_tools,
        openai_chat,
        agent_ws_handler
    ],
    
    io:format("~n📦 Key Modules Status:~n"),
    lists:foreach(fun(Module) ->
        case code:is_loaded(Module) of
            {file, _} ->
                io:format("   ✅ ~p - loaded~n", [Module]);
            false ->
                io:format("   ❌ ~p - not loaded~n", [Module])
        end
    end, OurModules).

%% Test token processing function
test_token_processing() ->
    %% Test various token types that might cause the bytes issue
    TestTokens = [
        <<"Hello world">>,
        "Sociology is",
        "the scientific study",
        [83, 111, 99, 105, 111, 108, 111, 103, 121], % "Sociology" as bytes
        123,
        #{type => test}
    ],
    
    io:format("🧪 Testing token processing with various inputs:~n"),
    lists:foreach(fun(Token) ->
        try
            Result = streaming_function_handler:process_token_for_display(Token),
            io:format("   Input:  ~p~n", [Token]),
            io:format("   Output: ~p~n", [Result]),
            io:format("   Type:   ~p~n~n", [type_of(Result)])
        catch
            E:R ->
                io:format("   Input:  ~p~n", [Token]),
                io:format("   ERROR:  ~p:~p~n~n", [E, R])
        end
    end, TestTokens).

%% Hot reload a module
hot_reload_module(Module) ->
    io:format("🔄 Hot reloading module: ~p~n", [Module]),
    
    %% First, check current version
    case code:is_loaded(Module) of
        {file, File} ->
            io:format("   📍 Current location: ~s~n", [File]);
        false ->
            io:format("   ⚠️  Module not currently loaded~n")
    end,
    
    %% Purge old version
    io:format("   🗑️  Purging old version...~n"),
    code:purge(Module),
    
    %% Load new version
    io:format("   📥 Loading new version...~n"),
    case code:load_file(Module) of
        {module, Module} ->
            io:format("   ✅ Successfully reloaded ~p~n", [Module]),
            case code:is_loaded(Module) of
                {file, NewFile} ->
                    io:format("   📍 New location: ~s~n", [NewFile]);
                false ->
                    io:format("   ⚠️  Unexpected: module not loaded after reload~n")
            end;
        {error, Reason} ->
            io:format("   ❌ Failed to reload: ~p~n", [Reason])
    end.

%% Show the modular architecture
show_modular_architecture() ->
    io:format("🏗️  OTP Application Architecture:~n"),
    io:format("~n"),
    io:format("   agents.erl System~n"),
    io:format("   ├── 📱 openai (API Layer)~n"),
    io:format("   │   ├── openai_chat.erl~n"),
    io:format("   │   ├── openai_responses.erl~n"),
    io:format("   │   ├── anthropic_client.erl~n"),
    io:format("   │   └── rate limiting & cost tracking~n"),
    io:format("   │~n"),
    io:format("   ├── 🤖 agents (Core Agent System)~n"),
    io:format("   │   ├── agent.erl (main interface)~n"),
    io:format("   │   ├── agent_instance.erl (individual agents)~n"),
    io:format("   │   ├── agent_tools.erl (tool registry)~n"),
    io:format("   │   ├── streaming_function_handler.erl ⚡ JUST RELOADED~n"),
    io:format("   │   └── knowledge base & search~n"),
    io:format("   │~n"),
    io:format("   └── 🌐 agent_web (Web Interface)~n"),
    io:format("       ├── HTTP/WebSocket handlers~n"),
    io:format("       ├── React/TypeScript frontend~n"),
    io:format("       ├── MCP protocol integration~n"),
    io:format("       └── monitoring & metrics~n"),
    io:format("~n"),
    
    %% Show supervision tree
    io:format("🌳 Supervision Tree Structure:~n"),
    show_supervision_tree().

%% Show supervision tree
show_supervision_tree() ->
    %% Get main supervisors
    MainSups = [
        {openai_sup, "OpenAI API management"},
        {agents_sup, "Agent process supervision"},
        {agent_web_sup, "Web interface & HTTP"}
    ],
    
    lists:foreach(fun({SupName, Description}) ->
        case whereis(SupName) of
            undefined ->
                io:format("   ❌ ~p (~s) - not running~n", [SupName, Description]);
            Pid ->
                io:format("   ✅ ~p (~s) - PID: ~p~n", [SupName, Description, Pid]),
                try
                    Children = supervisor:which_children(Pid),
                    lists:foreach(fun({Id, ChildPid, Type, _Modules}) ->
                        Status = if
                            is_pid(ChildPid) -> "running";
                            true -> "not running"
                        end,
                        io:format("      └── ~p (~p) - ~s~n", [Id, Type, Status])
                    end, Children)
                catch
                    _:_ ->
                        io:format("      └── (unable to get children)~n")
                end
        end
    end, MainSups).

%% Demonstrate additional hot reload capabilities
demonstrate_additional_hot_reload() ->
    io:format("⚡ Additional Hot Reload Capabilities:~n"),
    io:format("~n"),
    
    %% Show hot reload tools available
    io:format("🛠️  Available Hot Reload Tools:~n"),
    io:format("   • Manual module reload: code:load_file(Module)~n"),
    io:format("   • Batch reload: [code:load_file(M) || M <- Modules]~n"),
    io:format("   • File watcher: inotify-based auto-reload~n"),
    io:format("   • HTTP API: POST /api/reload with module name~n"),
    io:format("   • Agent tools: reload_module, compile_and_reload~n"),
    io:format("~n"),
    
    %% Show what can be hot reloaded
    io:format("🔧 What Can Be Hot Reloaded:~n"),
    HotReloadable = [
        "Business logic modules",
        "Handler functions", 
        "Tool implementations",
        "WebSocket event handlers",
        "HTTP route handlers",
        "Agent behavior patterns",
        "Stream processing logic",
        "Running gen_server state (requires restart)",
        "Supervisor configuration (requires restart)"
    ],
    lists:foreach(fun(Item) ->
        io:format("   ~s~n", [Item])
    end, HotReloadable),
    
    %% Demo actual hot reload of another module
    io:format("~n🔄 Live Demo - Hot Reloading agent_tools:~n"),
    case code:is_loaded(agent_tools) of
        {file, _} ->
            io:format("   📍 agent_tools currently loaded~n"),
            code:purge(agent_tools),
            case code:load_file(agent_tools) of
                {module, agent_tools} ->
                    io:format("   ✅ agent_tools successfully hot reloaded~n");
                Error ->
                    io:format("   ❌ Failed to reload agent_tools: ~p~n", [Error])
            end;
        false ->
            io:format("   ⚠️  agent_tools not currently loaded~n")
    end.

%% Helper function to determine type
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_list(X) -> list;
type_of(X) when is_atom(X) -> atom;
type_of(X) when is_integer(X) -> integer;
type_of(X) when is_float(X) -> float;
type_of(X) when is_map(X) -> map;
type_of(X) when is_tuple(X) -> tuple;
type_of(X) when is_pid(X) -> pid;
type_of(_) -> unknown.