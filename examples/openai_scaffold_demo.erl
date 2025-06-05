#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall

%%%-------------------------------------------------------------------
%%% @doc OpenAPI Scaffold Demo using OpenAI API specification
%%% @end
%%%-------------------------------------------------------------------

-module(openai_scaffold_demo).
-mode(compile).

-export([main/1]).

main(_) ->
    io:format("~n=== OpenAPI Scaffold Demo ===~n~n"),
    
    %% Start required applications
    start_apps(),
    
    %% Start the OpenAPI scaffold system
    io:format("Starting OpenAPI Scaffold system...~n"),
    {ok, _} = application:start(openapi_scaffold),
    
    %% Download OpenAI's OpenAPI spec
    io:format("~nDownloading OpenAI API specification...~n"),
    SpecUrl = "https://raw.githubusercontent.com/openai/openai-openapi/master/openapi.yaml",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {SpecUrl, []}, [], [{body_format, binary}]),
    
    %% Save the spec
    SpecFile = "priv/openapi_specs/openai.yaml",
    filelib:ensure_dir(SpecFile),
    ok = file:write_file(SpecFile, Body),
    io:format("Saved OpenAI spec to ~s~n", [SpecFile]),
    
    %% Parse and load the spec
    io:format("~nParsing OpenAI specification...~n"),
    {ok, Spec} = openapi_parser:parse_file(SpecFile),
    io:format("Successfully parsed OpenAI spec~n"),
    
    %% Load it into the router
    io:format("~nLoading spec into router...~n"),
    ok = openapi_router:load_spec(<<"openai">>, Spec),
    
    %% Wait for code generation
    timer:sleep(2000),
    
    %% Show introspection info
    show_introspection_info(),
    
    %% Demonstrate API calls
    demonstrate_api_calls(),
    
    %% Show generated modules
    show_generated_modules(),
    
    io:format("~n=== Demo Complete ===~n~n"),
    
    %% Keep running for manual testing
    io:format("Server is running on http://localhost:8081~n"),
    io:format("~nAvailable endpoints:~n"),
    io:format("  - GET  /openapi/specs           - List loaded specs~n"),
    io:format("  - GET  /openapi/specs/openai    - Get OpenAI spec info~n"),
    io:format("  - GET  /openapi/routes/openai   - List OpenAI routes~n"),
    io:format("  - GET  /openapi/schemas/openai  - List OpenAI schemas~n"),
    io:format("  - GET  /health                  - Health check~n"),
    io:format("  - POST /v1/chat/completions     - Chat completions (mock)~n"),
    io:format("~nPress Ctrl+C to exit...~n~n"),
    
    receive
        stop -> ok
    end.

start_apps() ->
    %% Start dependencies
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(jsx),
    application:start(yamerl),
    ok.

show_introspection_info() ->
    io:format("~n=== Introspection Info ===~n"),
    
    %% Get spec info
    {ok, Info} = openapi_introspection:get_spec_info(<<"openai">>),
    Stats = maps:get(<<"statistics">>, Info),
    
    io:format("~nOpenAI API Statistics:~n"),
    io:format("  - Paths: ~p~n", [maps:get(<<"paths">>, Stats)]),
    io:format("  - Operations: ~p~n", [maps:get(<<"operations">>, Stats)]),
    io:format("  - Schemas: ~p~n", [maps:get(<<"schemas">>, Stats)]),
    io:format("  - Generated Modules: ~p~n", [maps:get(<<"generated_modules">>, Stats)]),
    
    %% Show some routes
    {ok, Routes} = openapi_introspection:get_routes(<<"openai">>),
    io:format("~nSample Routes:~n"),
    lists:foreach(fun(Route) ->
        Path = maps:get(<<"path">>, Route),
        Methods = maps:get(<<"methods">>, Route),
        io:format("  - ~s [~s]~n", [Path, string:join([binary_to_list(M) || M <- Methods], ", ")])
    end, lists:sublist(Routes, 5)),
    
    %% Show some schemas  
    {ok, Schemas} = openapi_introspection:get_schemas(<<"openai">>),
    io:format("~nSample Schemas:~n"),
    SchemaNames = maps:keys(Schemas),
    lists:foreach(fun(Name) ->
        io:format("  - ~s~n", [Name])
    end, lists:sublist(SchemaNames, 5)).

demonstrate_api_calls() ->
    io:format("~n=== API Call Demonstrations ===~n"),
    
    %% Test chat completions endpoint
    io:format("~nTesting POST /v1/chat/completions (mock response)...~n"),
    
    ChatRequest = #{
        <<"model">> => <<"gpt-4">>,
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => <<"Hello!">>}
        ]
    },
    
    case httpc:request(post, 
        {"http://localhost:8081/v1/chat/completions", 
         [{"content-type", "application/json"}],
         "application/json",
         jsx:encode(ChatRequest)},
        [], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _, RespBody}} ->
            io:format("Response Status: ~p~n", [StatusCode]),
            case jsx:decode(RespBody, [return_maps]) of
                Response when is_map(Response) ->
                    io:format("Response: ~s~n", [jsx:prettify(jsx:encode(Response))]);
                _ ->
                    io:format("Response: ~s~n", [RespBody])
            end;
        {error, Reason} ->
            io:format("Request failed: ~p~n", [Reason])
    end,
    
    %% Test models endpoint
    io:format("~nTesting GET /v1/models (mock response)...~n"),
    
    case httpc:request(get, 
        {"http://localhost:8081/v1/models", []},
        [], [{body_format, binary}]) of
        {ok, {{_, StatusCode2, _}, _, RespBody2}} ->
            io:format("Response Status: ~p~n", [StatusCode2]),
            case jsx:decode(RespBody2, [return_maps]) of
                Response2 when is_map(Response2) ->
                    io:format("Response: ~s~n", [jsx:prettify(jsx:encode(Response2))]);
                _ ->
                    io:format("Response: ~s~n", [RespBody2])
            end;
        {error, Reason2} ->
            io:format("Request failed: ~p~n", [Reason2])
    end.

show_generated_modules() ->
    io:format("~n=== Generated Modules ===~n"),
    
    {ok, Modules} = openapi_codegen:get_generated_modules(<<"openai">>),
    io:format("~nGenerated ~p modules:~n", [length(Modules)]),
    
    %% Show first few modules
    lists:foreach(fun(Module) ->
        io:format("  - ~s~n", [Module])
    end, lists:sublist(Modules, 10)),
    
    case length(Modules) > 10 of
        true ->
            io:format("  ... and ~p more~n", [length(Modules) - 10]);
        false ->
            ok
    end,
    
    %% Demonstrate using a generated schema module
    io:format("~nDemonstrating generated schema usage:~n"),
    
    %% Try to use the chat completion request schema if it exists
    case lists:member(openai_schema_createchatcompletionrequest, Modules) of
        true ->
            %% Create a new instance
            ChatReq = openai_schema_createchatcompletionrequest:new(),
            io:format("Created new ChatCompletionRequest: ~p~n", [ChatReq]),
            
            %% Convert to map
            ChatReqMap = openai_schema_createchatcompletionrequest:to_map(ChatReq),
            io:format("As map: ~p~n", [ChatReqMap]);
        false ->
            io:format("Chat completion request schema not found in generated modules~n")
    end.