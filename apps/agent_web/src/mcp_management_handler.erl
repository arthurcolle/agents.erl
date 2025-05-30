-module(mcp_management_handler).
-export([init/2]).

%% Enhanced MCP management API handler for both local and remote servers

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    handle_request(Method, Path, Req0, State).

%% Local server management endpoints
handle_request(<<"GET">>, [<<"local">>, <<"servers">>], Req0, State) ->
    try
        Servers = mcp_manager:list_local_servers(),
        Response = jsx:encode(#{servers => Servers}),
        Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            Response,
            Req0),
        {ok, Req, State}
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req0),
            {ok, CatchReq, State}
    end;

handle_request(<<"POST">>, [<<"local">>, <<"servers">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        Config = jsx:decode(Body, [return_maps]),
        case mcp_manager:start_local_server(Config) of
            {ok, ServerId} ->
                Response = jsx:encode(#{id => ServerId, status => <<"started">>}),
                Req = cowboy_req:reply(201,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

handle_request(<<"DELETE">>, [<<"local">>, <<"servers">>, ServerId], Req0, State) ->
    try
        case mcp_manager:stop_local_server(ServerId) of
            ok ->
                Response = jsx:encode(#{status => <<"stopped">>}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req0),
                {ok, Req, State};
            {error, not_found} ->
                ErrorResponse = jsx:encode(#{error => <<"Server not found">>}),
                Req = cowboy_req:reply(404,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req0),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(500,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req0),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req0),
            {ok, CatchReq, State}
    end;

%% Tool registration endpoints
handle_request(<<"POST">>, [<<"local">>, <<"servers">>, ServerId, <<"tools">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"name">> := ToolName, <<"definition">> := ToolDef} = jsx:decode(Body, [return_maps]),
        case mcp_manager:register_tool(ServerId, ToolName, ToolDef) of
            ok ->
                Response = jsx:encode(#{status => <<"registered">>}),
                Req = cowboy_req:reply(201,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

%% Resource registration endpoints
handle_request(<<"POST">>, [<<"local">>, <<"servers">>, ServerId, <<"resources">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"uri">> := URI, <<"definition">> := ResourceDef} = jsx:decode(Body, [return_maps]),
        case mcp_manager:register_resource(ServerId, URI, ResourceDef) of
            ok ->
                Response = jsx:encode(#{status => <<"registered">>}),
                Req = cowboy_req:reply(201,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

%% Prompt registration endpoints
handle_request(<<"POST">>, [<<"local">>, <<"servers">>, ServerId, <<"prompts">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"name">> := PromptName, <<"definition">> := PromptDef} = jsx:decode(Body, [return_maps]),
        case mcp_manager:register_prompt(ServerId, PromptName, PromptDef) of
            ok ->
                Response = jsx:encode(#{status => <<"registered">>}),
                Req = cowboy_req:reply(201,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

%% Remote server management
handle_request(<<"POST">>, [<<"remote">>, <<"connect">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"server_id">> := ServerId, <<"config">> := Config} = jsx:decode(Body, [return_maps]),
        case mcp_manager:connect_remote_server(ServerId, Config) of
            {ok, _} ->
                Response = jsx:encode(#{status => <<"connected">>}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

handle_request(<<"POST">>, [<<"remote">>, <<"disconnect">>, ServerId], Req0, State) ->
    try
        case mcp_manager:disconnect_remote_server(ServerId) of
            ok ->
                Response = jsx:encode(#{status => <<"disconnected">>}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req0),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req0),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req0),
            {ok, CatchReq, State}
    end;

%% Discovery endpoints
handle_request(<<"POST">>, [<<"discover">>], Req0, State) ->
    try
        Discovered = mcp_manager:discover_servers(),
        Response = jsx:encode(#{discovered => Discovered}),
        Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            Response,
            Req0),
        {ok, Req, State}
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req0),
            {ok, CatchReq, State}
    end;

%% Status endpoint
handle_request(<<"GET">>, [<<"status">>], Req0, State) ->
    try
        Status = mcp_manager:get_status(),
        Response = jsx:encode(Status),
        Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"application/json">>},
            Response,
            Req0),
        {ok, Req, State}
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req0),
            {ok, CatchReq, State}
    end;

%% Auto-connect control
handle_request(<<"POST">>, [<<"auto_connect">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"enabled">> := Enabled} = jsx:decode(Body, [return_maps]),
        case mcp_manager:auto_connect(Enabled) of
            ok ->
                Response = jsx:encode(#{status => <<"updated">>, auto_connect => Enabled}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

%% Operations endpoints
handle_request(<<"POST">>, [<<"execute_tool">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"server_id">> := ServerId, <<"tool_name">> := ToolName, 
          <<"arguments">> := Arguments} = jsx:decode(Body, [return_maps]),
        case mcp_manager:execute_tool(ServerId, ToolName, Arguments) of
            {ok, Result} ->
                Response = jsx:encode(#{result => Result}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

handle_request(<<"POST">>, [<<"read_resource">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"server_id">> := ServerId, <<"uri">> := URI} = jsx:decode(Body, [return_maps]),
        case mcp_manager:read_resource(ServerId, URI) of
            {ok, Content} ->
                Response = jsx:encode(#{content => Content}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

handle_request(<<"POST">>, [<<"get_prompt">>], Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{<<"server_id">> := ServerId, <<"prompt_name">> := PromptName} = jsx:decode(Body, [return_maps]),
        case mcp_manager:get_prompt(ServerId, PromptName) of
            {ok, Prompt} ->
                Response = jsx:encode(#{prompt => Prompt}),
                Req = cowboy_req:reply(200,
                    #{<<"content-type">> => <<"application/json">>},
                    Response,
                    Req1),
                {ok, Req, State};
            {error, Reason} ->
                ErrorResponse = jsx:encode(#{error => format_error(Reason)}),
                Req = cowboy_req:reply(400,
                    #{<<"content-type">> => <<"application/json">>},
                    ErrorResponse,
                    Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            CatchErrorResponse = jsx:encode(#{error => iolist_to_binary(io_lib:format("~p", [Error]))}),
            CatchReq = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                CatchErrorResponse,
                Req1),
            {ok, CatchReq, State}
    end;

handle_request(_, _, Req0, State) ->
    Response = jsx:encode(#{error => <<"Method not allowed">>}),
    Req = cowboy_req:reply(405,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req0),
    {ok, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).