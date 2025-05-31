%%%-------------------------------------------------------------------
%%% @doc
%%% Claude Configuration Handler
%%% Provides API endpoints for Claude-compatible MCP configuration
%%% @end
%%%-------------------------------------------------------------------
-module(claude_config_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    handle_request(Method, Path, Req0, State).

handle_request(<<"GET">>, <<"/api/claude/config">>, Req0, State) ->
    case claude_oauth_adapter:get_claude_mcp_config() of
        {ok, Config} ->
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(Config), Req0),
            {ok, Req1, State};
        {error, Reason} ->
            Req1 = cowboy_req:reply(500, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => Reason}), Req0),
            {ok, Req1, State}
    end;

handle_request(<<"POST">>, <<"/api/claude/export">>, Req0, State) ->
    case claude_oauth_adapter:export_for_claude_desktop() of
        {ok, ConfigPath} ->
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(#{
                    success => true,
                    message => <<"Configuration exported to Claude Desktop">>,
                    path => list_to_binary(ConfigPath)
                }), Req0),
            {ok, Req1, State};
        {error, Reason} ->
            Req1 = cowboy_req:reply(500, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{
                    error => Reason,
                    message => <<"Failed to export configuration">>
                }), Req0),
            {ok, Req1, State}
    end;

handle_request(<<"GET">>, <<"/api/claude/oauth-status">>, Req0, State) ->
    Summary = claude_oauth_adapter:get_oauth_summary_for_claude(),
    Req1 = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        jsx:encode(Summary), Req0),
    {ok, Req1, State};

handle_request(_, _, Req0, State) ->
    Req1 = cowboy_req:reply(404, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{error => <<"not_found">>}), Req0),
    {ok, Req1, State}.