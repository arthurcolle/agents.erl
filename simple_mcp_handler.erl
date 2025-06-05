-module(simple_mcp_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    io:format("Request: ~p ~p~n", [Method, Path]),
    
    Response = case Path of
        <<"/api/mcp/servers">> ->
            jsx:encode(#{servers => []});
        <<"/api/mcp/local/servers">> ->
            jsx:encode(#{servers => [], message => <<"Local servers endpoint working">>});
        _ ->
            jsx:encode(#{status => <<"ok">>, path => Path, method => Method})
    end,
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>,
          <<"access-control-allow-origin">> => <<"*">>},
        Response,
        Req0),
    {ok, Req, State}.