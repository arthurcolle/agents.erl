-module(agent_web_handler).

-export([init/2]).

init(Req0, Opts) ->
    %% Serve the built index.html file
    {ok, Html} = file:read_file("apps/agent_web/priv/static/dist/index.html"),
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    
    {ok, Req, Opts}.