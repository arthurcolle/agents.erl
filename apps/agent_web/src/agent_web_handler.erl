-module(agent_web_handler).

-export([init/2]).

init(Req0, Opts) ->
    Html = <<"<!DOCTYPE html>
<html>
<head>
    <title>Agent System Web Interface</title>
    <link rel=\"stylesheet\" href=\"/static/css/style.css\">
</head>
<body>
    <div id=\"app\"></div>
    <script src=\"/static/js/app.js\"></script>
</body>
</html>">>,
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    
    {ok, Req, Opts}.