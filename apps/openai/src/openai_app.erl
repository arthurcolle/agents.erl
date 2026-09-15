%% openai_app.erl
%% Main application module for the OpenAI Erlang client
-module(openai_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    % Child clients are declared by openai_clients_sup from
    % auto_start_groups; the application callback owns only the root.
    EnvConfig = application:get_all_env(openai),
    ConfigMap = maps:from_list(EnvConfig),
    openai_sup:start_link(ConfigMap).

stop(_State) ->
    ok.
