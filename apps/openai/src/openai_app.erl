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
    % Get application environment
    EnvConfig = application:get_all_env(openai),
    ConfigMap = maps:from_list(EnvConfig),
    
    % Start the supervisor tree
    case openai_sup:start_link(ConfigMap) of
        {ok, Pid} ->
            % Start the chat client
            openai_clients_sup:start_client(chat, ConfigMap),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.