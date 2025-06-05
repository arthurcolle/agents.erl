-module(error_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        idle_timeout => 60000,
        max_frame_size => 1048576
    }}.

websocket_init(State) ->
    % Subscribe to error updates
    error_tracking_system:subscribe(self()),
    
    % Send initial connection success message
    {[{text, jsx:encode(#{
        type => <<"connected">>,
        message => <<"Connected to error tracking system">>
    })}], State}.

websocket_handle({text, Msg}, State) ->
    try
        case jsx:decode(Msg, [return_maps]) of
            #{<<"type">> := <<"ping">>} ->
                {[{text, jsx:encode(#{type => <<"pong">>})}], State};
            #{<<"type">> := <<"get_errors">>} ->
                Errors = error_tracking_system:get_errors(50),
                {[{text, jsx:encode(#{
                    type => <<"errors">>,
                    data => Errors
                })}], State};
            _ ->
                {[], State}
        end
    catch
        _:_ ->
            {[{text, jsx:encode(#{
                type => <<"error">>,
                message => <<"Invalid message format">>
            })}], State}
    end;

websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({error_logged, Error}, State) ->
    % Process logged error with AI for dynamic analysis
    ai_error_processor:process_websocket_error(#{
        <<"logged_error">> => Error,
        <<"handler">> => <<"error_ws_handler">>,
        <<"timestamp">> => erlang:system_time(second)
    }, <<"error_tracking_system">>),
    
    {[{text, jsx:encode(#{
        type => <<"error_logged">>,
        error => Error
    })}], State};

websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Req, _State) ->
    error_tracking_system:unsubscribe(self()),
    ok.