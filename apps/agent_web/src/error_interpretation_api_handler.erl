-module(error_interpretation_api_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    Req = handle_request(Method, Path, Req0, State),
    {ok, Req, State}.

handle_request(<<"GET">>, [], Req, _State) ->
    %% GET /api/interpretations - Get recent interpretations
    case ai_error_interpreter:get_recent_interpretations() of
        {ok, Interpretations} ->
            respond_json(Req, 200, #{
                status => success,
                interpretations => Interpretations,
                total => length(Interpretations)
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_request(<<"GET">>, [InterpId], Req, _State) ->
    %% GET /api/interpretations/:id - Get specific interpretation
    case ai_error_interpreter:get_interpretation(InterpId) of
        {ok, Interpretation} ->
            respond_json(Req, 200, #{
                status => success,
                interpretation => Interpretation
            });
        {error, not_found} ->
            respond_json(Req, 404, #{error => <<"Interpretation not found">>});
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_request(<<"POST">>, [<<"interpret">>], Req0, _State) ->
    %% POST /api/interpretations/interpret - Interpret error text
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"error_text">> := ErrorText} ->
            ai_error_interpreter:interpret_log_line(ErrorText),
            respond_json(Req, 202, #{
                status => accepted,
                message => <<"Interpretation in progress">>
            });
        _ ->
            respond_json(Req, 400, #{error => <<"Missing error_text field">>})
    end;

handle_request(<<"POST">>, [<<"subscribe">>], Req0, _State) ->
    %% POST /api/interpretations/subscribe - Subscribe to interpretations via SSE
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>
    },
    
    Req = cowboy_req:stream_reply(200, Headers, Req0),
    
    %% Subscribe to interpretations
    ai_error_interpreter:subscribe(self()),
    
    %% Send initial connection event
    cowboy_req:stream_body(
        <<"event: connected\ndata: {\"status\":\"connected\"}\n\n">>, 
        nofin, Req
    ),
    
    %% Enter SSE loop
    sse_loop(Req),
    {ok, Req, _State};

handle_request(_, _, Req, _State) ->
    respond_json(Req, 405, #{error => <<"Method not allowed">>}).

%%====================================================================
%% SSE handling
%%====================================================================

sse_loop(Req) ->
    receive
        {error_interpretation_event, {new_interpretation, Interpretation}} ->
            Event = jsx:encode(#{
                event => <<"interpretation">>,
                data => Interpretation
            }),
            cowboy_req:stream_body(
                [<<"event: interpretation\ndata: ">>, Event, <<"\n\n">>],
                nofin, Req
            ),
            sse_loop(Req);
            
        {error_interpretation_event, Event} ->
            %% Handle other events
            EventData = jsx:encode(Event),
            cowboy_req:stream_body(
                [<<"event: update\ndata: ">>, EventData, <<"\n\n">>],
                nofin, Req
            ),
            sse_loop(Req);
            
        stop ->
            cowboy_req:stream_body(<<"event: close\ndata: {}\n\n">>, fin, Req);
            
        _ ->
            sse_loop(Req)
            
    after 30000 ->
        %% Send heartbeat every 30 seconds
        cowboy_req:stream_body(<<":heartbeat\n\n">>, nofin, Req),
        sse_loop(Req)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

respond_json(Req, StatusCode, Data) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).