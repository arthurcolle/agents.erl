-module(timeline_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,
         handle_get/2, handle_post/2, handle_delete/2, init_storage/0, safe_to_atom/2]).

-record(timeline_event, {
    id,
    timestamp,
    type,              % message | system | agent_action | error | warning | success
    source,            % user | agent | system
    agent_id,
    agent_name,
    conversation_id,
    content,
    metadata
}).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

handle_get(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            % Get all timeline events
            Events = get_all_events(),
            Body = jsx:encode(#{<<"events">> => Events}),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            {Body, Req3, State};
        _ ->
            {false, Req, State}
    end.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        Data = jsx:decode(Body, [return_maps]),
        Event = #timeline_event{
            id = maps:get(<<"id">>, Data),
            timestamp = maps:get(<<"timestamp">>, Data),
            type = safe_to_atom(maps:get(<<"type">>, Data, <<"message">>), 
                              [message, system, agent_action, error, warning, success]),
            source = safe_to_atom(maps:get(<<"source">>, Data, <<"system">>),
                                [user, agent, system]),
            agent_id = maps:get(<<"agentId">>, Data, null),
            agent_name = maps:get(<<"agentName">>, Data, null),
            conversation_id = maps:get(<<"conversationId">>, Data, null),
            content = maps:get(<<"content">>, Data),
            metadata = maps:get(<<"metadata">>, Data, #{})
        },
        
        % Store event
        ok = store_event(Event),
        
        % Broadcast to connected clients
        broadcast_event(Event),
        
        Response = #{
            <<"status">> => <<"saved">>,
            <<"id">> => Event#timeline_event.id
        },
        
        Body2 = jsx:encode(Response),
        Req3 = cowboy_req:set_resp_body(Body2, Req2),
        Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
        {true, Req4, State}
    catch
        _:Error ->
            logger:error("Error saving timeline event: ~p", [Error]),
            Req5 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                                   jsx:encode(#{<<"error">> => <<"Invalid request">>}), Req2),
            {false, Req5, State}
    end.

handle_delete(Req, State) ->
    % Clear all timeline events
    clear_all_events(),
    Response = #{<<"status">> => <<"cleared">>},
    Body = jsx:encode(Response),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
    {true, Req3, State}.

% Storage functions
store_event(Event) ->
    % Ensure storage is initialized before accessing
    init_storage(),
    % Store in ETS
    ets:insert(timeline_events, {Event#timeline_event.id, Event}),
    
    % Also persist to disk for recovery
    persist_event_to_disk(Event),
    ok.

get_all_events() ->
    % Ensure storage is initialized before accessing
    init_storage(),
    Events = ets:tab2list(timeline_events),
    SortedEvents = lists:sort(fun({_, A}, {_, B}) ->
        A#timeline_event.timestamp > B#timeline_event.timestamp
    end, Events),
    [event_to_map(Event) || {_, Event} <- SortedEvents].

clear_all_events() ->
    % Ensure storage is initialized before accessing
    init_storage(),
    ets:delete_all_objects(timeline_events),
    % Clear persisted file
    file:delete(get_timeline_file_path()).

event_to_map(#timeline_event{id = Id, timestamp = Timestamp, type = Type,
                            source = Source, agent_id = AgentId, agent_name = AgentName,
                            conversation_id = ConversationId, content = Content,
                            metadata = Metadata}) ->
    #{
        <<"id">> => Id,
        <<"timestamp">> => Timestamp,
        <<"type">> => atom_to_binary(Type, utf8),
        <<"source">> => atom_to_binary(Source, utf8),
        <<"agentId">> => AgentId,
        <<"agentName">> => AgentName,
        <<"conversationId">> => ConversationId,
        <<"content">> => Content,
        <<"metadata">> => Metadata
    }.

% Disk persistence
persist_event_to_disk(Event) ->
    FilePath = get_timeline_file_path(),
    EventMap = event_to_map(Event),
    EventJson = jsx:encode(EventMap),
    file:write_file(FilePath, [EventJson, "\n"], [append]).

load_events_from_disk() ->
    FilePath = get_timeline_file_path(),
    case file:read_file(FilePath) of
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global, trim]),
            lists:foreach(fun(Line) ->
                try
                    EventMap = jsx:decode(Line, [return_maps]),
                    Event = map_to_event(EventMap),
                    ets:insert(timeline_events, {Event#timeline_event.id, Event})
                catch
                    _:_ -> ok  % Skip malformed lines
                end
            end, Lines);
        {error, enoent} ->
            ok  % File doesn't exist yet
    end.

map_to_event(Map) ->
    #timeline_event{
        id = maps:get(<<"id">>, Map),
        timestamp = maps:get(<<"timestamp">>, Map),
        type = safe_to_atom(maps:get(<<"type">>, Map, <<"message">>),
                          [message, system, agent_action, error, warning, success]),
        source = safe_to_atom(maps:get(<<"source">>, Map, <<"system">>),
                            [user, agent, system]),
        agent_id = maps:get(<<"agentId">>, Map, null),
        agent_name = maps:get(<<"agentName">>, Map, null),
        conversation_id = maps:get(<<"conversationId">>, Map, null),
        content = maps:get(<<"content">>, Map),
        metadata = maps:get(<<"metadata">>, Map, #{})
    }.

% Safe atom conversion function
safe_to_atom(Binary, AllowedAtoms) when is_binary(Binary) ->
    try
        Atom = binary_to_existing_atom(Binary, utf8),
        case lists:member(Atom, AllowedAtoms) of
            true -> Atom;
            false -> hd(AllowedAtoms)  % Default to first allowed atom
        end
    catch
        error:badarg ->
            % Atom doesn't exist, try to match against allowed atoms
            BinaryLower = string:lowercase(Binary),
            case lists:filter(fun(A) -> 
                string:lowercase(atom_to_binary(A, utf8)) =:= BinaryLower 
            end, AllowedAtoms) of
                [Match|_] -> Match;
                [] -> hd(AllowedAtoms)  % Default to first allowed atom
            end
    end;
safe_to_atom(_, AllowedAtoms) ->
    hd(AllowedAtoms).  % Default for non-binary input

get_timeline_file_path() ->
    filename:join([code:priv_dir(agent_web), "data", "timeline_events.ndjson"]).

% Broadcast to WebSocket clients
broadcast_event(Event) ->
    EventMap = event_to_map(Event),
    Message = jsx:encode(#{
        <<"type">> => <<"timeline_event">>,
        <<"event">> => EventMap
    }),
    % Send to all connected WebSocket clients
    agent_ws_handler:broadcast(Message).

% Initialize storage
init_storage() ->
    case ets:info(timeline_events) of
        undefined ->
            ets:new(timeline_events, [named_table, public, {keypos, 1}]),
            % Load persisted events
            load_events_from_disk();
        _ ->
            ok
    end.