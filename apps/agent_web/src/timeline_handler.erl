-module(timeline_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,
         handle_get/2, handle_post/2, handle_delete/2, init_storage/0, safe_to_atom/2,
         get_all_events/0]).

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
            % Get query params for pagination
            QsVals = cowboy_req:parse_qs(Req),
            Limit = case lists:keyfind(<<"limit">>, 1, QsVals) of
                {_, L} -> binary_to_integer(L);
                false -> 100
            end,
            Offset = case lists:keyfind(<<"offset">>, 1, QsVals) of
                {_, O} -> binary_to_integer(O);
                false -> 0
            end,
            
            % Get events from timeline_event_store
            case timeline_event_store:retrieve(Limit, Offset) of
                {ok, StoredEvents} ->
                    % Convert stored events to handler format
                    Events = [convert_stored_event(E) || E <- StoredEvents],
                    Body = jsx:encode(#{
                        <<"events">> => Events,
                        <<"total">> => timeline_event_store:get_event_count()
                    }),
                    Req2 = cowboy_req:set_resp_body(Body, Req),
                    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                    {Body, Req3, State};
                _ ->
                    % Fallback to old method if timeline_event_store is not available
                    Events = get_all_events(),
                    Body = jsx:encode(#{<<"events">> => Events}),
                    Req2 = cowboy_req:set_resp_body(Body, Req),
                    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                    {Body, Req3, State}
            end;
        _ ->
            {false, Req, State}
    end.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        Data = jsx:decode(Body, [return_maps]),
        Timestamp = maps:get(<<"timestamp">>, Data, timeline_timestamp(erlang:timestamp())),

        % Convert to timeline_event_store format
        EventData = #{
            id => maps:get(<<"id">>, Data, generate_id()),
            type => safe_to_atom(maps:get(<<"type">>, Data, <<"message">>), 
                              [message, system, agent_action, error, warning, success]),
            source => safe_to_atom(maps:get(<<"source">>, Data, <<"system">>),
                                [user, agent, system]),
            agent_id => maps:get(<<"agentId">>, Data, undefined),
            agent_name => maps:get(<<"agentName">>, Data, undefined),
            conversation_id => maps:get(<<"conversationId">>, Data, undefined),
            content => maps:get(<<"content">>, Data),
            metadata => maps:get(<<"metadata">>, Data, #{})
        },

        % timeline_event_store owns the shared ETS table and the durable NDJSON
        % log.  Keep this record only for the websocket notification; writing
        % it here as well used a second, incompatible table layout.
        Event = #timeline_event{
            id = maps:get(id, EventData),
            timestamp = Timestamp,
            type = maps:get(type, EventData),
            source = maps:get(source, EventData),
            agent_id = maps:get(agent_id, EventData),
            agent_name = maps:get(agent_name, EventData),
            conversation_id = maps:get(conversation_id, EventData),
            content = maps:get(content, EventData),
            metadata = maps:get(metadata, EventData)
        },
        % Validate and construct the whole event before publishing it. This
        % prevents a malformed HTTP request from leaving a durable event after
        % the handler has returned a 400 response.
        case timeline_event_store:store_sync(EventData) of
            {ok, StoredId} ->
                %% Broadcast only after the durable write succeeds.
                try broadcast_event(Event) of
                    _ -> ok
                catch
                    _:_ -> ok
                end,
                Response = #{
                    <<"status">> => <<"saved">>,
                    <<"id">> => StoredId
                },
                Body2 = jsx:encode(Response),
                Req3 = cowboy_req:set_resp_body(Body2, Req2),
                Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
                {true, Req4, State};
            {error, Reason} ->
                logger:error("Timeline persistence failed: ~p", [Reason]),
                Req3 = cowboy_req:reply(
                    503,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(#{<<"error">> => <<"Timeline storage unavailable">>}),
                    Req2
                ),
                {false, Req3, State}
        end
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

% Convert stored event from timeline_event_store format
convert_stored_event(StoredEvent) when is_map(StoredEvent) ->
    % Extract data from the stored event
    EventData = maps:get(data, StoredEvent, #{}),

    % Build the event map for frontend
    #{
        <<"id">> => timeline_id(maps:get(id, StoredEvent, generate_id())),
        <<"timestamp">> => timeline_timestamp(maps:get(timestamp, StoredEvent, undefined)),
        <<"type">> => timeline_text(event_value(type, EventData, maps:get(type, StoredEvent, system))),
        <<"source">> => timeline_text(event_value(source, EventData, system)),
        <<"agentId">> => timeline_optional(maps:get(agent_id, StoredEvent, event_value(agent_id, EventData, null))),
        <<"agentName">> => timeline_optional(event_value(agent_name, EventData, null)),
        <<"conversationId">> => timeline_optional(event_value(conversation_id, EventData, null)),
        <<"content">> => event_value(content, EventData, event_value(message, EventData, <<"">>)),
        <<"metadata">> => event_value(metadata, EventData, #{})
    }.

event_value(Key, EventData, Default) ->
    maps:get(Key, EventData, maps:get(atom_to_binary(Key, utf8), EventData, Default)).

timeline_timestamp({Mega, Sec, Micro}) ->
    %% Convert Erlang timestamps to a JSON string rather than a JSON byte list.
    MilliSec = (Mega * 1000000 + Sec) * 1000 + Micro div 1000,
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(MilliSec, [{unit, millisecond}, {offset, "Z"}])
    );
timeline_timestamp(Value) when is_binary(Value) ->
    timeline_text(Value);
timeline_timestamp(Value) when is_list(Value) ->
    timeline_text(Value);
timeline_timestamp(undefined) ->
    null;
timeline_timestamp(Value) ->
    timeline_text(Value).

timeline_id(Id) when is_binary(Id), byte_size(Id) =:= 16 ->
    list_to_binary(agent_uuid:to_string(Id));
timeline_id(Id) ->
    timeline_text(Id).

timeline_optional(undefined) ->
    null;
timeline_optional(<<"undefined">>) ->
    null;
timeline_optional(Value) ->
    Value.

timeline_text(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
timeline_text(Value) when is_binary(Value) ->
    case unicode:characters_to_binary(Value, utf8, utf8) of
        Text when is_binary(Text) -> Text;
        _ -> base64:encode(Value)
    end;
timeline_text(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Text when is_binary(Text) -> Text;
        _ -> base64:encode(iolist_to_binary(Value))
    end;
timeline_text(undefined) ->
    null;
timeline_text(null) ->
    null;
timeline_text(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

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

% Generate a unique ID for timeline events
generate_id() ->
    list_to_binary(agent_uuid:to_string(agent_uuid:generate())).

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
