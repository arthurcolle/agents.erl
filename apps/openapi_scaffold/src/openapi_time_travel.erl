%%%-------------------------------------------------------------------
%%% @doc Time Travel Debugging for APIs
%%% Records and replays API requests with time-travel capabilities
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_time_travel).
-behaviour(gen_server).

-export([
    start_link/0,
    record_request/2,
    replay_request/2,
    replay_session/2,
    travel_to/2,
    get_timeline/1,
    export_session/2,
    import_session/2,
    enable_recording/1,
    disable_recording/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    recordings :: ets:tid(),
    sessions :: ets:tid(),
    active_recordings :: map(),
    replay_mode :: map(),
    time_index :: ets:tid()
}).

-record(recording, {
    id :: binary(),
    timestamp :: integer(),
    request :: map(),
    response :: map(),
    duration :: integer(),
    metadata :: map()
}).

-record(session, {
    id :: binary(),
    name :: binary(),
    start_time :: integer(),
    end_time :: integer(),
    recordings :: [binary()],
    metadata :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record an API request/response
record_request(SessionId, RequestData) ->
    gen_server:cast(?MODULE, {record, SessionId, RequestData}).

%% @doc Replay a specific request
replay_request(SessionId, RequestId) ->
    gen_server:call(?MODULE, {replay_request, SessionId, RequestId}).

%% @doc Replay an entire session
replay_session(SessionId, Options) ->
    gen_server:call(?MODULE, {replay_session, SessionId, Options}, 60000).

%% @doc Travel to specific point in time
travel_to(SessionId, Timestamp) ->
    gen_server:call(?MODULE, {travel_to, SessionId, Timestamp}).

%% @doc Get timeline of events
get_timeline(SessionId) ->
    gen_server:call(?MODULE, {get_timeline, SessionId}).

%% @doc Export session for sharing
export_session(SessionId, Format) ->
    gen_server:call(?MODULE, {export_session, SessionId, Format}).

%% @doc Import session from file
import_session(SessionId, Data) ->
    gen_server:call(?MODULE, {import_session, SessionId, Data}).

%% @doc Enable recording for session
enable_recording(SessionId) ->
    gen_server:call(?MODULE, {enable_recording, SessionId}).

%% @doc Disable recording for session
disable_recording(SessionId) ->
    gen_server:call(?MODULE, {disable_recording, SessionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS tables for storage
    Recordings = ets:new(time_travel_recordings, [
        set, 
        private, 
        {keypos, #recording.id}
    ]),
    
    Sessions = ets:new(time_travel_sessions, [
        set,
        private,
        {keypos, #session.id}
    ]),
    
    TimeIndex = ets:new(time_index, [
        ordered_set,
        private
    ]),
    
    State = #state{
        recordings = Recordings,
        sessions = Sessions,
        active_recordings = #{},
        replay_mode = #{},
        time_index = TimeIndex
    },
    
    %% Start periodic cleanup
    timer:send_interval(3600000, cleanup_old_recordings),
    
    {ok, State}.

handle_call({replay_request, SessionId, RequestId}, _From, State) ->
    Result = perform_replay_request(SessionId, RequestId, State),
    {reply, Result, State};

handle_call({replay_session, SessionId, Options}, _From, State) ->
    Result = perform_replay_session(SessionId, Options, State),
    {reply, Result, State};

handle_call({travel_to, SessionId, Timestamp}, _From, State) ->
    Result = perform_time_travel(SessionId, Timestamp, State),
    {reply, Result, State};

handle_call({get_timeline, SessionId}, _From, State) ->
    Timeline = build_timeline(SessionId, State),
    {reply, {ok, Timeline}, State};

handle_call({export_session, SessionId, Format}, _From, State) ->
    Result = export_session_data(SessionId, Format, State),
    {reply, Result, State};

handle_call({import_session, SessionId, Data}, _From, State) ->
    Result = import_session_data(SessionId, Data, State),
    {reply, Result, State};

handle_call({enable_recording, SessionId}, _From, State) ->
    NewActive = maps:put(SessionId, true, State#state.active_recordings),
    {reply, ok, State#state{active_recordings = NewActive}};

handle_call({disable_recording, SessionId}, _From, State) ->
    NewActive = maps:remove(SessionId, State#state.active_recordings),
    {reply, ok, State#state{active_recordings = NewActive}}.

handle_cast({record, SessionId, RequestData}, State) ->
    case maps:get(SessionId, State#state.active_recordings, false) of
        true ->
            NewState = record_request_data(SessionId, RequestData, State),
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

handle_info(cleanup_old_recordings, State) ->
    cleanup_old_data(State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Recording
%%====================================================================

record_request_data(SessionId, RequestData, State) ->
    %% Create recording entry
    RecordingId = generate_recording_id(),
    Timestamp = erlang:system_time(microsecond),
    
    Recording = #recording{
        id = RecordingId,
        timestamp = Timestamp,
        request = extract_request_data(RequestData),
        response = extract_response_data(RequestData),
        duration = maps:get(duration, RequestData, 0),
        metadata = extract_metadata(RequestData)
    },
    
    %% Store recording
    ets:insert(State#state.recordings, Recording),
    
    %% Update time index
    ets:insert(State#state.time_index, {{SessionId, Timestamp}, RecordingId}),
    
    %% Update session
    update_session(SessionId, RecordingId, Timestamp, State),
    
    State.

extract_request_data(RequestData) ->
    #{
        method => maps:get(method, RequestData),
        path => maps:get(path, RequestData),
        headers => sanitize_headers(maps:get(headers, RequestData, #{})),
        query_params => maps:get(query_params, RequestData, #{}),
        body => maps:get(request_body, RequestData, <<>>)
    }.

extract_response_data(RequestData) ->
    #{
        status => maps:get(status, RequestData),
        headers => sanitize_headers(maps:get(response_headers, RequestData, #{})),
        body => maps:get(response_body, RequestData, <<>>),
        error => maps:get(error, RequestData, undefined)
    }.

extract_metadata(RequestData) ->
    #{
        client_ip => maps:get(client_ip, RequestData, <<"unknown">>),
        user_agent => maps:get(user_agent, RequestData, <<"unknown">>),
        api_version => maps:get(api_version, RequestData, <<"v1">>),
        environment => maps:get(environment, RequestData, <<"production">>),
        tags => maps:get(tags, RequestData, [])
    }.

sanitize_headers(Headers) ->
    %% Remove sensitive headers
    SensitiveHeaders = [<<"authorization">>, <<"cookie">>, <<"x-api-key">>],
    maps:filter(fun(K, _V) ->
        not lists:member(string:lowercase(K), SensitiveHeaders)
    end, Headers).

update_session(SessionId, RecordingId, Timestamp, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [] ->
            %% Create new session
            Session = #session{
                id = SessionId,
                name = <<"Session ", SessionId/binary>>,
                start_time = Timestamp,
                end_time = Timestamp,
                recordings = [RecordingId],
                metadata = #{}
            },
            ets:insert(State#state.sessions, Session);
        [Session] ->
            %% Update existing session
            UpdatedSession = Session#session{
                end_time = Timestamp,
                recordings = [RecordingId | Session#session.recordings]
            },
            ets:insert(State#state.sessions, UpdatedSession)
    end.

%%====================================================================
%% Internal functions - Replay
%%====================================================================

perform_replay_request(SessionId, RequestId, State) ->
    case ets:lookup(State#state.recordings, RequestId) of
        [] ->
            {error, recording_not_found};
        [Recording] ->
            %% Execute the recorded request
            execute_replay(Recording, #{})
    end.

perform_replay_session(SessionId, Options, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [] ->
            {error, session_not_found};
        [Session] ->
            %% Get replay options
            Speed = maps:get(speed, Options, 1.0),
            StartTime = maps:get(start_time, Options, Session#session.start_time),
            EndTime = maps:get(end_time, Options, Session#session.end_time),
            
            %% Get recordings in time range
            Recordings = get_recordings_in_range(Session, StartTime, EndTime, State),
            
            %% Start replay process
            ReplayPid = spawn_link(fun() ->
                replay_recordings(Recordings, Speed, State)
            end),
            
            {ok, #{
                replay_pid => ReplayPid,
                recording_count => length(Recordings),
                duration => EndTime - StartTime
            }}
    end.

get_recordings_in_range(Session, StartTime, EndTime, State) ->
    %% Filter recordings by time range
    RecordingIds = Session#session.recordings,
    
    Recordings = lists:filtermap(fun(RecId) ->
        case ets:lookup(State#state.recordings, RecId) of
            [Rec] when Rec#recording.timestamp >= StartTime,
                      Rec#recording.timestamp =< EndTime ->
                {true, Rec};
            _ ->
                false
        end
    end, RecordingIds),
    
    %% Sort by timestamp
    lists:sort(fun(A, B) ->
        A#recording.timestamp < B#recording.timestamp
    end, Recordings).

replay_recordings([], _Speed, _State) ->
    ok;
replay_recordings([Recording | Rest], Speed, State) ->
    %% Execute recording
    execute_replay(Recording, #{}),
    
    %% Calculate delay to next recording
    case Rest of
        [] ->
            ok;
        [Next | _] ->
            Delay = (Next#recording.timestamp - Recording#recording.timestamp) / Speed,
            timer:sleep(trunc(Delay / 1000)) % Convert microseconds to milliseconds
    end,
    
    replay_recordings(Rest, Speed, State).

execute_replay(Recording, Options) ->
    %% Reconstruct and execute the request
    Request = Recording#recording.request,
    
    Method = binary_to_atom(maps:get(method, Request), utf8),
    Path = maps:get(path, Request),
    Headers = maps:get(headers, Request),
    Body = maps:get(body, Request),
    
    %% Make the actual request
    Url = build_replay_url(Path, Options),
    
    case hackney:request(Method, Url, maps:to_list(Headers), Body, []) of
        {ok, StatusCode, RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            
            %% Compare with recorded response
            RecordedResponse = Recording#recording.response,
            Comparison = compare_responses(
                #{status => StatusCode, body => RespBody, headers => RespHeaders},
                RecordedResponse
            ),
            
            {ok, #{
                recording => Recording,
                actual_response => #{
                    status => StatusCode,
                    body => RespBody,
                    headers => RespHeaders
                },
                comparison => Comparison
            }};
        {error, Reason} ->
            {error, #{
                recording => Recording,
                error => Reason
            }}
    end.

build_replay_url(Path, Options) ->
    BaseUrl = maps:get(base_url, Options, <<"http://localhost:8080">>),
    <<BaseUrl/binary, Path/binary>>.

compare_responses(Actual, Recorded) ->
    %% Compare status codes
    StatusMatch = maps:get(status, Actual) =:= maps:get(status, Recorded),
    
    %% Compare bodies (with normalization)
    BodyMatch = compare_bodies(
        maps:get(body, Actual),
        maps:get(body, Recorded)
    ),
    
    %% Compare important headers
    HeadersMatch = compare_headers(
        maps:get(headers, Actual),
        maps:get(headers, Recorded)
    ),
    
    #{
        status_match => StatusMatch,
        body_match => BodyMatch,
        headers_match => HeadersMatch,
        overall_match => StatusMatch andalso BodyMatch
    }.

compare_bodies(Body1, Body2) when is_binary(Body1), is_binary(Body2) ->
    %% Try to parse as JSON for better comparison
    case {jsx:decode(Body1, [return_maps]), jsx:decode(Body2, [return_maps])} of
        {{ok, Json1}, {ok, Json2}} ->
            normalize_json(Json1) =:= normalize_json(Json2);
        _ ->
            Body1 =:= Body2
    end;
compare_bodies(Body1, Body2) ->
    Body1 =:= Body2.

normalize_json(Json) when is_map(Json) ->
    %% Remove timestamps and other volatile fields
    maps:filter(fun(K, _V) ->
        not lists:member(K, [<<"timestamp">>, <<"requestId">>, <<"_id">>])
    end, Json);
normalize_json(Json) ->
    Json.

compare_headers(Headers1, Headers2) ->
    %% Compare only important headers
    ImportantHeaders = [<<"content-type">>, <<"content-length">>],
    
    lists:all(fun(Header) ->
        maps:get(Header, Headers1, undefined) =:= 
        maps:get(Header, Headers2, undefined)
    end, ImportantHeaders).

%%====================================================================
%% Internal functions - Time Travel
%%====================================================================

perform_time_travel(SessionId, Timestamp, State) ->
    %% Find all recordings up to the timestamp
    case ets:lookup(State#state.sessions, SessionId) of
        [] ->
            {error, session_not_found};
        [Session] ->
            %% Get system state at timestamp
            SystemState = reconstruct_state_at(SessionId, Timestamp, State),
            
            %% Return the state and nearby recordings
            NearbyRecordings = get_nearby_recordings(SessionId, Timestamp, State),
            
            {ok, #{
                timestamp => Timestamp,
                system_state => SystemState,
                nearby_recordings => NearbyRecordings
            }}
    end.

reconstruct_state_at(SessionId, Timestamp, State) ->
    %% Get all recordings up to timestamp
    Pattern = {{SessionId, '$1'}, '$2'},
    Guard = [{'=<', '$1', Timestamp}],
    Result = ['$2'],
    
    RecordingIds = ets:select(State#state.time_index, [{Pattern, Guard, Result}]),
    
    %% Reconstruct state by replaying recordings
    InitialState = #{},
    
    lists:foldl(fun(RecId, AccState) ->
        case ets:lookup(State#state.recordings, RecId) of
            [Recording] ->
                apply_recording_to_state(Recording, AccState);
            [] ->
                AccState
        end
    end, InitialState, RecordingIds).

apply_recording_to_state(Recording, State) ->
    %% Apply the effects of a recording to the state
    %% This is domain-specific and would need customization
    Request = Recording#recording.request,
    Response = Recording#recording.response,
    
    case maps:get(method, Request) of
        <<"POST">> ->
            %% Created resource
            add_resource_to_state(Request, Response, State);
        <<"PUT">> ->
            %% Updated resource
            update_resource_in_state(Request, Response, State);
        <<"DELETE">> ->
            %% Deleted resource
            remove_resource_from_state(Request, State);
        _ ->
            %% GET requests don't change state
            State
    end.

add_resource_to_state(Request, Response, State) ->
    %% Extract resource type and data
    Path = maps:get(path, Request),
    ResourceType = extract_resource_type(Path),
    ResourceData = maps:get(body, Response),
    
    Resources = maps:get(ResourceType, State, []),
    maps:put(ResourceType, [ResourceData | Resources], State).

update_resource_in_state(Request, Response, State) ->
    %% Similar to add but replaces existing
    State.

remove_resource_from_state(Request, State) ->
    %% Remove resource from state
    State.

extract_resource_type(Path) ->
    %% Extract resource type from path
    case binary:split(Path, <<"/">>, [global, trim]) of
        [<<"api">>, <<"v1">>, Type | _] -> Type;
        [<<"api">>, Type | _] -> Type;
        [Type | _] -> Type;
        _ -> <<"unknown">>
    end.

get_nearby_recordings(SessionId, Timestamp, State) ->
    %% Get recordings within 1 minute of timestamp
    Window = 60 * 1000000, % 1 minute in microseconds
    StartTime = Timestamp - Window,
    EndTime = Timestamp + Window,
    
    Pattern = {{SessionId, '$1'}, '$2'},
    Guard = [{'>=', '$1', StartTime}, {'=<', '$1', EndTime}],
    Result = ['$2'],
    
    RecordingIds = ets:select(State#state.time_index, [{Pattern, Guard, Result}]),
    
    %% Fetch full recordings
    lists:filtermap(fun(RecId) ->
        case ets:lookup(State#state.recordings, RecId) of
            [Rec] -> {true, recording_to_map(Rec)};
            [] -> false
        end
    end, RecordingIds).

%%====================================================================
%% Internal functions - Timeline
%%====================================================================

build_timeline(SessionId, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [] ->
            [];
        [Session] ->
            %% Get all recordings
            Recordings = lists:filtermap(fun(RecId) ->
                case ets:lookup(State#state.recordings, RecId) of
                    [Rec] -> {true, Rec};
                    [] -> false
                end
            end, Session#session.recordings),
            
            %% Sort by timestamp
            Sorted = lists:sort(fun(A, B) ->
                A#recording.timestamp < B#recording.timestamp
            end, Recordings),
            
            %% Build timeline events
            build_timeline_events(Sorted)
    end.

build_timeline_events(Recordings) ->
    lists:map(fun(Recording) ->
        #{
            id => Recording#recording.id,
            timestamp => Recording#recording.timestamp,
            type => determine_event_type(Recording),
            summary => build_event_summary(Recording),
            details => recording_to_map(Recording)
        }
    end, Recordings).

determine_event_type(#recording{request = #{method := Method}}) ->
    case Method of
        <<"GET">> -> <<"read">>;
        <<"POST">> -> <<"create">>;
        <<"PUT">> -> <<"update">>;
        <<"PATCH">> -> <<"partial_update">>;
        <<"DELETE">> -> <<"delete">>;
        _ -> <<"unknown">>
    end.

build_event_summary(#recording{request = Request, response = Response}) ->
    Method = maps:get(method, Request),
    Path = maps:get(path, Request),
    Status = maps:get(status, Response),
    
    <<Method/binary, " ", Path/binary, " -> ", (integer_to_binary(Status))/binary>>.

%%====================================================================
%% Internal functions - Import/Export
%%====================================================================

export_session_data(SessionId, Format, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [] ->
            {error, session_not_found};
        [Session] ->
            %% Get all recordings
            Recordings = get_session_recordings(Session, State),
            
            %% Export in requested format
            case Format of
                json -> export_as_json(Session, Recordings);
                har -> export_as_har(Session, Recordings);
                postman -> export_as_postman(Session, Recordings);
                _ -> {error, unsupported_format}
            end
    end.

get_session_recordings(Session, State) ->
    lists:filtermap(fun(RecId) ->
        case ets:lookup(State#state.recordings, RecId) of
            [Rec] -> {true, Rec};
            [] -> false
        end
    end, Session#session.recordings).

export_as_json(Session, Recordings) ->
    Data = #{
        <<"session">> => session_to_map(Session),
        <<"recordings">> => [recording_to_map(R) || R <- Recordings]
    },
    
    {ok, jsx:encode(Data, [pretty])}.

export_as_har(Session, Recordings) ->
    %% Export as HTTP Archive format
    Har = #{
        <<"log">> => #{
            <<"version">> => <<"1.2">>,
            <<"creator">> => #{
                <<"name">> => <<"OpenAPI Time Travel">>,
                <<"version">> => <<"1.0">>
            },
            <<"entries">> => [recording_to_har_entry(R) || R <- Recordings]
        }
    },
    
    {ok, jsx:encode(Har, [pretty])}.

export_as_postman(Session, Recordings) ->
    %% Export as Postman collection
    Collection = #{
        <<"info">> => #{
            <<"name">> => Session#session.name,
            <<"schema">> => <<"https://schema.getpostman.com/json/collection/v2.1.0/collection.json">>
        },
        <<"item">> => [recording_to_postman_item(R) || R <- Recordings]
    },
    
    {ok, jsx:encode(Collection, [pretty])}.

import_session_data(SessionId, Data, State) ->
    %% Parse imported data
    case jsx:decode(Data, [return_maps]) of
        {ok, #{<<"session">> := SessionData, <<"recordings">> := RecordingsData}} ->
            %% Import session
            Session = map_to_session(SessionId, SessionData),
            ets:insert(State#state.sessions, Session),
            
            %% Import recordings
            lists:foreach(fun(RecData) ->
                Recording = map_to_recording(RecData),
                ets:insert(State#state.recordings, Recording),
                ets:insert(State#state.time_index, 
                          {{SessionId, Recording#recording.timestamp}, Recording#recording.id})
            end, RecordingsData),
            
            {ok, SessionId};
        _ ->
            {error, invalid_format}
    end.

%%====================================================================
%% Internal functions - Data Conversion
%%====================================================================

recording_to_map(#recording{} = R) ->
    #{
        <<"id">> => R#recording.id,
        <<"timestamp">> => R#recording.timestamp,
        <<"request">> => R#recording.request,
        <<"response">> => R#recording.response,
        <<"duration">> => R#recording.duration,
        <<"metadata">> => R#recording.metadata
    }.

session_to_map(#session{} = S) ->
    #{
        <<"id">> => S#session.id,
        <<"name">> => S#session.name,
        <<"start_time">> => S#session.start_time,
        <<"end_time">> => S#session.end_time,
        <<"recording_count">> => length(S#session.recordings),
        <<"metadata">> => S#session.metadata
    }.

recording_to_har_entry(#recording{} = R) ->
    Request = R#recording.request,
    Response = R#recording.response,
    
    #{
        <<"startedDateTime">> => format_iso8601(R#recording.timestamp),
        <<"time">> => R#recording.duration / 1000, % Convert to milliseconds
        <<"request">> => #{
            <<"method">> => maps:get(method, Request),
            <<"url">> => build_full_url(Request),
            <<"headers">> => headers_to_har(maps:get(headers, Request, #{})),
            <<"postData">> => #{
                <<"text">> => maps:get(body, Request, <<>>)
            }
        },
        <<"response">> => #{
            <<"status">> => maps:get(status, Response),
            <<"headers">> => headers_to_har(maps:get(headers, Response, #{})),
            <<"content">> => #{
                <<"text">> => maps:get(body, Response, <<>>),
                <<"mimeType">> => maps:get(<<"content-type">>, 
                                          maps:get(headers, Response, #{}), 
                                          <<"application/json">>)
            }
        }
    }.

recording_to_postman_item(#recording{} = R) ->
    Request = R#recording.request,
    
    #{
        <<"name">> => build_request_name(Request),
        <<"request">> => #{
            <<"method">> => maps:get(method, Request),
            <<"url">> => #{
                <<"raw">> => build_full_url(Request)
            },
            <<"header">> => headers_to_postman(maps:get(headers, Request, #{})),
            <<"body">> => #{
                <<"mode">> => <<"raw">>,
                <<"raw">> => maps:get(body, Request, <<>>)
            }
        }
    }.

headers_to_har(Headers) ->
    maps:fold(fun(Name, Value, Acc) ->
        [#{<<"name">> => Name, <<"value">> => Value} | Acc]
    end, [], Headers).

headers_to_postman(Headers) ->
    maps:fold(fun(Name, Value, Acc) ->
        [#{<<"key">> => Name, <<"value">> => Value} | Acc]
    end, [], Headers).

build_full_url(#{path := Path}) ->
    %% In production, would include host and scheme
    <<"http://localhost:8080", Path/binary>>.

build_request_name(#{method := Method, path := Path}) ->
    <<Method/binary, " ", Path/binary>>.

format_iso8601(Timestamp) ->
    %% Convert microsecond timestamp to ISO8601
    Seconds = Timestamp div 1000000,
    {{Y, M, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(
        Seconds + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    ),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Y, M, D, H, Mi, S])).

map_to_session(SessionId, Data) ->
    #session{
        id = SessionId,
        name = maps:get(<<"name">>, Data),
        start_time = maps:get(<<"start_time">>, Data),
        end_time = maps:get(<<"end_time">>, Data),
        recordings = [],
        metadata = maps:get(<<"metadata">>, Data, #{})
    }.

map_to_recording(Data) ->
    #recording{
        id = maps:get(<<"id">>, Data),
        timestamp = maps:get(<<"timestamp">>, Data),
        request = maps:get(<<"request">>, Data),
        response = maps:get(<<"response">>, Data),
        duration = maps:get(<<"duration">>, Data),
        metadata = maps:get(<<"metadata">>, Data)
    }.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

generate_recording_id() ->
    base64:encode(crypto:strong_rand_bytes(12)).

cleanup_old_data(State) ->
    %% Remove recordings older than 30 days
    CutoffTime = erlang:system_time(microsecond) - (30 * 24 * 60 * 60 * 1000000),
    
    %% Find old recordings
    OldRecordings = ets:select(State#state.recordings, 
        [{#recording{timestamp = '$1', _ = '_'},
          [{'<', '$1', CutoffTime}],
          ['$_']}]),
    
    %% Delete old recordings
    lists:foreach(fun(Rec) ->
        ets:delete(State#state.recordings, Rec#recording.id),
        %% Also remove from time index
        ets:match_delete(State#state.time_index, {{'_', '_'}, Rec#recording.id})
    end, OldRecordings).