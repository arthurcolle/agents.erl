-module(timeline_ws_handler).
-behaviour(cowboy_websocket).

%% Cowboy WebSocket callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-record(state, {
    client_id :: binary(),
    subscriptions :: map(),
    filters :: map(),
    authenticated :: boolean()
}).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{
        client_id = generate_client_id(),
        subscriptions = #{},
        filters = #{},
        authenticated = false
    }}.

websocket_init(State) ->
    %% Send welcome message
    self() ! {send_welcome, State#state.client_id},
    {ok, State}.

websocket_handle({text, Data}, State) ->
    try
        Message = jsx:decode(Data, [return_maps]),
        handle_client_message(Message, State)
    catch
        _:_ ->
            ErrorResponse = jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"error">> => <<"invalid_json">>
            }),
            {reply, {text, ErrorResponse}, State}
    end;

websocket_handle({binary, _Data}, State) ->
    {ok, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_welcome, ClientId}, State) ->
    Welcome = jsx:encode(#{
        <<"type">> => <<"welcome">>,
        <<"client_id">> => ClientId,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"capabilities">> => [
            <<"timeline_events">>,
            <<"agent_events">>,
            <<"scheduled_tasks">>,
            <<"temporal_awareness">>
        ]
    }),
    {reply, {text, Welcome}, State};

websocket_info({timeline_event, Event}, State) ->
    %% Check if event matches any filters
    case should_send_event(Event, State) of
        true ->
            EventData = jsx:encode(#{
                <<"type">> => <<"timeline_event">>,
                <<"event">> => event_to_json(Event),
                <<"timestamp">> => erlang:system_time(millisecond)
            }),
            {reply, {text, EventData}, State};
        false ->
            {ok, State}
    end;

websocket_info({scheduled_task_update, Update}, State) ->
    case maps:get(scheduled_tasks, State#state.subscriptions, false) of
        true ->
            UpdateData = jsx:encode(#{
                <<"type">> => <<"scheduled_task_update">>,
                <<"update">> => Update,
                <<"timestamp">> => erlang:system_time(millisecond)
            }),
            {reply, {text, UpdateData}, State};
        false ->
            {ok, State}
    end;

websocket_info({temporal_update, AgentId, TemporalData}, State) ->
    case maps:get({temporal, AgentId}, State#state.subscriptions, false) of
        true ->
            UpdateData = jsx:encode(#{
                <<"type">> => <<"temporal_update">>,
                <<"agent_id">> => AgentId,
                <<"data">> => TemporalData,
                <<"timestamp">> => erlang:system_time(millisecond)
            }),
            {reply, {text, UpdateData}, State};
        false ->
            {ok, State}
    end;

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    %% Cleanup subscriptions
    cleanup_subscriptions(State),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_client_message(#{<<"type">> := <<"subscribe">>} = Msg, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<"timeline_events">>),
    Options = maps:get(<<"options">>, Msg, #{}),
    
    NewState = subscribe_to_topic(Topic, Options, State),
    
    Response = jsx:encode(#{
        <<"type">> => <<"subscribed">>,
        <<"topic">> => Topic,
        <<"status">> => <<"ok">>
    }),
    
    {reply, {text, Response}, NewState};

handle_client_message(#{<<"type">> := <<"unsubscribe">>} = Msg, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<"timeline_events">>),
    
    NewState = unsubscribe_from_topic(Topic, State),
    
    Response = jsx:encode(#{
        <<"type">> => <<"unsubscribed">>,
        <<"topic">> => Topic,
        <<"status">> => <<"ok">>
    }),
    
    {reply, {text, Response}, NewState};

handle_client_message(#{<<"type">> := <<"set_filter">>} = Msg, State) ->
    FilterType = maps:get(<<"filter_type">>, Msg, <<"event_type">>),
    FilterValue = maps:get(<<"filter_value">>, Msg),
    
    NewFilters = maps:put(FilterType, FilterValue, State#state.filters),
    NewState = State#state{filters = NewFilters},
    
    Response = jsx:encode(#{
        <<"type">> => <<"filter_set">>,
        <<"filter_type">> => FilterType,
        <<"status">> => <<"ok">>
    }),
    
    {reply, {text, Response}, NewState};

handle_client_message(#{<<"type">> := <<"get_timeline">>} = Msg, State) ->
    Limit = maps:get(<<"limit">>, Msg, 50),
    Offset = maps:get(<<"offset">>, Msg, 0),
    
    spawn(fun() ->
        case timeline_event_store:retrieve(Limit, Offset) of
            {ok, Events} ->
                self() ! {send_timeline, Events};
            _ ->
                ok
        end
    end),
    
    {ok, State};

handle_client_message(#{<<"type">> := <<"get_agent_timeline">>} = Msg, State) ->
    AgentId = maps:get(<<"agent_id">>, Msg),
    Options = maps:get(<<"options">>, Msg, #{}),
    
    spawn(fun() ->
        case timeline_event_store:retrieve_by_agent(AgentId, Options) of
            {ok, Events} ->
                self() ! {send_agent_timeline, AgentId, Events};
            _ ->
                ok
        end
    end),
    
    {ok, State};

handle_client_message(#{<<"type">> := <<"get_scheduled_tasks">>} = Msg, State) ->
    AgentId = maps:get(<<"agent_id">>, Msg, undefined),
    
    spawn(fun() ->
        Tasks = case AgentId of
            undefined ->
                case agent_scheduler_engine:list_all_tasks() of
                    {ok, T} -> T;
                    _ -> []
                end;
            _ ->
                case agent_scheduler_engine:get_agent_schedule(AgentId) of
                    {ok, T} -> T;
                    _ -> []
                end
        end,
        self() ! {send_scheduled_tasks, Tasks}
    end),
    
    {ok, State};

handle_client_message(#{<<"type">> := <<"get_temporal_context">>} = Msg, State) ->
    AgentId = maps:get(<<"agent_id">>, Msg),
    
    spawn(fun() ->
        case agent_temporal_awareness:get_temporal_context(AgentId) of
            {ok, Context} ->
                self() ! {send_temporal_context, AgentId, Context};
            _ ->
                ok
        end
    end),
    
    {ok, State};

handle_client_message(#{<<"type">> := <<"ping">>}, State) ->
    Pong = jsx:encode(#{
        <<"type">> => <<"pong">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    {reply, {text, Pong}, State};

handle_client_message(_Msg, State) ->
    ErrorResponse = jsx:encode(#{
        <<"type">> => <<"error">>,
        <<"error">> => <<"unknown_message_type">>
    }),
    {reply, {text, ErrorResponse}, State}.

subscribe_to_topic(<<"timeline_events">>, Options, State) ->
    %% Subscribe to timeline event stream
    FilterFun = create_filter_function(Options),
    timeline_event_store:stream_events(FilterFun, self()),
    
    NewSubscriptions = maps:put(timeline_events, true, State#state.subscriptions),
    State#state{subscriptions = NewSubscriptions};

subscribe_to_topic(<<"scheduled_tasks">>, _Options, State) ->
    %% Subscribe to scheduled task updates
    gproc:reg({p, l, scheduled_task_updates}),
    
    NewSubscriptions = maps:put(scheduled_tasks, true, State#state.subscriptions),
    State#state{subscriptions = NewSubscriptions};

subscribe_to_topic(<<"agent_events">>, #{<<"agent_id">> := AgentId}, State) ->
    %% Subscribe to specific agent events
    gproc:reg({p, l, {agent_events, AgentId}}),
    
    NewSubscriptions = maps:put({agent_events, AgentId}, true, State#state.subscriptions),
    State#state{subscriptions = NewSubscriptions};

subscribe_to_topic(<<"temporal_awareness">>, #{<<"agent_id">> := AgentId}, State) ->
    %% Subscribe to temporal awareness updates
    gproc:reg({p, l, {temporal_updates, AgentId}}),
    
    NewSubscriptions = maps:put({temporal, AgentId}, true, State#state.subscriptions),
    State#state{subscriptions = NewSubscriptions};

subscribe_to_topic(_Topic, _Options, State) ->
    State.

unsubscribe_from_topic(Topic, State) ->
    %% Remove subscription and cleanup
    case Topic of
        <<"timeline_events">> ->
            %% TODO: Implement unsubscribe from timeline_event_store
            ok;
        <<"scheduled_tasks">> ->
            catch gproc:unreg({p, l, scheduled_task_updates});
        _ ->
            ok
    end,
    
    NewSubscriptions = maps:remove(Topic, State#state.subscriptions),
    State#state{subscriptions = NewSubscriptions}.

create_filter_function(Options) ->
    fun(Event) ->
        check_event_filters(Event, Options)
    end.

check_event_filters(Event, Options) ->
    %% Check event type filter
    case maps:get(<<"event_types">>, Options, undefined) of
        undefined -> true;
        Types ->
            EventType = maps:get(type, Event, undefined),
            lists:member(EventType, Types)
    end andalso
    %% Check agent filter
    case maps:get(<<"agent_ids">>, Options, undefined) of
        undefined -> true;
        AgentIds ->
            AgentId = maps:get(agent_id, Event, undefined),
            lists:member(AgentId, AgentIds)
    end.

should_send_event(Event, State) ->
    %% Check if event should be sent based on subscriptions and filters
    case maps:get(timeline_events, State#state.subscriptions, false) of
        false -> false;
        true ->
            %% Apply filters
            check_filters(Event, State#state.filters)
    end.

check_filters(_Event, Filters) when map_size(Filters) =:= 0 ->
    true;
check_filters(Event, Filters) ->
    maps:fold(
        fun(FilterType, FilterValue, Acc) ->
            Acc andalso apply_filter(FilterType, FilterValue, Event)
        end,
        true,
        Filters
    ).

apply_filter(<<"event_type">>, Types, Event) ->
    EventType = maps:get(type, Event, undefined),
    lists:member(EventType, Types);
apply_filter(<<"agent_id">>, AgentIds, Event) ->
    AgentId = maps:get(agent_id, Event, undefined),
    lists:member(AgentId, AgentIds);
apply_filter(_, _, _) ->
    true.

event_to_json(Event) when is_map(Event) ->
    %% Convert event to JSON-safe format
    maps:map(
        fun(_K, V) when is_tuple(V) ->
            %% Convert timestamps and other tuples
            tuple_to_list(V);
           (_K, V) when is_atom(V) ->
            atom_to_binary(V, utf8);
           (_K, V) ->
            V
        end,
        Event
    ).

cleanup_subscriptions(State) ->
    %% Cleanup all subscriptions
    maps:foreach(
        fun(Key, _Value) ->
            case Key of
                timeline_events ->
                    %% TODO: Implement cleanup for timeline_event_store
                    ok;
                scheduled_tasks ->
                    catch gproc:unreg({p, l, scheduled_task_updates});
                {agent_events, AgentId} ->
                    catch gproc:unreg({p, l, {agent_events, AgentId}});
                {temporal, AgentId} ->
                    catch gproc:unreg({p, l, {temporal_updates, AgentId}});
                _ ->
                    ok
            end
        end,
        State#state.subscriptions
    ).

generate_client_id() ->
    agent_uuid:generate().