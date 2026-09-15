%%%-------------------------------------------------------------------
%%% @doc
%%% Shared, policy-aware clock authority for agent decisions.
%%%
%%% Wall-clock timestamps answer when an event happened; monotonic timestamps
%%% order work performed on this node. Every issued snapshot also carries a
%%% hash-chain attestation so callers can detect reordering within a runtime.
%%%-------------------------------------------------------------------
-module(agent_time_authority).
-behaviour(gen_server).

-export([start_link/0, start_link/1, now/0, health/0, refresh/0, authorize/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    config = #{} :: map(),
    probe :: fun(() -> {ok, binary()} | {error, term()}),
    health = #{} :: map(),
    chain_id :: binary(),
    previous_hash = <<>> :: binary(),
    sequence = 0 :: non_neg_integer(),
    refresh_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(#{}).

start_link(Options) when is_map(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Issue an authoritative snapshot for an agent or service.
now() ->
    gen_server:call(?SERVER, {now, #{}}, 5000).

%% @doc Return the latest local time-sync evidence and its computed uncertainty.
health() ->
    gen_server:call(?SERVER, health, 5000).

%% @doc Refresh Chrony evidence immediately.
refresh() ->
    gen_server:call(?SERVER, refresh, 5000).

%% @doc Gate a time-sensitive operation on a bounded sync uncertainty.
authorize(Policy) when is_map(Policy) ->
    gen_server:call(?SERVER, {authorize, Policy}, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Options) ->
    Config = maps:merge(default_config(), maps:remove(probe, Options)),
    Probe = maps:get(probe, Options, fun default_probe/0),
    Health = probe_health(Probe, Config),
    Timer = schedule_refresh(Config),
    {ok, #state{
        config = Config,
        probe = Probe,
        health = Health,
        chain_id = hex(crypto:strong_rand_bytes(16)),
        refresh_timer = Timer
    }}.

handle_call(health, _From, State0) ->
    {State, Health} = refresh_if_stale(State0),
    {reply, {ok, health_with_age(Health)}, State};

handle_call(refresh, _From, State) ->
    NewState = refresh_health(State),
    {reply, {ok, health_with_age(NewState#state.health)}, NewState};

handle_call({now, Context}, _From, State0) ->
    {State1, Health} = refresh_if_stale(State0),
    {Snapshot, State} = issue_snapshot(Context, Health, State1),
    {reply, {ok, Snapshot}, State};

handle_call({authorize, Policy}, _From, State0) ->
    {State1, Health} = refresh_if_stale(State0),
    {Snapshot, State} = issue_snapshot(Policy, Health, State1),
    Decision = gate_decision(Policy, Snapshot, State#state.config),
    audit_gate(Policy, Decision, Snapshot),
    {reply, Decision, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    NewState = refresh_health(State),
    {noreply, NewState#state{refresh_timer = schedule_refresh(NewState#state.config)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    maybe_cancel_timer(State#state.refresh_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Snapshot issuance and policy
%%%===================================================================

issue_snapshot(Context, Health0, State) ->
    UnixMs = erlang:system_time(millisecond),
    UnixNs = erlang:system_time(nanosecond),
    MonotonicNs = erlang:monotonic_time(nanosecond),
    Sequence = State#state.sequence + 1,
    Health = health_with_age(Health0),
    Payload = term_to_binary({
        State#state.chain_id,
        Sequence,
        UnixNs,
        MonotonicNs,
        maps:get(status, Health, unknown),
        Context
    }),
    Attestation = hex(crypto:hash(sha256, <<(State#state.previous_hash)/binary, Payload/binary>>)),
    Snapshot = #{
        utc => format_utc(UnixMs),
        unix_ms => UnixMs,
        unix_ns => UnixNs,
        monotonic_ns => MonotonicNs,
        authority_id => maps:get(authority_id, State#state.config),
        source => maps:get(source, Health, <<"unknown">>),
        reference_id => maps:get(reference_id, Health, <<"unknown">>),
        uncertainty_us => maps:get(uncertainty_us, Health, undefined),
        sync_status => maps:get(status, Health, unknown),
        synchronized => maps:get(synchronized, Health, false),
        health_age_ms => maps:get(age_ms, Health, undefined),
        sequence => Sequence,
        chain_id => State#state.chain_id,
        attestation_sha256 => Attestation
    },
    {Snapshot, State#state{sequence = Sequence, previous_hash = Attestation}}.

gate_decision(Policy, Snapshot, Config) ->
    MaxUncertainty = policy_value(max_uncertainty_us, Policy,
                                  maps:get(default_max_uncertainty_us, Config)),
    MaxAge = policy_value(max_age_ms, Policy, maps:get(max_sync_age_ms, Config)),
    Reasons = gate_reasons(Snapshot, MaxUncertainty, MaxAge),
    case Reasons of
        [] ->
            {ok, #{
                decision => allow,
                policy => normalized_policy(Policy, MaxUncertainty, MaxAge),
                time => Snapshot
            }};
        _ ->
            {error, #{
                decision => deny,
                reasons => Reasons,
                policy => normalized_policy(Policy, MaxUncertainty, MaxAge),
                time => Snapshot
            }}
    end.

gate_reasons(Snapshot, MaxUncertainty, MaxAge) ->
    SyncReasons = case maps:get(synchronized, Snapshot, false) of
        true -> [];
        false -> [unsynchronized_clock]
    end,
    AgeReasons = case maps:get(health_age_ms, Snapshot, MaxAge + 1) =< MaxAge of
        true -> [];
        false -> [stale_sync_evidence]
    end,
    UncertaintyReasons = case maps:get(uncertainty_us, Snapshot, undefined) of
        Value when is_integer(Value), Value =< MaxUncertainty -> [];
        _ -> [uncertainty_exceeds_policy]
    end,
    SyncReasons ++ AgeReasons ++ UncertaintyReasons.

normalized_policy(Policy, MaxUncertainty, MaxAge) ->
    #{
        operation => policy_value(operation, Policy, <<"unspecified">>),
        max_uncertainty_us => MaxUncertainty,
        max_age_ms => MaxAge,
        agent_id => policy_value(agent_id, Policy, undefined)
    }.

audit_gate(Policy, Decision, Snapshot) ->
    Event = #{
        type => time_gate,
        agent_id => policy_value(agent_id, Policy, undefined),
        operation => policy_value(operation, Policy, <<"unspecified">>),
        decision => case Decision of {ok, _} -> allow; {error, _} -> deny end,
        time => Snapshot
    },
    try
        timeline_event_store:store(Event)
    catch
        _:_ -> ok
    end,
    ok.

policy_value(Key, Policy, Default) ->
    BinaryKey = atom_to_binary(Key, utf8),
    maps:get(Key, Policy, maps:get(BinaryKey, Policy, Default)).

%%%===================================================================
%%% Chrony health collection
%%%===================================================================

default_config() ->
    ConfigFromEnv = case application:get_env(agents, time_authority) of
        {ok, Value} when is_map(Value) -> Value;
        _ -> #{}
    end,
    maps:merge(#{
        authority_id => <<"local-chrony">>,
        refresh_interval_ms => 15000,
        max_sync_age_ms => 60000,
        max_root_distance_us => 20000,
        default_max_uncertainty_us => 10000
    }, ConfigFromEnv).

default_probe() ->
    case os:find_executable("chronyc") of
        false ->
            {error, chronyc_not_found};
        Executable ->
            try
                Port = open_port(
                    {spawn_executable, Executable},
                    [binary, use_stdio, stderr_to_stdout, exit_status,
                     {line, 4096}]
                ),
                collect_chronyc_output(Port, [], chronyc_probe_timeout())
            catch
                Class:Reason -> {error, {chronyc_spawn_failed, Class, Reason}}
            end
    end.

chronyc_probe_timeout() ->
    case application:get_env(agents, time_authority_probe_timeout_ms, 2000) of
        Timeout when is_integer(Timeout), Timeout > 0 -> Timeout;
        _ -> 2000
    end.

collect_chronyc_output(Port, Chunks, Timeout) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_chronyc_output(Port, [Line, <<"\n">> | Chunks], Timeout);
        {Port, {data, {noeol, Chunk}}} ->
            collect_chronyc_output(Port, [Chunk | Chunks], Timeout);
        {Port, {exit_status, 0}} ->
            Output = unicode:characters_to_binary(lists:reverse(Chunks)),
            case binary:match(Output, <<"Stratum">>) of
                {_, _} -> {ok, Output};
                nomatch -> {error, {chronyc_failed, Output}}
            end;
        {Port, {exit_status, Status}} ->
            {error, {chronyc_exit, Status}};
        {Port, _Other} ->
            collect_chronyc_output(Port, Chunks, Timeout)
    after Timeout ->
        safe_port_close(Port),
        {error, chronyc_timeout}
    end.

safe_port_close(Port) ->
    try port_close(Port) catch _:_ -> ok end.

refresh_if_stale(State) ->
    Health = State#state.health,
    AgeMs = erlang:monotonic_time(millisecond) - maps:get(checked_at_monotonic_ms, Health, 0),
    case AgeMs > maps:get(refresh_interval_ms, State#state.config) of
        true ->
            NewState = refresh_health(State),
            {NewState, NewState#state.health};
        false ->
            {State, Health}
    end.

refresh_health(State) ->
    State#state{health = probe_health(State#state.probe, State#state.config)}.

probe_health(Probe, Config) ->
    CheckedAtMs = erlang:system_time(millisecond),
    CheckedAtMonotonicMs = erlang:monotonic_time(millisecond),
    ProbeResult = try
        Probe()
    catch
        Class:CaughtReason -> {probe_crash, Class, CaughtReason}
    end,
    case ProbeResult of
        {ok, Output} when is_binary(Output); is_list(Output) ->
            case parse_tracking(unicode:characters_to_binary(Output), Config) of
                {ok, Health} -> Health#{
                    source => <<"chronyc">>,
                    checked_at_unix_ms => CheckedAtMs,
                    checked_at_monotonic_ms => CheckedAtMonotonicMs
                };
                {error, ParseReason} -> unavailable_health(ParseReason, CheckedAtMs, CheckedAtMonotonicMs)
            end;
        {error, ProbeReason} -> unavailable_health(ProbeReason, CheckedAtMs, CheckedAtMonotonicMs);
        {probe_crash, _Class, CrashReason} ->
            unavailable_health(CrashReason, CheckedAtMs, CheckedAtMonotonicMs);
        Other -> unavailable_health({unexpected_probe_result, Other}, CheckedAtMs, CheckedAtMonotonicMs)
    end.

unavailable_health(Reason, CheckedAtMs, CheckedAtMonotonicMs) ->
    #{
        status => unavailable,
        synchronized => false,
        source => <<"chronyc">>,
        reason => format_reason(Reason),
        checked_at_unix_ms => CheckedAtMs,
        checked_at_monotonic_ms => CheckedAtMonotonicMs
    }.

parse_tracking(Output, Config) ->
    case {
        integer_field(<<"Stratum">>, Output),
        field(<<"Leap status">>, Output),
        seconds_field(<<"System time">>, Output),
        seconds_field(<<"Last offset">>, Output),
        seconds_field(<<"Root delay">>, Output),
        seconds_field(<<"Root dispersion">>, Output)
    } of
        {{ok, Stratum}, {ok, LeapStatus}, {ok, SystemTimeUs}, {ok, LastOffsetUs},
         {ok, RootDelayUs}, {ok, RootDispersionUs}} ->
            RootDistanceUs = round((RootDelayUs / 2) + RootDispersionUs),
            %% Root distance bounds source uncertainty. System time is the
            %% remaining local-clock correction, so include it conservatively
            %% before authorizing any time-sensitive work.
            UncertaintyUs = RootDistanceUs + SystemTimeUs,
            IsSynchronized = Stratum > 0 andalso LeapStatus =:= <<"Normal">> andalso
                             RootDistanceUs =< maps:get(max_root_distance_us, Config),
            {ok, #{
                status => case IsSynchronized of true -> synchronized; false -> degraded end,
                synchronized => IsSynchronized,
                stratum => Stratum,
                leap_status => LeapStatus,
                reference_id => optional_field(<<"Reference ID">>, Output, <<"unknown">>),
                system_time_us => SystemTimeUs,
                last_offset_us => LastOffsetUs,
                root_delay_us => RootDelayUs,
                root_dispersion_us => RootDispersionUs,
                uncertainty_us => UncertaintyUs
            }};
        Fields ->
            {error, {unparseable_tracking, Fields}}
    end.

field(Label, Output) ->
    Pattern = <<"(?m)^", Label/binary, "\\s*:\\s*(.*?)\\s*$">>,
    case re:run(Output, Pattern, [{capture, [1], binary}]) of
        {match, [Value]} -> {ok, Value};
        nomatch -> {error, {missing_field, Label}}
    end.

optional_field(Label, Output, Default) ->
    case field(Label, Output) of
        {ok, Value} -> Value;
        {error, _} -> Default
    end.

integer_field(Label, Output) ->
    case field(Label, Output) of
        {ok, Value} ->
            case re:run(Value, <<"^\\s*([0-9]+)">>, [{capture, [1], binary}]) of
                {match, [Number]} -> {ok, binary_to_integer(Number)};
                nomatch -> {error, {invalid_integer, Label, Value}}
            end;
        Error -> Error
    end.

seconds_field(Label, Output) ->
    case field(Label, Output) of
        {ok, Value} ->
            case re:run(Value,
                        <<"^\\s*([+-]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)\\s+seconds">>,
                        [{capture, [1], binary}]) of
                {match, [Number]} -> {ok, round(abs(number(Number)) * 1000000)};
                nomatch -> {error, {invalid_seconds, Label, Value}}
            end;
        Error -> Error
    end.

number(Binary) ->
    String = binary_to_list(Binary),
    case string:to_float(String) of
        {Float, _Rest} when is_float(Float) -> Float;
        {error, no_float} ->
            {Integer, _Rest} = string:to_integer(String),
            Integer
    end.

health_with_age(Health) ->
    AgeMs = erlang:monotonic_time(millisecond) - maps:get(checked_at_monotonic_ms, Health, 0),
    Health#{age_ms => max(0, AgeMs)}.

%%%===================================================================
%%% Formatting and housekeeping
%%%===================================================================

schedule_refresh(Config) ->
    erlang:send_after(maps:get(refresh_interval_ms, Config), self(), refresh).

maybe_cancel_timer(undefined) -> ok;
maybe_cancel_timer(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok.

format_utc(UnixMs) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(UnixMs, millisecond),
    Millisecond = UnixMs rem 1000,
    iolist_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millisecond]
    )).

format_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

hex(Binary) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [Byte]) || <<Byte>> <= Binary]).
