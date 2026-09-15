-module(timeline_event_store_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

replay_is_read_only_and_deduplicated_test() ->
    Path = filename:join(
        "/tmp",
        "agents_timeline_" ++
            integer_to_list(erlang:unique_integer([positive])) ++
            ".ndjson"
    ),
    Original = #{
        <<"id">> => <<"persisted-event">>,
        <<"timestamp">> => <<"2026-07-22T12:00:00Z">>,
        <<"type">> => <<"message">>,
        <<"agentId">> => <<"agent-1">>,
        <<"content">> => <<"hello">>
    },
    ReplayEnvelope = #{
        <<"id">> => <<"accidental-replay-copy">>,
        <<"timestamp">> => <<"2026-07-22T12:01:00Z">>,
        <<"type">> => <<"unknown">>,
        <<"agent_id">> => <<"undefined">>,
        <<"data">> => Original
    },
    FileData = [
        jsx:encode(Original),
        <<"\n">>,
        jsx:encode(ReplayEnvelope),
        <<"\n">>
    ],
    ok = file:write_file(Path, FileData),
    {ok, BeforeInfo} = file:read_file_info(Path),
    application:set_env(agents, timeline_event_path, Path),
    try
        {ok, Pid} = timeline_event_store:start_link(),
        unlink(Pid),
        ?assertEqual(1, timeline_event_store:get_event_count()),
        {ok, AfterInfo} = file:read_file_info(Path),
        ?assertEqual(BeforeInfo#file_info.size, AfterInfo#file_info.size),
        ok = gen_server:stop(Pid)
    after
        application:unset_env(agents, timeline_event_path),
        file:delete(Path)
    end.

safe_pagination_and_stream_lifecycle_test_() ->
    {setup,
     fun() ->
         Path = filename:join(
             "/tmp",
             "agents_timeline_safe_" ++
                 integer_to_list(erlang:unique_integer([positive])) ++
                 ".ndjson"
         ),
         application:set_env(agents, timeline_event_path, Path),
         {ok, Pid} = timeline_event_store:start_link(),
         unlink(Pid),
         {Pid, Path}
     end,
     fun({Pid, Path}) ->
         catch gen_server:stop(Pid),
         application:unset_env(agents, timeline_event_path),
         file:delete(Path),
         ok
     end,
     fun(_) ->
         ?_test(begin
             Self = self(),
             ok = timeline_event_store:stream_events(fun(_Event) -> true end, Self),
             {ok, EventId} = timeline_event_store:store_sync(#{
                 id => <<"safe-event">>,
                 type => message,
                 agent_id => <<"safe-agent">>
             }),
             ?assertEqual(<<"safe-event">>, EventId),
             receive
                 {timeline_event, #{id := <<"safe-event">>}} -> ok
             after 1000 ->
                 ?assert(false, "stream subscriber did not receive event")
             end,
             ?assertEqual({ok, []}, timeline_event_store:retrieve(10, 10)),
             ?assertEqual({error, invalid_pagination}, timeline_event_store:retrieve(1001, 0)),
             ok = timeline_event_store:unsubscribe(Self),
             ok
         end)
     end}.
