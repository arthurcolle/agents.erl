-module(agent_time_authority_tests).

-include_lib("eunit/include/eunit.hrl").

ordered_attestations_and_policy_gate_test() ->
    with_authority(healthy_tracking(), fun() ->
        {ok, First} = agent_time_authority:now(),
        {ok, Second} = agent_time_authority:now(),
        ?assertEqual(1, maps:get(sequence, First)),
        ?assertEqual(2, maps:get(sequence, Second)),
        ?assert(maps:get(attestation_sha256, First) =/= maps:get(attestation_sha256, Second)),
        ?assertEqual(synchronized, maps:get(sync_status, First)),
        {ok, #{decision := allow}} = agent_time_authority:authorize(#{
            operation => <<"agent-decision">>,
            max_uncertainty_us => 10000
        })
    end).

uncertain_clock_is_denied_test() ->
    with_authority(degraded_tracking(), fun() ->
        {error, #{decision := deny, reasons := Reasons}} = agent_time_authority:authorize(#{
            operation => <<"execution">>,
            max_uncertainty_us => 1000
        }),
        ?assert(lists:member(unsynchronized_clock, Reasons)),
        ?assert(lists:member(uncertainty_exceeds_policy, Reasons))
    end).

with_authority(Tracking, Test) ->
    _ = application:ensure_all_started(crypto),
    {ok, Pid} = agent_time_authority:start_link(#{
        probe => fun() -> {ok, Tracking} end,
        refresh_interval_ms => 60000,
        max_root_distance_us => 20000
    }),
    unlink(Pid),
    try
        Test()
    after
        ok = gen_server:stop(Pid)
    end.

healthy_tracking() ->
    <<"Reference ID    : 11FD142D (usqas2-ntp-002.aaplimg.com)\n",
      "Stratum         : 2\n",
      "System time     : 0.000223406 seconds slow of NTP time\n",
      "Last offset     : +0.000042546 seconds\n",
      "Root delay      : 0.008000178 seconds\n",
      "Root dispersion : 0.001362779 seconds\n",
      "Leap status     : Normal\n">>.

degraded_tracking() ->
    <<"Reference ID    : 00000000\n",
      "Stratum         : 0\n",
      "System time     : 0.025000000 seconds slow of NTP time\n",
      "Last offset     : +0.002000000 seconds\n",
      "Root delay      : 0.200000000 seconds\n",
      "Root dispersion : 0.050000000 seconds\n",
      "Leap status     : Not synchronised\n">>.
