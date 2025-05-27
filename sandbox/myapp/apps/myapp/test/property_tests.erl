%%%-------------------------------------------------------------------
%%% @doc
%%% Property-based Tests using PropEr
%%% Tests system properties and invariants
%%% @end
%%%-------------------------------------------------------------------
-module(property_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

%%%===================================================================
%%% Properties
%%%===================================================================

%% UUID Properties
prop_uuid_always_valid() ->
    ?FORALL(_, any(),
            begin
                UUID = uuid:v4(),
                uuid:is_valid(UUID)
            end).

prop_uuid_always_unique() ->
    ?FORALL(N, range(1, 1000),
            begin
                UUIDs = [uuid:v4() || _ <- lists:seq(1, N)],
                length(UUIDs) =:= length(lists:usort(UUIDs))
            end).

prop_uuid_format_consistent() ->
    ?FORALL(_, any(),
            begin
                UUID = uuid:v4(),
                36 =:= byte_size(UUID) andalso
                $- =:= binary:at(UUID, 8) andalso
                $- =:= binary:at(UUID, 13) andalso
                $- =:= binary:at(UUID, 18) andalso
                $- =:= binary:at(UUID, 23) andalso
                $4 =:= binary:at(UUID, 14) % Version 4
            end).

prop_uuid_roundtrip() ->
    ?FORALL(_, any(),
            begin
                Original = uuid:v4(),
                Binary = uuid:string_to_binary(Original),
                Restored = uuid:binary_to_string(Binary),
                string:lowercase(binary_to_list(Original)) =:= 
                    string:lowercase(Restored)
            end).

%% Agent Registry Properties
prop_registry_register_lookup() ->
    ?FORALL({Id, Pid}, {agent_id(), pid()},
            begin
                agent_registry:clear_all(),
                ok = agent_registry:register(Id, Pid),
                Result = agent_registry:lookup(Id),
                agent_registry:unregister(Id),
                Result =:= {ok, Pid}
            end).

prop_registry_no_duplicates() ->
    ?FORALL(Agents, non_empty(list({agent_id(), pid()})),
            begin
                agent_registry:clear_all(),
                % Register all agents
                lists:foreach(fun({Id, Pid}) ->
                    agent_registry:register(Id, Pid)
                end, Agents),
                
                % Get all registered
                AllIds = agent_registry:list_agents(),
                
                % Clean up
                agent_registry:clear_all(),
                
                % Check uniqueness
                length(AllIds) =:= length(lists:usort(AllIds))
            end).

prop_registry_concurrent_safe() ->
    ?FORALL(Operations, list(registry_operation()),
            begin
                agent_registry:clear_all(),
                Parent = self(),
                
                % Execute operations concurrently
                Pids = lists:map(fun(Op) ->
                    spawn(fun() ->
                        Result = execute_registry_op(Op),
                        Parent ! {self(), Result}
                    end)
                end, Operations),
                
                % Collect results
                Results = lists:map(fun(Pid) ->
                    receive
                        {Pid, Result} -> Result
                    after 5000 ->
                        timeout
                    end
                end, Pids),
                
                agent_registry:clear_all(),
                
                % No timeouts
                not lists:member(timeout, Results)
            end).

%% Agent Discovery Properties
prop_discovery_capability_search() ->
    ?FORALL({Agents, SearchCap}, {list(agent_spec()), capability()},
            begin
                agent_discovery:clear_all(),
                
                % Register agents
                lists:foreach(fun({Id, Caps}) ->
                    agent_discovery:register_agent(Id, Caps, #{})
                end, Agents),
                
                % Search for capability
                Found = agent_discovery:find_agents_by_capability(SearchCap),
                Expected = [Id || {Id, Caps} <- Agents, lists:member(SearchCap, Caps)],
                
                agent_discovery:clear_all(),
                
                % Found should match expected
                lists:sort(Found) =:= lists:sort(Expected)
            end).

prop_discovery_multi_capability() ->
    ?FORALL({Agent, RequiredCaps}, {agent_with_caps(), non_empty(list(capability()))},
            begin
                agent_discovery:clear_all(),
                {Id, Caps} = Agent,
                
                agent_discovery:register_agent(Id, Caps, #{}),
                Found = agent_discovery:find_agents_with_all_capabilities(RequiredCaps),
                
                agent_discovery:clear_all(),
                
                % Agent found iff it has all required capabilities
                case lists:all(fun(C) -> lists:member(C, Caps) end, RequiredCaps) of
                    true -> Found =:= [Id];
                    false -> Found =:= []
                end
            end).

%% Message Protocol Properties
prop_protocol_encode_decode() ->
    ?FORALL(Msg, message(),
            begin
                Encoded = agent_protocol:encode(Msg),
                case agent_protocol:decode(Encoded) of
                    {ok, Decoded} -> 
                        maps_equal(Msg, Decoded);
                    _ -> 
                        false
                end
            end).

prop_protocol_binary_safety() ->
    ?FORALL(BinaryData, binary(),
            begin
                Msg = #{
                    id => uuid:v4(),
                    type => <<"data">>,
                    payload => BinaryData
                },
                
                Encoded = agent_protocol:encode(Msg),
                {ok, Decoded} = agent_protocol:decode(Encoded),
                
                maps:get(payload, Decoded) =:= BinaryData
            end).

prop_protocol_compression_lossless() ->
    ?FORALL(Msg, large_message(),
            begin
                Compressed = agent_protocol:encode(Msg, #{compress => true}),
                {ok, Decompressed} = agent_protocol:decode(Compressed),
                
                maps_equal(Msg, Decompressed)
            end).

%% Tool Execution Properties
prop_tools_registration_idempotent() ->
    ?FORALL(Tool, tool_spec(),
            begin
                agent_tools:clear_all(),
                
                % Register twice
                ok = agent_tools:register_tool(Tool),
                Result = agent_tools:register_tool(Tool),
                
                agent_tools:clear_all(),
                
                % Second registration should fail or be ignored
                Result =/= ok
            end).

prop_tools_parameter_validation() ->
    ?FORALL({Tool, Params}, {tool_with_params(), tool_params()},
            begin
                agent_tools:clear_all(),
                ok = agent_tools:register_tool(Tool),
                
                Result = agent_tools:execute_tool(
                    maps:get(name, Tool), 
                    Params
                ),
                
                agent_tools:clear_all(),
                
                % Check if all required params are present
                RequiredParams = [P || P <- maps:get(parameters, Tool),
                                     maps:get(required, P, false)],
                AllPresent = lists:all(fun(P) ->
                    maps:is_key(maps:get(name, P), Params)
                end, RequiredParams),
                
                case AllPresent of
                    true -> element(1, Result) =:= ok;
                    false -> element(1, Result) =:= error
                end
            end).

%% Messenger Properties
prop_messenger_delivery() ->
    ?FORALL({From, To, Msg}, {agent_id(), agent_id(), message_content()},
            begin
                % Set up receiver
                ReceivedRef = make_ref(),
                Parent = self(),
                agent_messenger:register_handler(To, fun(M) ->
                    Parent ! {ReceivedRef, M}
                end),
                
                % Send message
                case agent_messenger:send_message(From, To, Msg) of
                    {ok, _MsgId} ->
                        receive
                            {ReceivedRef, Received} ->
                                InnerMsg = maps:get(message, Received),
                                maps_equal(Msg, InnerMsg)
                        after 1000 ->
                            false
                        end;
                    _ ->
                        false
                end
            end).

prop_messenger_broadcast() ->
    ?FORALL({From, Recipients, Msg}, 
            {agent_id(), non_empty(list(agent_id())), message_content()},
            begin
                UniqueRecipients = lists:usort(Recipients),
                ReceivedRef = make_ref(),
                Parent = self(),
                
                % Register handlers
                lists:foreach(fun(R) ->
                    agent_messenger:register_handler(R, fun(M) ->
                        Parent ! {ReceivedRef, R, M}
                    end)
                end, UniqueRecipients),
                
                % Broadcast
                case agent_messenger:broadcast_message(From, UniqueRecipients, Msg) of
                    {ok, _MsgIds} ->
                        % Collect received messages
                        Received = lists:map(fun(_) ->
                            receive
                                {ReceivedRef, R, _} -> R
                            after 1000 ->
                                timeout
                            end
                        end, UniqueRecipients),
                        
                        % All should receive
                        lists:sort(Received) =:= lists:sort(UniqueRecipients);
                    _ ->
                        false
                end
            end).

%% System Invariants
prop_system_agent_lifecycle() ->
    ?FORALL(AgentSpec, full_agent_spec(),
            begin
                Id = maps:get(id, AgentSpec),
                
                % Start agent
                case agent:start_agent(AgentSpec) of
                    {ok, Pid} ->
                        Started = is_pid(Pid),
                        
                        % Check registration
                        Registered = case agent_registry:lookup(Id) of
                            {ok, Pid} -> true;
                            _ -> false
                        end,
                        
                        % Check discovery
                        Caps = maps:get(capabilities, AgentSpec, []),
                        Discoverable = lists:all(fun(Cap) ->
                            Agents = agent_discovery:find_agents_by_capability(Cap),
                            lists:member(Id, Agents)
                        end, Caps),
                        
                        % Stop agent
                        ok = agent:stop_agent(Id),
                        
                        % Check cleanup
                        NotRegistered = agent_registry:lookup(Id) =:= {error, not_found},
                        NotDiscoverable = lists:all(fun(Cap) ->
                            Agents = agent_discovery:find_agents_by_capability(Cap),
                            not lists:member(Id, Agents)
                        end, Caps),
                        
                        Started andalso Registered andalso Discoverable andalso
                        NotRegistered andalso NotDiscoverable;
                    _ ->
                        false
                end
            end).

%%%===================================================================
%%% Generators
%%%===================================================================

agent_id() ->
    ?LET(N, pos_integer(),
         list_to_binary("agent_" ++ integer_to_list(N))).

capability() ->
    oneof([
        <<"compute">>, <<"analyze">>, <<"store">>, <<"retrieve">>,
        <<"transform">>, <<"validate">>, <<"monitor">>, <<"report">>
    ]).

pid() ->
    ?LET(_, any(), self()). % Use self() as a valid pid

registry_operation() ->
    oneof([
        {register, agent_id(), pid()},
        {unregister, agent_id()},
        {lookup, agent_id()},
        list_agents
    ]).

agent_spec() ->
    ?LET({Id, NumCaps}, {agent_id(), range(1, 5)},
         {Id, vector(NumCaps, capability())}).

agent_with_caps() ->
    ?LET({Id, Caps}, {agent_id(), non_empty(list(capability()))},
         {Id, lists:usort(Caps)}).

message() ->
    ?LET({Id, Type, Payload}, 
         {binary(), message_type(), message_payload()},
         #{
             id => Id,
             type => Type,
             payload => Payload,
             timestamp => pos_integer()
         }).

message_type() ->
    oneof([<<"request">>, <<"response">>, <<"notification">>, <<"error">>]).

message_payload() ->
    oneof([
        binary(),
        pos_integer(),
        boolean(),
        ?LET(N, range(0, 5), vector(N, simple_term())),
        ?LET(N, range(0, 5), 
             maps:from_list([{binary(), simple_term()} || _ <- lists:seq(1, N)]))
    ]).

message_content() ->
    ?LET({Type, Data}, {binary(), simple_term()},
         #{type => Type, data => Data}).

large_message() ->
    ?LET(Size, range(100, 1000),
         #{
             id => uuid:v4(),
             type => <<"data">>,
             payload => binary(Size),
             metadata => maps:from_list([{integer_to_binary(N), N} 
                                       || N <- lists:seq(1, 10)])
         }).

tool_spec() ->
    ?LET({Name, NumParams}, {tool_name(), range(0, 5)},
         #{
             name => Name,
             description => <<"Generated tool">>,
             parameters => vector(NumParams, parameter_spec()),
             handler => fun(_) -> {ok, <<"result">>} end
         }).

tool_name() ->
    ?LET(N, pos_integer(),
         list_to_binary("tool_" ++ integer_to_list(N))).

parameter_spec() ->
    ?LET({Name, Type, Required}, 
         {param_name(), param_type(), boolean()},
         #{
             name => Name,
             type => Type,
             required => Required
         }).

param_name() ->
    ?LET(N, range(1, 10),
         list_to_binary("param_" ++ integer_to_list(N))).

param_type() ->
    oneof([string, number, boolean, list, map, any]).

tool_with_params() ->
    ?LET(Tool, tool_spec(),
         Tool#{
             handler => fun(Params) -> 
                 {ok, maps:size(Params)}
             end
         }).

tool_params() ->
    ?LET(N, range(0, 10),
         maps:from_list([{param_name(), simple_term()} 
                        || _ <- lists:seq(1, N)])).

full_agent_spec() ->
    ?LET({Id, Caps, Tools}, 
         {agent_id(), list(capability()), list(tool_spec())},
         #{
             id => Id,
             name => <<"Test Agent">>,
             capabilities => lists:usort(Caps),
             tools => Tools,
             handlers => #{}
         }).

simple_term() ->
    oneof([
        binary(),
        pos_integer(),
        boolean(),
        atom(),
        float()
    ]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

execute_registry_op({register, Id, Pid}) ->
    agent_registry:register(Id, Pid);
execute_registry_op({unregister, Id}) ->
    agent_registry:unregister(Id);
execute_registry_op({lookup, Id}) ->
    agent_registry:lookup(Id);
execute_registry_op(list_agents) ->
    agent_registry:list_agents().

maps_equal(M1, M2) when is_map(M1), is_map(M2) ->
    Keys1 = maps:keys(M1),
    Keys2 = maps:keys(M2),
    
    lists:sort(Keys1) =:= lists:sort(Keys2) andalso
    lists:all(fun(K) ->
        V1 = maps:get(K, M1),
        V2 = maps:get(K, M2),
        values_equal(V1, V2)
    end, Keys1);
maps_equal(_, _) ->
    false.

values_equal(V1, V2) when is_map(V1), is_map(V2) ->
    maps_equal(V1, V2);
values_equal(V1, V2) when is_list(V1), is_list(V2) ->
    length(V1) =:= length(V2) andalso
    lists:all(fun({E1, E2}) -> values_equal(E1, E2) end,
              lists:zip(V1, V2));
values_equal(V1, V2) ->
    V1 =:= V2.

%%%===================================================================
%%% EUnit Wrapper for PropEr Tests  
%%%===================================================================

proper_test_() ->
    Props = [
        {uuid_always_valid, fun prop_uuid_always_valid/0},
        {uuid_always_unique, fun prop_uuid_always_unique/0},
        {uuid_format_consistent, fun prop_uuid_format_consistent/0},
        {uuid_roundtrip, fun prop_uuid_roundtrip/0},
        {registry_register_lookup, fun prop_registry_register_lookup/0},
        {registry_no_duplicates, fun prop_registry_no_duplicates/0},
        {discovery_capability_search, fun prop_discovery_capability_search/0},
        {protocol_encode_decode, fun prop_protocol_encode_decode/0},
        {tools_parameter_validation, fun prop_tools_parameter_validation/0}
    ],
    
    [
        {atom_to_list(Name), 
         {timeout, 60, 
          fun() -> 
              ?assert(proper:quickcheck(Fun(), [{numtests, 100}]))
          end}} 
        || {Name, Fun} <- Props
    ].

%%%===================================================================
%%% Performance Properties
%%%===================================================================

prop_uuid_generation_performance() ->
    ?FORALL(N, range(1000, 10000),
            begin
                {Time, UUIDs} = timer:tc(fun() ->
                    [uuid:v4() || _ <- lists:seq(1, N)]
                end),
                
                PerSecond = N * 1000000 / Time,
                
                % Should generate at least 50k UUIDs per second
                PerSecond > 50000 andalso
                length(UUIDs) =:= N andalso
                length(lists:usort(UUIDs)) =:= N
            end).

prop_message_encoding_performance() ->
    ?FORALL(Msg, message(),
            begin
                % Warm up
                _ = agent_protocol:encode(Msg),
                
                % Measure
                {EncTime, Encoded} = timer:tc(fun() ->
                    lists:foreach(fun(_) ->
                        agent_protocol:encode(Msg)
                    end, lists:seq(1, 1000))
                end),
                
                {DecTime, _} = timer:tc(fun() ->
                    lists:foreach(fun(_) ->
                        agent_protocol:decode(Encoded)
                    end, lists:seq(1, 1000))
                end),
                
                % Should encode/decode at least 10k messages per second
                EncRate = 1000 * 1000000 / EncTime,
                DecRate = 1000 * 1000000 / DecTime,
                
                EncRate > 10000 andalso DecRate > 10000
            end).