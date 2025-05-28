-module(examples_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    ExampleType = cowboy_req:binding(type, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"example">> := Example} ->
            Result = run_example(ExampleType, Example),
            Response = jsx:encode(#{result => Result}),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req1);
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

init(Req0 = #{method := <<"GET">>}, State) ->
    ExampleType = cowboy_req:binding(type, Req0),
    Examples = list_examples(ExampleType),
    Response = jsx:encode(#{examples => Examples}),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, Req0),
    {ok, Req, State}.

list_examples(<<"distributed">>) ->
    [
        #{name => <<"cluster">>, description => <<"Multi-node agent deployment">>},
        #{name => <<"research_team">>, description => <<"Collaborative research agents">>},
        #{name => <<"data_pipeline">>, description => <<"Distributed data processing">>},
        #{name => <<"swarm">>, description => <<"Swarm intelligence optimization">>},
        #{name => <<"hierarchy">>, description => <<"Hierarchical decision network">>},
        #{name => <<"fault_tolerance">>, description => <<"Fault-tolerant agent system">>}
    ];

list_examples(<<"streaming">>) ->
    [
        #{name => <<"pipeline">>, description => <<"Streaming data pipeline">>},
        #{name => <<"orchestra">>, description => <<"Async task orchestra">>},
        #{name => <<"realtime_chat">>, description => <<"Real-time AI chat">>},
        #{name => <<"code_generator">>, description => <<"Streaming code generation">>},
        #{name => <<"batch_processor">>, description => <<"Parallel batch processing">>},
        #{name => <<"event_system">>, description => <<"Event-driven agents">>}
    ];

list_examples(<<"composition">>) ->
    [
        #{name => <<"code_analysis">>, description => <<"Multi-stage code analysis">>},
        #{name => <<"debugging">>, description => <<"Autonomous debugging">>},
        #{name => <<"data_science">>, description => <<"ML pipeline workflow">>},
        #{name => <<"security_audit">>, description => <<"Security analysis chain">>},
        #{name => <<"infrastructure">>, description => <<"Infrastructure automation">>},
        #{name => <<"knowledge_extraction">>, description => <<"NLP knowledge pipeline">>}
    ];

list_examples(_) ->
    [].

run_example(<<"distributed">>, <<"cluster">>) ->
    case advanced_distributed_agents:distributed_cluster_example() of
        ok -> #{status => <<"success">>, message => <<"Cluster deployed">>};
        Error -> #{status => <<"error">>, message => list_to_binary(io_lib:format("~p", [Error]))}
    end;

run_example(<<"distributed">>, <<"research_team">>) ->
    Result = advanced_distributed_agents:collaborative_research_example(),
    #{status => <<"success">>, data => Result};

run_example(<<"streaming">>, <<"pipeline">>) ->
    Result = advanced_streaming_async:streaming_pipeline_example(),
    #{status => <<"success">>, data => Result};

run_example(<<"streaming">>, <<"realtime_chat">>) ->
    Result = advanced_streaming_async:realtime_chat_example(),
    #{status => <<"success">>, data => Result};

run_example(<<"composition">>, <<"code_analysis">>) ->
    Result = advanced_tool_composition:code_analysis_pipeline(),
    #{status => <<"success">>, data => Result};

run_example(_, _) ->
    #{status => <<"error">>, message => <<"Unknown example">>}.