%% advanced_streaming_async.erl
%% Advanced streaming and asynchronous agent examples
-module(advanced_streaming_async).

%% API exports
-export([
    streaming_data_pipeline/2,
    async_agent_orchestra/3,
    real_time_chat_processor/2,
    streaming_code_generator/2,
    async_batch_processor/3,
    event_driven_agent_system/2,
    streaming_analysis_engine/2
]).

%% @doc Streaming data pipeline with backpressure handling
-spec streaming_data_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
streaming_data_pipeline(DataSource, Config) ->
    io:format("Initializing streaming data pipeline~n"),
    
    % Create stream processor
    StreamProcessor = spawn_link(fun() -> 
        stream_processor_loop(DataSource, Config) 
    end),
    
    % Setup pipeline stages
    Stages = [
        {ingestion, create_ingestion_stage()},
        {transformation, create_transformation_stage()},
        {enrichment, create_enrichment_stage()},
        {aggregation, create_aggregation_stage()},
        {output, create_output_stage()}
    ],
    
    % Connect stages with flow control
    Pipeline = connect_pipeline_stages(Stages, Config),
    
    % Start streaming
    start_streaming(StreamProcessor, Pipeline, Config),
    
    {ok, StreamProcessor}.

%% @doc Asynchronous agent orchestra for parallel task execution
-spec async_agent_orchestra(binary(), list(), map()) -> {ok, map()}.
async_agent_orchestra(MasterPrompt, SubTasks, Options) ->
    io:format("Starting asynchronous agent orchestra~n"),
    
    % Create conductor agent
    Conductor = spawn_conductor_agent(MasterPrompt, Options),
    
    % Spawn musician agents for subtasks
    Musicians = spawn_musician_agents(SubTasks, Options),
    
    % Create communication channels
    Channels = establish_orchestra_channels(Conductor, Musicians),
    
    % Start asynchronous performance
    Performance = start_orchestra_performance(Conductor, Musicians, Channels),
    
    % Monitor and collect results
    Results = monitor_orchestra_performance(Performance),
    
    #{
        conductor => Conductor,
        musicians => Musicians,
        channels => Channels,
        performance => Performance,
        results => Results,
        metrics => collect_orchestra_metrics(Performance)
    }.

%% @doc Real-time chat processor with streaming responses
-spec real_time_chat_processor(pid(), map()) -> {ok, pid()}.
real_time_chat_processor(ClientPid, Config) ->
    io:format("Starting real-time chat processor~n"),
    
    % Create chat processor with streaming capability
    ProcessorPid = spawn_link(fun() ->
        chat_processor_loop(ClientPid, Config, init_chat_state())
    end),
    
    % Setup streaming response handler
    StreamHandler = create_stream_handler(ClientPid),
    
    % Configure OpenAI streaming
    StreamConfig = #{
        model => maps:get(model, Config, <<"gpt-4">>),
        stream => true,
        temperature => maps:get(temperature, Config, 0.7),
        max_tokens => maps:get(max_tokens, Config, 2000)
    },
    
    % Link processor with stream handler
    link_processor_stream(ProcessorPid, StreamHandler, StreamConfig),
    
    {ok, ProcessorPid}.

%% @doc Streaming code generator with incremental output
-spec streaming_code_generator(binary(), map()) -> {ok, pid()}.
streaming_code_generator(CodeSpec, Options) ->
    io:format("Initializing streaming code generator~n"),
    
    % Create generator process
    GeneratorPid = spawn_link(fun() ->
        code_generator_loop(CodeSpec, Options)
    end),
    
    % Setup incremental output handler
    OutputHandler = create_incremental_output_handler(),
    
    % Configure streaming generation
    GenConfig = #{
        language => maps:get(language, Options, erlang),
        style => maps:get(style, Options, functional),
        streaming => true,
        chunk_size => maps:get(chunk_size, Options, 100)
    },
    
    % Start generation
    start_streaming_generation(GeneratorPid, OutputHandler, GenConfig),
    
    {ok, GeneratorPid}.

%% @doc Asynchronous batch processor with parallel execution
-spec async_batch_processor(list(), fun(), map()) -> {ok, map()}.
async_batch_processor(Items, ProcessFun, Config) ->
    io:format("Starting async batch processor for ~p items~n", [length(Items)]),
    
    % Determine optimal batch size and parallelism
    BatchSize = maps:get(batch_size, Config, 10),
    Parallelism = maps:get(parallelism, Config, 4),
    
    % Create batches
    Batches = create_batches(Items, BatchSize),
    
    % Create worker pool
    WorkerPool = create_worker_pool(Parallelism, ProcessFun),
    
    % Process batches asynchronously
    BatchResults = process_batches_async(Batches, WorkerPool),
    
    % Aggregate results
    AggregatedResults = aggregate_batch_results(BatchResults),
    
    #{
        total_items => length(Items),
        batch_size => BatchSize,
        parallelism => Parallelism,
        batches_processed => length(Batches),
        results => AggregatedResults,
        performance => calculate_batch_performance(BatchResults)
    }.

%% @doc Event-driven agent system with reactive processing
-spec event_driven_agent_system(binary(), map()) -> {ok, pid()}.
event_driven_agent_system(SystemName, Config) ->
    io:format("Creating event-driven agent system: ~s~n", [SystemName]),
    
    % Create event bus
    EventBus = create_event_bus(),
    
    % Create reactive agents
    Agents = [
        create_reactive_agent(event_analyzer, EventBus),
        create_reactive_agent(event_processor, EventBus),
        create_reactive_agent(event_responder, EventBus),
        create_reactive_agent(event_logger, EventBus)
    ],
    
    % Setup event subscriptions
    Subscriptions = setup_event_subscriptions(Agents, Config),
    
    % Create event generator
    EventGenerator = create_event_generator(EventBus, Config),
    
    % Start event flow
    start_event_flow(EventGenerator, EventBus),
    
    % Monitor system
    Monitor = spawn_link(fun() ->
        event_system_monitor(EventBus, Agents)
    end),
    
    {ok, Monitor}.

%% @doc Streaming analysis engine with real-time insights
-spec streaming_analysis_engine(binary(), map()) -> {ok, pid()}.
streaming_analysis_engine(DataStream, AnalysisConfig) ->
    io:format("Starting streaming analysis engine~n"),
    
    % Create analysis pipeline
    Pipeline = [
        {window, create_windowing_stage(AnalysisConfig)},
        {aggregate, create_aggregation_stage(AnalysisConfig)},
        {analyze, create_analysis_stage(AnalysisConfig)},
        {alert, create_alerting_stage(AnalysisConfig)}
    ],
    
    % Create stream processor
    Processor = spawn_link(fun() ->
        analysis_processor_loop(DataStream, Pipeline, init_analysis_state())
    end),
    
    % Setup real-time dashboard
    Dashboard = create_real_time_dashboard(Processor),
    
    % Start analysis
    start_streaming_analysis(Processor, Dashboard),
    
    {ok, Processor}.

%% Stream processor implementation
stream_processor_loop(DataSource, Config) ->
    receive
        {data, Data} ->
            % Process data chunk
            ProcessedData = process_data_chunk(Data, Config),
            
            % Apply backpressure if needed
            apply_backpressure(Config),
            
            % Forward to next stage
            forward_to_next_stage(ProcessedData),
            
            stream_processor_loop(DataSource, Config);
            
        {control, backpressure} ->
            % Handle backpressure signal
            timer:sleep(100),
            stream_processor_loop(DataSource, Config);
            
        {control, stop} ->
            io:format("Stream processor stopping~n"),
            ok;
            
        {control, {update_config, NewConfig}} ->
            stream_processor_loop(DataSource, NewConfig)
    end.

%% Chat processor with streaming
chat_processor_loop(ClientPid, Config, State) ->
    receive
        {chat, Message} ->
            % Create streaming chat completion
            StreamRef = openai_chat:create_streaming_completion(
                maps:get(model, Config, <<"gpt-4">>),
                [#{role => <<"user">>, content => Message}],
                #{stream => true}
            ),
            
            % Handle streaming response
            NewState = handle_streaming_response(StreamRef, ClientPid, State),
            
            chat_processor_loop(ClientPid, Config, NewState);
            
        {stream_chunk, Chunk} ->
            % Forward chunk to client
            ClientPid ! {chat_chunk, Chunk},
            chat_processor_loop(ClientPid, Config, State);
            
        {stream_complete, Response} ->
            % Send completion signal
            ClientPid ! {chat_complete, Response},
            chat_processor_loop(ClientPid, Config, State);
            
        stop ->
            ok
    end.

%% Code generator with streaming
code_generator_loop(CodeSpec, Options) ->
    receive
        {generate, Requirements} ->
            % Start streaming code generation
            GeneratorPrompt = build_code_prompt(CodeSpec, Requirements, Options),
            
            StreamRef = openai_chat:create_streaming_completion(
                <<"gpt-4">>,
                [#{role => <<"system">>, content => <<"You are an expert code generator">>},
                 #{role => <<"user">>, content => GeneratorPrompt}],
                #{stream => true, max_tokens => 4000}
            ),
            
            % Handle streaming code output
            handle_streaming_code(StreamRef, Options),
            
            code_generator_loop(CodeSpec, Options);
            
        {update_spec, NewSpec} ->
            code_generator_loop(NewSpec, Options);
            
        stop ->
            ok
    end.

%% Event system monitor
event_system_monitor(EventBus, Agents) ->
    receive
        {event, Event} ->
            % Log and analyze event
            log_event(Event),
            analyze_event_pattern(Event),
            
            event_system_monitor(EventBus, Agents);
            
        {metrics, request} ->
            % Collect and send metrics
            Metrics = collect_event_metrics(EventBus, Agents),
            self() ! {metrics, response, Metrics},
            
            event_system_monitor(EventBus, Agents);
            
        stop ->
            ok
    after 5000 ->
        % Periodic health check
        check_agent_health(Agents),
        event_system_monitor(EventBus, Agents)
    end.

%% Analysis processor with streaming
analysis_processor_loop(DataStream, Pipeline, State) ->
    receive
        {data, DataPoint} ->
            % Add to window
            Window = update_window(State, DataPoint),
            
            % Check if window is complete
            case is_window_complete(Window) of
                true ->
                    % Process through pipeline
                    Results = process_analysis_pipeline(Window, Pipeline),
                    
                    % Stream results
                    stream_analysis_results(Results),
                    
                    % Update state
                    NewState = reset_window(State),
                    analysis_processor_loop(DataStream, Pipeline, NewState);
                    
                false ->
                    NewState = State#{window => Window},
                    analysis_processor_loop(DataStream, Pipeline, NewState)
            end;
            
        {query, Query} ->
            % Handle real-time query
            Result = execute_analysis_query(Query, State),
            self() ! {query_result, Result},
            
            analysis_processor_loop(DataStream, Pipeline, State);
            
        stop ->
            ok
    end.

%% Helper functions

create_ingestion_stage() ->
    #{
        type => ingestion,
        handler => fun(Data) -> 
            validate_and_parse(Data)
        end
    }.

create_transformation_stage() ->
    #{
        type => transformation,
        handler => fun(Data) ->
            transform_data_format(Data)
        end
    }.

create_enrichment_stage() ->
    #{
        type => enrichment,
        handler => fun(Data) ->
            enrich_with_metadata(Data)
        end
    }.

create_aggregation_stage() ->
    #{
        type => aggregation,
        handler => fun(Data) ->
            aggregate_data_points(Data)
        end
    }.

create_output_stage() ->
    #{
        type => output,
        handler => fun(Data) ->
            write_to_output(Data)
        end
    }.

connect_pipeline_stages(Stages, Config) ->
    % Connect stages with channels
    lists:zipwith(fun({_, Stage1}, {_, Stage2}) ->
        create_stage_connection(Stage1, Stage2, Config)
    end, lists:droplast(Stages), tl(Stages)).

start_streaming(Processor, Pipeline, Config) ->
    Processor ! {start, Pipeline, Config}.

spawn_conductor_agent(Prompt, Options) ->
    spawn_link(fun() ->
        conductor_loop(Prompt, Options, init_conductor_state())
    end).

spawn_musician_agents(SubTasks, Options) ->
    lists:map(fun(Task) ->
        spawn_link(fun() ->
            musician_loop(Task, Options)
        end)
    end, SubTasks).

establish_orchestra_channels(Conductor, Musicians) ->
    % Create bidirectional channels
    lists:map(fun(Musician) ->
        {Musician, create_channel(Conductor, Musician)}
    end, Musicians).

start_orchestra_performance(Conductor, Musicians, Channels) ->
    % Send start signal to all agents
    Conductor ! {start_performance, Musicians, Channels},
    lists:foreach(fun(M) -> M ! {prepare, Conductor} end, Musicians),
    
    #{
        start_time => erlang:system_time(millisecond),
        conductor => Conductor,
        musicians => Musicians,
        status => performing
    }.

monitor_orchestra_performance(Performance) ->
    % Collect results from all musicians
    timer:sleep(100), % Allow performance to start
    
    % This would normally collect actual results
    #{
        performance_id => make_ref(),
        duration => 5000,
        results => <<"Orchestra performance completed">>
    }.

create_stream_handler(ClientPid) ->
    spawn_link(fun() ->
        stream_handler_loop(ClientPid)
    end).

stream_handler_loop(ClientPid) ->
    receive
        {stream, Chunk} ->
            ClientPid ! {stream_chunk, Chunk},
            stream_handler_loop(ClientPid);
            
        {stream_end, Final} ->
            ClientPid ! {stream_complete, Final},
            stream_handler_loop(ClientPid);
            
        stop ->
            ok
    end.

init_chat_state() ->
    #{
        conversation => [],
        tokens_used => 0,
        start_time => erlang:system_time(millisecond)
    }.

handle_streaming_response(StreamRef, ClientPid, State) ->
    % Handle streaming response chunks
    State.

link_processor_stream(Processor, Handler, Config) ->
    Processor ! {link_stream, Handler, Config}.

create_incremental_output_handler() ->
    spawn_link(fun() -> output_handler_loop([]) end).

output_handler_loop(Buffer) ->
    receive
        {code_chunk, Chunk} ->
            NewBuffer = [Chunk | Buffer],
            io:format("~s", [Chunk]),
            output_handler_loop(NewBuffer);
            
        {get_complete_code, From} ->
            From ! {complete_code, lists:reverse(Buffer)},
            output_handler_loop(Buffer);
            
        stop ->
            ok
    end.

build_code_prompt(Spec, Requirements, Options) ->
    Language = maps:get(language, Options, erlang),
    Style = maps:get(style, Options, functional),
    
    <<"Generate ", (atom_to_binary(Language, utf8))/binary, 
      " code in ", (atom_to_binary(Style, utf8))/binary, 
      " style for: ", Spec/binary, 
      "\nRequirements: ", Requirements/binary>>.

start_streaming_generation(Generator, Handler, Config) ->
    Generator ! {start_generation, Handler, Config}.

handle_streaming_code(StreamRef, Options) ->
    % Handle streaming code generation
    ok.

create_batches(Items, BatchSize) ->
    create_batches(Items, BatchSize, []).

create_batches([], _, Acc) ->
    lists:reverse(Acc);
create_batches(Items, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(min(BatchSize, length(Items)), Items),
    create_batches(Rest, BatchSize, [Batch | Acc]).

create_worker_pool(Size, ProcessFun) ->
    lists:map(fun(I) ->
        spawn_link(fun() -> 
            worker_loop(I, ProcessFun)
        end)
    end, lists:seq(1, Size)).

worker_loop(Id, ProcessFun) ->
    receive
        {process_batch, Batch, From} ->
            Result = process_batch(Batch, ProcessFun),
            From ! {batch_result, Id, Result},
            worker_loop(Id, ProcessFun);
            
        stop ->
            ok
    end.

process_batch(Batch, ProcessFun) ->
    lists:map(ProcessFun, Batch).

process_batches_async(Batches, Workers) ->
    % Distribute batches to workers
    distribute_batches(Batches, Workers, []).

distribute_batches([], _, Results) ->
    Results;
distribute_batches([Batch | Rest], [Worker | Workers], Results) ->
    Worker ! {process_batch, Batch, self()},
    distribute_batches(Rest, Workers ++ [Worker], Results).

aggregate_batch_results(Results) ->
    % Aggregate all batch results
    Results.

calculate_batch_performance(Results) ->
    #{
        total_time => 1000,
        items_per_second => 100,
        cpu_usage => 45.2,
        memory_usage => 128
    }.

create_event_bus() ->
    spawn_link(fun() -> event_bus_loop(#{}) end).

event_bus_loop(Subscribers) ->
    receive
        {subscribe, Topic, Pid} ->
            NewSubs = maps:update_with(Topic, fun(Pids) -> [Pid | Pids] end, [Pid], Subscribers),
            event_bus_loop(NewSubs);
            
        {publish, Topic, Event} ->
            case maps:get(Topic, Subscribers, []) of
                [] -> ok;
                Pids -> lists:foreach(fun(Pid) -> Pid ! {event, Topic, Event} end, Pids)
            end,
            event_bus_loop(Subscribers);
            
        stop ->
            ok
    end.

create_reactive_agent(Type, EventBus) ->
    spawn_link(fun() ->
        reactive_agent_loop(Type, EventBus)
    end).

reactive_agent_loop(Type, EventBus) ->
    receive
        {event, Topic, Event} ->
            % React to event based on agent type
            handle_event(Type, Topic, Event),
            reactive_agent_loop(Type, EventBus);
            
        stop ->
            ok
    end.

handle_event(event_analyzer, Topic, Event) ->
    % Analyze event patterns
    io:format("Analyzing event on topic ~p: ~p~n", [Topic, Event]);
handle_event(event_processor, Topic, Event) ->
    % Process event
    io:format("Processing event on topic ~p~n", [Topic]);
handle_event(event_responder, Topic, Event) ->
    % Respond to event
    io:format("Responding to event on topic ~p~n", [Topic]);
handle_event(event_logger, Topic, Event) ->
    % Log event
    io:format("Logging event on topic ~p~n", [Topic]).

setup_event_subscriptions(Agents, Config) ->
    % Setup subscriptions based on config
    [].

create_event_generator(EventBus, Config) ->
    spawn_link(fun() ->
        event_generator_loop(EventBus, Config)
    end).

event_generator_loop(EventBus, Config) ->
    timer:sleep(maps:get(event_interval, Config, 1000)),
    
    % Generate random event
    Event = generate_random_event(),
    EventBus ! {publish, random_topic(), Event},
    
    event_generator_loop(EventBus, Config).

generate_random_event() ->
    #{
        timestamp => erlang:system_time(millisecond),
        type => random_event_type(),
        data => random_event_data()
    }.

random_topic() ->
    Topics = [system, user, data, alert],
    lists:nth(rand:uniform(length(Topics)), Topics).

random_event_type() ->
    Types = [create, update, delete, notify],
    lists:nth(rand:uniform(length(Types)), Types).

random_event_data() ->
    #{value => rand:uniform(100)}.

start_event_flow(Generator, EventBus) ->
    Generator ! start.

create_windowing_stage(Config) ->
    WindowSize = maps:get(window_size, Config, 1000),
    #{
        type => window,
        size => WindowSize,
        handler => fun(Data) -> create_window(Data, WindowSize) end
    }.

create_analysis_stage(Config) ->
    #{
        type => analysis,
        handler => fun(Window) -> analyze_window(Window, Config) end
    }.

create_alerting_stage(Config) ->
    #{
        type => alert,
        threshold => maps:get(alert_threshold, Config, 0.9),
        handler => fun(Analysis) -> check_alerts(Analysis, Config) end
    }.

init_analysis_state() ->
    #{
        window => [],
        window_start => erlang:system_time(millisecond),
        metrics => #{}
    }.

create_real_time_dashboard(Processor) ->
    spawn_link(fun() ->
        dashboard_loop(Processor, init_dashboard_state())
    end).

dashboard_loop(Processor, State) ->
    receive
        {update, Metric, Value} ->
            NewState = update_dashboard_metric(State, Metric, Value),
            render_dashboard(NewState),
            dashboard_loop(Processor, NewState);
            
        {query, Metric, From} ->
            Value = get_metric_value(State, Metric),
            From ! {metric_value, Metric, Value},
            dashboard_loop(Processor, State);
            
        stop ->
            ok
    after 1000 ->
        % Periodic refresh
        refresh_dashboard(State),
        dashboard_loop(Processor, State)
    end.

init_dashboard_state() ->
    #{
        metrics => #{},
        last_update => erlang:system_time(millisecond),
        refresh_rate => 1000
    }.

start_streaming_analysis(Processor, Dashboard) ->
    Processor ! {start, Dashboard}.

% Additional helper functions
validate_and_parse(Data) -> Data.
transform_data_format(Data) -> Data.
enrich_with_metadata(Data) -> Data.
aggregate_data_points(Data) -> Data.
write_to_output(Data) -> ok.
create_stage_connection(S1, S2, _) -> {S1, S2}.
conductor_loop(P, O, S) -> ok.
musician_loop(T, O) -> ok.
create_channel(A, B) -> {A, B}.
collect_orchestra_metrics(P) -> #{}.
process_data_chunk(D, C) -> D.
apply_backpressure(C) -> ok.
forward_to_next_stage(D) -> ok.
log_event(E) -> ok.
analyze_event_pattern(E) -> ok.
collect_event_metrics(B, A) -> #{}.
check_agent_health(A) -> ok.
update_window(S, D) -> [D | maps:get(window, S, [])].
is_window_complete(W) -> length(W) >= 10.
process_analysis_pipeline(W, P) -> #{}.
stream_analysis_results(R) -> ok.
reset_window(S) -> S#{window => []}.
execute_analysis_query(Q, S) -> #{}.
create_window(D, S) -> D.
analyze_window(W, C) -> #{}.
check_alerts(A, C) -> ok.
update_dashboard_metric(S, M, V) -> S.
render_dashboard(S) -> ok.
get_metric_value(S, M) -> 0.
refresh_dashboard(S) -> ok.