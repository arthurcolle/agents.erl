-module(pdf_processing_integration).
-behaviour(gen_server).

%% API
-export([start_link/0,
         process_pdf/2,
         process_pdf_batch/2,
         get_processing_status/1,
         get_extraction_results/1,
         create_qa_agent/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PYTHON_PATH, "/Users/agent/www.erlang.org").

-record(state, {
    active_jobs = #{},      % job_id => job_info
    python_port,
    qa_agents = #{}        % agent_id => agent_info
}).

-record(job_info, {
    id,
    type,               % pdf_process, batch_process, qa_generation
    status,             % pending, processing, completed, failed
    input_files = [],
    output_data = #{},
    started_at,
    completed_at,
    error
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process_pdf(FilePath, Options) ->
    gen_server:call(?SERVER, {process_pdf, FilePath, Options}, 300000). % 5 min timeout

process_pdf_batch(FilePaths, Options) ->
    gen_server:call(?SERVER, {process_pdf_batch, FilePaths, Options}, 600000). % 10 min timeout

get_processing_status(JobId) ->
    gen_server:call(?SERVER, {get_status, JobId}).

get_extraction_results(JobId) ->
    gen_server:call(?SERVER, {get_results, JobId}).

create_qa_agent(JobId, AgentConfig) ->
    gen_server:call(?SERVER, {create_qa_agent, JobId, AgentConfig}).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    
    % Start Python integration port
    PythonPort = start_python_port(),
    
    State = #state{
        python_port = PythonPort
    },
    
    {ok, State}.

handle_call({process_pdf, FilePath, Options}, From, State) ->
    JobId = generate_job_id(),
    
    Job = #job_info{
        id = JobId,
        type = pdf_process,
        status = processing,
        input_files = [FilePath],
        started_at = erlang:system_time(second)
    },
    
    % Start async processing
    spawn_link(fun() ->
        Result = execute_pdf_processing(FilePath, Options, State#state.python_port),
        gen_server:cast(?SERVER, {job_completed, JobId, Result})
    end),
    
    NewState = State#state{
        active_jobs = maps:put(JobId, Job, State#state.active_jobs)
    },
    
    % Reply with job ID immediately
    gen_server:reply(From, {ok, JobId}),
    {noreply, NewState};

handle_call({process_pdf_batch, FilePaths, Options}, From, State) ->
    JobId = generate_job_id(),
    
    Job = #job_info{
        id = JobId,
        type = batch_process,
        status = processing,
        input_files = FilePaths,
        started_at = erlang:system_time(second)
    },
    
    % Start async batch processing
    spawn_link(fun() ->
        Results = process_batch_async(FilePaths, Options, State#state.python_port),
        gen_server:cast(?SERVER, {job_completed, JobId, Results})
    end),
    
    NewState = State#state{
        active_jobs = maps:put(JobId, Job, State#state.active_jobs)
    },
    
    gen_server:reply(From, {ok, JobId}),
    {noreply, NewState};

handle_call({get_status, JobId}, _From, State) ->
    case maps:find(JobId, State#state.active_jobs) of
        {ok, Job} ->
            Status = #{
                id => JobId,
                status => Job#job_info.status,
                type => Job#job_info.type,
                files_count => length(Job#job_info.input_files),
                started_at => Job#job_info.started_at,
                completed_at => Job#job_info.completed_at
            },
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_results, JobId}, _From, State) ->
    case maps:find(JobId, State#state.active_jobs) of
        {ok, #job_info{status = completed, output_data = Data}} ->
            {reply, {ok, Data}, State};
        {ok, #job_info{status = failed, error = Error}} ->
            {reply, {error, Error}, State};
        {ok, #job_info{status = Status}} ->
            {reply, {error, {still_processing, Status}}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_qa_agent, JobId, AgentConfig}, _From, State) ->
    case maps:find(JobId, State#state.active_jobs) of
        {ok, #job_info{status = completed, output_data = Data}} ->
            % Extract QA data
            QAData = maps:get(qa_pairs, Data, []),
            Concepts = maps:get(concepts, Data, []),
            
            % Create specialized QA agent
            AgentId = create_specialized_agent(QAData, Concepts, AgentConfig),
            
            NewQAAgents = maps:put(AgentId, #{
                job_id => JobId,
                created_at => erlang:system_time(second),
                config => AgentConfig
            }, State#state.qa_agents),
            
            {reply, {ok, AgentId}, State#state{qa_agents = NewQAAgents}};
        {ok, _} ->
            {reply, {error, job_not_completed}, State};
        error ->
            {reply, {error, job_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({job_completed, JobId, Result}, State) ->
    case maps:find(JobId, State#state.active_jobs) of
        {ok, Job} ->
            UpdatedJob = case Result of
                {ok, Data} ->
                    Job#job_info{
                        status = completed,
                        output_data = Data,
                        completed_at = erlang:system_time(second)
                    };
                {error, Reason} ->
                    Job#job_info{
                        status = failed,
                        error = Reason,
                        completed_at = erlang:system_time(second)
                    }
            end,
            
            % Record event
            mcp_monitor:record_event(pdf_processor, #{
                type => job_completed,
                severity => case Result of {ok, _} -> info; _ -> error end,
                message => io_lib:format("Job ~s completed with status: ~p", 
                                       [JobId, UpdatedJob#job_info.status]),
                metadata => #{job_id => JobId}
            }),
            
            NewJobs = maps:put(JobId, UpdatedJob, State#state.active_jobs),
            {noreply, State#state{active_jobs = NewJobs}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.python_port ->
    io:format("[PDF_INTEGRATION] Python port crashed: ~p~n", [Reason]),
    % Restart Python port
    NewPort = start_python_port(),
    {noreply, State#state{python_port = NewPort}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch port_close(State#state.python_port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
start_python_port() ->
    PythonScript = filename:join(?PYTHON_PATH, "erlang_pdf_bridge.py"),
    
    % Create bridge script if it doesn't exist
    ensure_bridge_script(PythonScript),
    
    PortOpts = [
        {spawn, "python3 " ++ PythonScript},
        {packet, 4},
        binary,
        exit_status
    ],
    
    open_port({spawn, "python3 " ++ PythonScript}, PortOpts).

ensure_bridge_script(ScriptPath) ->
    case filelib:is_file(ScriptPath) of
        true -> ok;
        false -> create_bridge_script(ScriptPath)
    end.

create_bridge_script(ScriptPath) ->
    Script = <<"#!/usr/bin/env python3
import sys
import json
import struct
import asyncio
from pathlib import Path

# Add the PDF processing directory to path
sys.path.insert(0, str(Path(__file__).parent))

from async_pdf_processor import PDFProcessor
from pdf_to_eval_pipeline import PDFToEvalPipeline
from embedding_qa_extractor import EmbeddingQAExtractor

class ErlangBridge:
    def __init__(self):
        self.pdf_processor = PDFProcessor()
        self.eval_pipeline = PDFToEvalPipeline()
        self.qa_extractor = EmbeddingQAExtractor()
    
    async def process_command(self, command):
        cmd_type = command.get('type')
        
        if cmd_type == 'process_pdf':
            return await self.process_pdf(command['file_path'], command.get('options', {}))
        elif cmd_type == 'process_batch':
            return await self.process_batch(command['file_paths'], command.get('options', {}))
        elif cmd_type == 'extract_qa':
            return await self.extract_qa(command['file_path'], command.get('options', {}))
        else:
            return {'error': f'Unknown command type: {cmd_type}'}
    
    async def process_pdf(self, file_path, options):
        try:
            # Process PDF and extract content/concepts
            result = await self.pdf_processor.process_pdf(file_path)
            
            # Generate QA pairs if requested
            if options.get('generate_qa', True):
                qa_result = await self.qa_extractor.extract_from_pdf(file_path)
                result['qa_pairs'] = qa_result.get('questions', [])
            
            return {'success': True, 'data': result}
        except Exception as e:
            return {'error': str(e)}
    
    async def process_batch(self, file_paths, options):
        results = []
        for file_path in file_paths:
            result = await self.process_pdf(file_path, options)
            results.append(result)
        return {'success': True, 'data': {'results': results}}
    
    async def extract_qa(self, file_path, options):
        try:
            result = await self.qa_extractor.extract_from_pdf(file_path)
            return {'success': True, 'data': result}
        except Exception as e:
            return {'error': str(e)}

async def main():
    bridge = ErlangBridge()
    
    while True:
        try:
            # Read length (4 bytes)
            length_bytes = sys.stdin.buffer.read(4)
            if not length_bytes:
                break
            
            length = struct.unpack('!I', length_bytes)[0]
            
            # Read message
            message_bytes = sys.stdin.buffer.read(length)
            command = json.loads(message_bytes.decode('utf-8'))
            
            # Process command
            result = await bridge.process_command(command)
            
            # Send response
            response = json.dumps(result).encode('utf-8')
            sys.stdout.buffer.write(struct.pack('!I', len(response)))
            sys.stdout.buffer.write(response)
            sys.stdout.buffer.flush()
            
        except Exception as e:
            error_response = json.dumps({'error': str(e)}).encode('utf-8')
            sys.stdout.buffer.write(struct.pack('!I', len(error_response)))
            sys.stdout.buffer.write(error_response)
            sys.stdout.buffer.flush()

if __name__ == '__main__':
    asyncio.run(main())
">>,
    file:write_file(ScriptPath, Script),
    file:change_mode(ScriptPath, 8#755).

execute_pdf_processing(FilePath, Options, Port) ->
    Command = #{
        type => <<"process_pdf">>,
        file_path => FilePath,
        options => Options
    },
    
    case call_python(Port, Command) of
        {ok, #{<<"success">> := true, <<"data">> := Data}} ->
            {ok, Data};
        {ok, #{<<"error">> := Error}} ->
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

process_batch_async(FilePaths, Options, Port) ->
    Command = #{
        type => <<"process_batch">>,
        file_paths => FilePaths,
        options => Options
    },
    
    case call_python(Port, Command) of
        {ok, #{<<"success">> := true, <<"data">> := Data}} ->
            {ok, Data};
        {ok, #{<<"error">> := Error}} ->
            {error, Error};
        {error, Reason} ->
            {error, Reason}
    end.

call_python(Port, Command) ->
    JsonData = jsx:encode(Command),
    
    try
        port_command(Port, JsonData),
        
        % Wait for response (with timeout)
        receive
            {Port, {data, ResponseData}} ->
                {ok, jsx:decode(ResponseData, [return_maps])}
        after 60000 ->
            {error, timeout}
        end
    catch
        error:Reason ->
            {error, Reason}
    end.

generate_job_id() ->
    Base = integer_to_binary(erlang:unique_integer([positive])),
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    <<"pdf_job_", Base/binary, "_", Timestamp/binary>>.

create_specialized_agent(QAData, Concepts, Config) ->
    % Create a new agent specialized in the PDF content
    AgentName = maps:get(name, Config, <<"PDF QA Agent">>),
    
    Tools = [
        #{
            name => <<"answer_from_pdf">>,
            description => <<"Answer questions based on extracted PDF content">>,
            parameters => #{
                question => #{type => string, required => true}
            }
        },
        #{
            name => <<"find_concept">>,
            description => <<"Find information about specific concepts from the PDF">>,
            parameters => #{
                concept => #{type => string, required => true}
            }
        }
    ],
    
    % Register the agent with the QA data
    AgentConfig = #{
        name => AgentName,
        description => <<"Specialized agent for answering questions from PDF content">>,
        capabilities => [<<"qa">>, <<"concept_search">>, <<"pdf_analysis">>],
        tools => Tools,
        knowledge_base => #{
            qa_pairs => QAData,
            concepts => Concepts
        }
    },
    
    {ok, AgentId} = agent_registry:register_agent(AgentConfig),
    AgentId.