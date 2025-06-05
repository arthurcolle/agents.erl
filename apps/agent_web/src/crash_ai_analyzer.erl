-module(crash_ai_analyzer).
-behaviour(gen_server).

%% API
-export([start_link/0,
         analyze/2,
         generate_fixes/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% OpenAI integration - no header file needed

-record(state, {
    openai_client :: pid(),
    analysis_queue = [] :: list(),
    fix_queue = [] :: list()
}).

-record(analysis_result, {
    crash_id :: binary(),
    root_cause :: binary(),
    impact :: binary(),
    related_issues :: list(),
    confidence :: float(),
    suggested_actions :: list()
}).

-record(fix_proposal, {
    id :: binary(),
    crash_id :: binary(),
    description :: binary(),
    code_changes :: list(),
    risk_level :: low | medium | high,
    estimated_effort :: binary(),
    automated :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

analyze(Pid, CrashReport) ->
    gen_server:cast(Pid, {analyze_crash, CrashReport}).

generate_fixes(Pid, CrashId, Analysis) ->
    gen_server:cast(Pid, {generate_fixes, CrashId, Analysis}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize with a simple client reference
    %% The actual OpenAI calls will be made through openai_chat module
    {ok, #state{openai_client = undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({analyze_crash, CrashReport}, State) ->
    %% Perform AI analysis
    spawn_link(fun() -> perform_analysis(CrashReport, State#state.openai_client) end),
    {noreply, State};

handle_cast({generate_fixes, CrashId, Analysis}, State) ->
    %% Generate fix proposals
    spawn_link(fun() -> generate_fix_proposals(CrashId, Analysis, State#state.openai_client) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

perform_analysis(CrashReport, OpenAIClient) ->
    %% Build analysis prompt
    Prompt = build_analysis_prompt(CrashReport),
    
    %% Call OpenAI for analysis
    Messages = [
        #{role => system, content => <<"You are an expert Erlang/OTP crash analyzer. Analyze crash reports and identify root causes, impacts, and solutions."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.3,
            response_format => #{type => json_object}
        }),
        
        %% Parse AI response
        Analysis = parse_analysis_response(Response, CrashReport),
        
        %% Send result back to processor
        crash_report_processor ! {crash_analysis_complete, 
                                  element(2, CrashReport), 
                                  Analysis}
    catch
        Error:Reason ->
            error_logger:error_msg("AI analysis failed: ~p:~p~n", [Error, Reason])
    end.

build_analysis_prompt(CrashReport) ->
    {crash_report, _Id, _Timestamp, ProcessName, _Pid, ErrorType, Reason, 
     Module, Function, Line, Stacktrace, _State, _Severity, Frequency, 
     _LastOccurrence, _Status} = CrashReport,
     
    PromptParts = [
        <<"Analyze this Erlang crash report and provide a detailed analysis in JSON format:\n\n">>,
        <<"Process: ">>, atom_to_binary(ProcessName, utf8), <<"\n">>,
        <<"Module: ">>, atom_to_binary(Module, utf8), <<"\n">>,
        <<"Function: ">>, atom_to_binary(Function, utf8), <<"\n">>,
        <<"Line: ">>, integer_to_binary(Line), <<"\n">>,
        <<"Error Type: ">>, io_lib:format("~p", [ErrorType]), <<"\n">>,
        <<"Reason: ">>, io_lib:format("~p", [Reason]), <<"\n">>,
        <<"Frequency: ">>, integer_to_binary(Frequency), <<" occurrences\n">>,
        <<"Stacktrace: ">>, io_lib:format("~p", [Stacktrace]), <<"\n\n">>,
        <<"Provide analysis with the following structure:\n">>,
        <<"{\n">>,
        <<"  \"root_cause\": \"detailed explanation of why this crash occurred\",\n">>,
        <<"  \"impact\": \"impact on system functionality and users\",\n">>,
        <<"  \"related_issues\": [\"list of potentially related problems\"],\n">>,
        <<"  \"confidence\": 0.0-1.0,\n">>,
        <<"  \"suggested_actions\": [\"immediate actions to take\"]\n">>,
        <<"}\n">>
    ],
    
    iolist_to_binary(PromptParts).

parse_analysis_response(Response, CrashReport) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    JsonData = jsx:decode(Content, [return_maps]),
    
    #analysis_result{
        crash_id = element(2, CrashReport),
        root_cause = maps:get(<<"root_cause">>, JsonData),
        impact = maps:get(<<"impact">>, JsonData),
        related_issues = maps:get(<<"related_issues">>, JsonData, []),
        confidence = maps:get(<<"confidence">>, JsonData, 0.5),
        suggested_actions = maps:get(<<"suggested_actions">>, JsonData, [])
    }.

generate_fix_proposals(CrashId, Analysis, OpenAIClient) ->
    %% Build fix generation prompt
    Prompt = build_fix_prompt(CrashId, Analysis),
    
    Messages = [
        #{role => system, content => <<"You are an expert Erlang/OTP developer. Generate specific code fixes for crash issues. Provide practical, safe solutions."/utf8>>},
        #{role => user, content => Prompt}
    ],
    
    try
        {ok, Response} = openai_chat:create_completion(OpenAIClient, #{
            model => <<"gpt-4o">>,
            messages => Messages,
            temperature => 0.2,
            response_format => #{type => json_object}
        }),
        
        %% Parse fix proposals
        Fixes = parse_fix_response(Response, CrashId),
        
        %% Send result back
        crash_report_processor ! {fix_proposals_ready, CrashId, Fixes}
    catch
        Error:Reason ->
            error_logger:error_msg("Fix generation failed: ~p:~p~n", [Error, Reason])
    end.

build_fix_prompt(_CrashId, Analysis) ->
    #analysis_result{root_cause = RootCause, impact = Impact, 
                     related_issues = RelatedIssues,
                     suggested_actions = Actions} = Analysis,
    
    PromptParts = [
        <<"Generate specific code fixes for this crash issue:\n\n">>,
        <<"Root Cause: ">>, RootCause, <<"\n">>,
        <<"Impact: ">>, Impact, <<"\n">>,
        <<"Related Issues: ">>, jsx:encode(RelatedIssues), <<"\n">>,
        <<"Suggested Actions: ">>, jsx:encode(Actions), <<"\n\n">>,
        <<"Provide fixes in this JSON format:\n">>,
        <<"{\n">>,
        <<"  \"fixes\": [\n">>,
        <<"    {\n">>,
        <<"      \"description\": \"clear description of the fix\",\n">>,
        <<"      \"code_changes\": [\n">>,
        <<"        {\n">>,
        <<"          \"file\": \"path/to/file.erl\",\n">>,
        <<"          \"line\": 123,\n">>,
        <<"          \"old_code\": \"original code\",\n">>,
        <<"          \"new_code\": \"fixed code\",\n">>,
        <<"          \"explanation\": \"why this change fixes the issue\"\n">>,
        <<"        }\n">>,
        <<"      ],\n">>,
        <<"      \"risk_level\": \"low|medium|high\",\n">>,
        <<"      \"estimated_effort\": \"time estimate\",\n">>,
        <<"      \"automated\": true/false\n">>,
        <<"    }\n">>,
        <<"  ]\n">>,
        <<"}\n">>
    ],
    
    iolist_to_binary(PromptParts).

parse_fix_response(Response, CrashId) ->
    Content = maps:get(content, hd(maps:get(choices, Response))),
    JsonData = jsx:decode(Content, [return_maps]),
    Fixes = maps:get(<<"fixes">>, JsonData, []),
    
    [#fix_proposal{
        id = generate_fix_id(),
        crash_id = CrashId,
        description = maps:get(<<"description">>, Fix),
        code_changes = maps:get(<<"code_changes">>, Fix, []),
        risk_level = binary_to_atom(maps:get(<<"risk_level">>, Fix, <<"medium">>)),
        estimated_effort = maps:get(<<"estimated_effort">>, Fix, <<"unknown">>),
        automated = maps:get(<<"automated">>, Fix, false)
    } || Fix <- Fixes].

generate_fix_id() ->
    list_to_binary(io_lib:format("fix_~p", [erlang:system_time(microsecond)])).