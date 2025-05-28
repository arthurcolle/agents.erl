%% agent_tools.erl
%% Registry for agent tools and their executors
-module(agent_tools).
-behaviour(gen_server).

-export([
    start_link/1,
    register_tool/2,
    register_executor/3,
    unregister_tool/1,
    get_tools/1,
    execute_tool/2,
    list_tools/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    tools = #{} :: map(),       % Tool name -> schema mapping
    executors = #{} :: map()    % Tool name -> executor function mapping
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Register a tool schema
register_tool(Name, Schema) ->
    gen_server:call(?SERVER, {register_tool, Name, Schema}).

%% Register a tool executor function
register_executor(Name, ExecutorFn, Options) ->
    gen_server:call(?SERVER, {register_executor, Name, ExecutorFn, Options}).

%% Unregister a tool
unregister_tool(Name) ->
    gen_server:call(?SERVER, {unregister_tool, Name}).

%% Get tool schemas for specified tool names
get_tools(ToolNames) ->
    gen_server:call(?SERVER, {get_tools, ToolNames}).

%% Execute a tool with the given arguments
execute_tool(Name, Arguments) ->
    gen_server:call(?SERVER, {execute_tool, Name, Arguments}, infinity).

%% List all registered tools
list_tools() ->
    gen_server:call(?SERVER, list_tools).

%% gen_server callbacks
init(Options) ->
    % Initialize with default tools
    DefaultTools = maps:get(default_tools, Options, #{}),
    
    % Register predefined tools if requested
    RegisterPredefined = maps:get(register_predefined, Options, true),
    PredefinedTools = case RegisterPredefined of
        true -> predefined_tools();
        false -> #{}
    end,
    
    % Combine default and predefined tools
    AllTools = maps:merge(PredefinedTools, DefaultTools),
    
    % Initialize with default executors for predefined tools
    DefaultExecutors = maps:map(
        fun(_Name, _) -> 
            fun predefined_executor/2
        end,
        PredefinedTools
    ),
    
    {ok, #state{
        tools = AllTools,
        executors = DefaultExecutors
    }}.

handle_call({register_tool, Name, Schema}, _From, State) ->
    % Validate schema
    case validate_tool_schema(Schema) of
        ok ->
            % Update tools map
            NewTools = maps:put(Name, Schema, State#state.tools),
            {reply, ok, State#state{tools = NewTools}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({register_executor, Name, ExecutorFn, _Options}, _From, State) ->
    % Check if tool exists
    case maps:is_key(Name, State#state.tools) of
        true ->
            % Update executors map
            NewExecutors = maps:put(Name, ExecutorFn, State#state.executors),
            {reply, ok, State#state{executors = NewExecutors}};
        false ->
            {reply, {error, {unknown_tool, Name}}, State}
    end;

handle_call({unregister_tool, Name}, _From, State) ->
    % Remove tool and its executor
    NewTools = maps:remove(Name, State#state.tools),
    NewExecutors = maps:remove(Name, State#state.executors),
    {reply, ok, State#state{tools = NewTools, executors = NewExecutors}};

handle_call({get_tools, ToolNames}, _From, State) ->
    % Filter tools by name
    SelectedTools = lists:foldl(
        fun(ToolName, Acc) ->
            case maps:find(ToolName, State#state.tools) of
                {ok, Schema} -> [Schema | Acc];
                error -> Acc
            end
        end,
        [],
        ToolNames
    ),
    {reply, SelectedTools, State};

handle_call({execute_tool, Name, Arguments}, _From, State) ->
    % Look up the executor for this tool
    Result = case maps:find(Name, State#state.executors) of
        {ok, ExecutorFn} ->
            try
                ExecutorFn(Name, Arguments)
            catch
                E:R:S ->
                    {error, {tool_execution_failed, E, R, S}}
            end;
        error ->
            {error, {unknown_tool, Name}}
    end,
    {reply, Result, State};

handle_call(list_tools, _From, State) ->
    {reply, maps:keys(State#state.tools), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

%% Basic schema validation
validate_tool_schema(Schema) ->
    % Check required fields
    RequiredFields = [<<"name">>, <<"description">>, <<"parameters">>],
    HasAllRequired = lists:all(
        fun(Field) -> maps:is_key(Field, Schema) end,
        RequiredFields
    ),
    
    case HasAllRequired of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% Predefined tools
predefined_tools() ->
    #{
        shell => #{
            <<"name">> => <<"shell">>,
            <<"description">> => <<"Execute shell commands">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"command">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The shell command to execute">>
                    }
                },
                <<"required">> => [<<"command">>]
            }
        },
        
        file_read => #{
            <<"name">> => <<"file_read">>,
            <<"description">> => <<"Read file contents">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"path">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The path to the file to read">>
                    }
                },
                <<"required">> => [<<"path">>]
            }
        },
        
        file_write => #{
            <<"name">> => <<"file_write">>,
            <<"description">> => <<"Write content to a file">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"path">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The path to the file to write">>
                    },
                    <<"content">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The content to write to the file">>
                    }
                },
                <<"required">> => [<<"path">>, <<"content">>]
            }
        },
        
        http_request => #{
            <<"name">> => <<"http_request">>,
            <<"description">> => <<"Make an HTTP request">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"method">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The HTTP method to use">>
                    },
                    <<"url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The URL to make the request to">>
                    },
                    <<"headers">> => #{
                        <<"type">> => <<"object">>,
                        <<"description">> => <<"HTTP headers to include in the request">>
                    },
                    <<"body">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Request body">>
                    }
                },
                <<"required">> => [<<"method">>, <<"url">>]
            }
        },
        
        knowledge_base_retrieval => #{
            <<"name">> => <<"knowledge_base_retrieval">>,
            <<"description">> => <<"Search and retrieve information from domain-specific knowledge bases">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"domain">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The knowledge domain to search (e.g., psychology, medicine, education)">>
                    },
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query or topic to find information about">>
                    },
                    <<"max_results">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of results to return (default: 5)">>
                    }
                },
                <<"required">> => [<<"domain">>, <<"query">>]
            }
        }
    }.

%% Executor for predefined tools
predefined_executor(ToolName, Arguments) ->
    case ToolName of
        shell ->
            % Execute shell command
            Command = maps:get(<<"command">>, Arguments, <<"">>),
            execute_shell_command(Command);
        
        file_read ->
            % Read file
            Path = maps:get(<<"path">>, Arguments, <<"">>),
            read_file(Path);
        
        file_write ->
            % Write file
            Path = maps:get(<<"path">>, Arguments, <<"">>),
            Content = maps:get(<<"content">>, Arguments, <<"">>),
            write_file(Path, Content);
        
        http_request ->
            % Make HTTP request
            Method = maps:get(<<"method">>, Arguments, <<"GET">>),
            Url = maps:get(<<"url">>, Arguments, <<"">>),
            Headers = maps:get(<<"headers">>, Arguments, #{}),
            Body = maps:get(<<"body">>, Arguments, <<"">>),
            http_request(Method, Url, Headers, Body);
        
        knowledge_base_retrieval ->
            % Search knowledge base
            Domain = maps:get(<<"domain">>, Arguments, <<"">>),
            Query = maps:get(<<"query">>, Arguments, <<"">>),
            MaxResults = maps:get(<<"max_results">>, Arguments, 5),
            knowledge_base_search(Domain, Query, MaxResults);
        
        _ ->
            {error, {unknown_predefined_tool, ToolName}}
    end.

%% Execute a shell command
execute_shell_command(Command) ->
    % Convert binary to string if needed
    CmdStr = case is_binary(Command) of
        true -> binary_to_list(Command);
        false -> Command
    end,
    
    % Execute the command
    Port = open_port({spawn, CmdStr}, [exit_status, stderr_to_stdout, {line, 1000}]),
    collect_port_output(Port, []).

%% Read a file
read_file(Path) ->
    % Convert binary to string if needed
    PathStr = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,
    
    % Read the file
    case file:read_file(PathStr) of
        {ok, Content} -> Content;
        {error, Reason} -> {error, {file_read_error, Reason}}
    end.

%% Write to a file
write_file(Path, Content) ->
    % Convert binary to string if needed
    PathStr = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,
    
    % Write the file
    case file:write_file(PathStr, Content) of
        ok -> <<"File written successfully">>;
        {error, Reason} -> {error, {file_write_error, Reason}}
    end.

%% Make an HTTP request
http_request(Method, Url, Headers, Body) ->
    % Convert method to atom
    MethodAtom = case is_binary(Method) of
        true -> binary_to_atom(string:lowercase(Method), utf8);
        false -> Method
    end,
    
    % Convert URL to string
    UrlStr = case is_binary(Url) of
        true -> binary_to_list(Url);
        false -> Url
    end,
    
    % Convert headers to proplists
    HeadersList = maps:fold(
        fun(K, V, Acc) ->
            KeyStr = case is_binary(K) of
                true -> binary_to_list(K);
                false -> K
            end,
            ValStr = case is_binary(V) of
                true -> binary_to_list(V);
                false -> V
            end,
            [{KeyStr, ValStr} | Acc]
        end,
        [],
        Headers
    ),
    
    % Make the request
    case MethodAtom of
        get ->
            httpc:request(get, {UrlStr, HeadersList}, [], []);
        _ when MethodAtom =:= post; MethodAtom =:= put; MethodAtom =:= patch ->
            % For requests with body
            ContentType = proplists:get_value("Content-Type", HeadersList, "application/json"),
            httpc:request(MethodAtom, {UrlStr, HeadersList, ContentType, Body}, [], []);
        _ ->
            % Other methods
            httpc:request(MethodAtom, {UrlStr, HeadersList}, [], [])
    end.

%% Collect output from a port
collect_port_output(Port, Output) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_port_output(Port, [Line, "\n" | Output]);
        {Port, {data, {noeol, Line}}} ->
            collect_port_output(Port, [Line | Output]);
        {Port, {exit_status, Status}} ->
            case Status of
                0 -> list_to_binary(lists:reverse(Output));
                _ -> {error, {command_failed, Status, list_to_binary(lists:reverse(Output))}}
            end
    end.

%% Search knowledge base
knowledge_base_search(Domain, Query, MaxResults) ->
    % Convert binary to string if needed
    DomainStr = case is_binary(Domain) of
        true -> binary_to_list(Domain);
        false -> Domain
    end,
    
    QueryStr = case is_binary(Query) of
        true -> binary_to_list(Query);
        false -> Query
    end,
    
    % Use the knowledge base retrieval module
    case knowledge_base_retrieval:search_knowledge_base(DomainStr, QueryStr, fun(Result) -> Result end) of
        {ok, Results} ->
            % Limit results if specified
            LimitedResults = lists:sublist(Results, MaxResults),
            #{
                <<"domain">> => Domain,
                <<"query">> => Query,
                <<"results">> => LimitedResults,
                <<"total_found">> => length(Results)
            };
        {error, Reason} ->
            {error, {knowledge_base_error, Reason}}
    end.