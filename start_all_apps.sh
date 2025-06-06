#!/bin/bash

echo "Starting Erlang with all applications..."
cd /Users/agent/agents.erl

erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -eval '
        io:format("Starting applications...~n"),
        
        % Start dependencies first
        application:ensure_all_started(jsx),
        application:ensure_all_started(jiffy),
        application:ensure_all_started(cowboy),
        application:ensure_all_started(uuid),
        application:ensure_all_started(hackney),
        application:ensure_all_started(gproc),
        
        % Start main applications
        io:format("Starting openai...~n"),
        application:ensure_all_started(openai),
        
        io:format("Starting agents...~n"), 
        application:ensure_all_started(agents),
        
        io:format("Starting agent_web...~n"),
        application:ensure_all_started(agent_web),
        
        io:format("Starting openapi_scaffold...~n"),
        application:ensure_all_started(openapi_scaffold),
        
        io:format("~nAll applications started!~n"),
        io:format("Web interface: http://localhost:8080~n~n").
    '