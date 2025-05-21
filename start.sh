#!/bin/bash

# Make script executable with: chmod +x start.sh

# Check if --tests flag is provided
if [[ "$1" == "--tests" ]]; then
    echo "Running tests..."
    # Compile test modules first
    echo "Compiling test modules..."
    cd tests && erlc *.erl && cd ..
    # Start Erlang shell with test runner
    ./rebar3 shell --eval "code:add_patha(\"tests\"), test_runner:run_all()."
else
    # Start the Erlang node with our application
    ./rebar3 shell
fi