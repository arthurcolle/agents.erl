#!/bin/bash

# Hot-swap utility for live code updates
# Usage: ./hot_swap.sh [node_name] [module] [action]
# Actions: compile, reload, info, list_modules

NODE_NAME=${1:-agents@$(hostname -f)}
MODULE=$2
ACTION=${3:-reload}

echo "Hot-swap operation on node: $NODE_NAME"
echo "Module: $MODULE"
echo "Action: $ACTION"

case $ACTION in
    "compile")
        echo "Compiling all modules..."
        ./rebar3 compile
        ;;
    "reload")
        if [ -z "$MODULE" ]; then
            echo "Error: Module name required for reload action"
            exit 1
        fi
        echo "Reloading module: $MODULE"
        erl -name hotswap_client@$(hostname -f) \
            -setcookie agents_cluster_cookie \
            -noshell \
            -eval "case net_adm:ping('$NODE_NAME') of pong -> io:format('Connected to ~s~n', ['$NODE_NAME']), rpc:call('$NODE_NAME', code, load_file, [$MODULE]); pang -> io:format('Failed to connect to ~s~n', ['$NODE_NAME']) end" \
            -s init stop
        ;;
    "info")
        if [ -z "$MODULE" ]; then
            echo "Error: Module name required for info action"
            exit 1
        fi
        echo "Getting module info: $MODULE"
        erl -name hotswap_client@$(hostname -f) \
            -setcookie agents_cluster_cookie \
            -noshell \
            -eval "case net_adm:ping('$NODE_NAME') of pong -> io:format('Connected to ~s~n', ['$NODE_NAME']), Info = rpc:call('$NODE_NAME', code, which, [$MODULE]), io:format('Module ~s location: ~p~n', [$MODULE, Info]); pang -> io:format('Failed to connect to ~s~n', ['$NODE_NAME']) end" \
            -s init stop
        ;;
    "list_modules")
        echo "Listing loaded modules..."
        erl -name hotswap_client@$(hostname -f) \
            -setcookie agents_cluster_cookie \
            -noshell \
            -eval "case net_adm:ping('$NODE_NAME') of pong -> io:format('Connected to ~s~n', ['$NODE_NAME']), Modules = rpc:call('$NODE_NAME', code, all_loaded, []), io:format('Loaded modules: ~p~n', [length(Modules)]); pang -> io:format('Failed to connect to ~s~n', ['$NODE_NAME']) end" \
            -s init stop
        ;;
    *)
        echo "Unknown action: $ACTION"
        echo "Available actions: compile, reload, info, list_modules"
        exit 1
        ;;
esac