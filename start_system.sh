#!/bin/bash

echo "🚀 Starting Agents.erl System"
echo "============================="
echo ""

# Check if already running
if pgrep -f "beam.*agents" > /dev/null; then
    echo "⚠️  System appears to be already running"
    echo "   Use 'pkill -f \"beam.*agents\"' to stop it first"
    exit 1
fi

echo "📋 Pre-flight checks..."
echo "   Config: $([ -f config/sys.config ] && echo '✅' || echo '❌') sys.config"
echo "   Build:  $([ -d _build/default ] && echo '✅' || echo '❌') compiled code"
echo ""

# Get local network IP for distributed mode
LOCAL_IP=$(ifconfig | grep inet | grep -v 127.0.0.1 | grep -v ::1 | head -1 | awk '{print $2}')
NODE_NAME="agents@${LOCAL_IP}"

echo "🔧 Starting system with distributed configuration..."
echo "   Web interface will be available at: http://localhost:8080"
echo "   Node name: ${NODE_NAME}"
echo "   Auto-discovery: Enabled for local network"
echo "   Press Ctrl+C to stop the system"
echo ""

# Start the system with distributed mode
exec erl \
  -name "$NODE_NAME" \
  -setcookie agents_cluster_cookie \
  -proto_dist inet_tcp \
  -erl_epmd_port 4369 \
  -kernel inet_dist_listen_min 9100 \
  -kernel inet_dist_listen_max 9200 \
  -pa _build/default/lib/*/ebin \
  -config config/sys \
  -eval "
    io:format('🔧 Initializing applications...~n'),
    
    % Start applications in order
    {ok, _} = application:ensure_all_started(openai),
    {ok, _} = application:ensure_all_started(agents), 
    {ok, _} = application:ensure_all_started(agent_web),
    
    % Auto-discovery function
    AutoDiscover = fun() ->
        timer:sleep(3000),
        io:format('🔍 Scanning for other agent nodes on network...~n'),
        BaseIP = \"192.168.1.\",
        CurrentIP = \"${LOCAL_IP}\",
        Nodes = [list_to_atom(\"agents@\" ++ BaseIP ++ integer_to_list(N)) || 
                 N <- lists:seq(1, 254),
                 BaseIP ++ integer_to_list(N) =/= CurrentIP],
        Connected = [Node || Node <- Nodes, net_adm:ping(Node) =:= pong],
        case Connected of
            [] -> io:format('🌐 No other agent nodes found on network~n');
            _ -> io:format('✅ Connected to nodes: ~p~n', [Connected])
        end
    end,
    
    % Start auto-discovery
    spawn(AutoDiscover),
    
    io:format('✅ Distributed system started successfully!~n'),
    io:format('🌐 Web interface: http://localhost:8080~n'),
    io:format('⚡ Hot reload: Ready for zero-downtime updates~n'),
    io:format('💬 Streaming: Fixed to show readable text~n'),
    io:format('🏗️  Architecture: Modular OTP supervision~n'),
    io:format('🌍 Distribution: Automatic clustering enabled~n'),
    io:format('🔗 Node: ~s~n', [node()]),
    io:format('~n'),
    io:format('🎯 Distributed system is now ready for use!~n'),
    io:format('   • Automatically discovers other agent nodes~n'),
    io:format('   • Shares workload across the cluster~n'),
    io:format('   • Provides fault tolerance~n'),
    io:format('   • Web interface available on all nodes~n'),
    io:format('~n'),
    io:format('Press Ctrl+C to stop the system~n').
" \
  +JMsingle true \
  +MBas aobf \
  +MHas aobf \
  +MMmcs 30 \
  +P 1048576 \
  +Q 1048576 \
  +S 4:4 \
  +stbt db \
  +sbwt very_short \
  +swt very_low \
  +A 32 \
  +MBmmsbc 512 \
  +MHmmsbc 512 \
  +MBlmbcs 512 \
  +MHlmbcs 512 \
  +hmqd off_heap \
  +hmax 67108864 \
  +zdbbl 32768 \
  +c true \
  +K true \
  +Bd \
  +W w \
  -env ERL_CRASH_DUMP crash_dumps/erl_crash.dump \
  -env ERL_CRASH_DUMP_SECONDS 30