#!/bin/bash

echo "🎯 Starting Agents.erl MASTER Node"
echo "=================================="
echo ""

# Get hostname
HOSTNAME=$(hostname)
if [[ "$HOSTNAME" != *.local ]]; then
    HOSTNAME="${HOSTNAME}.local"
fi

echo "📋 Master Node Configuration:"
echo "   Hostname: $HOSTNAME"
echo "   Node: agents@$HOSTNAME"
echo "   Cookie: agents_cluster_cookie"
echo "   Ports: 9100-9200"
echo "   Web UI: http://localhost:8080"
echo ""

# Kill existing processes
pkill -f "beam.*agents" 2>/dev/null

# Ensure directories exist
mkdir -p logs crash_dumps

echo "🚀 Starting master node..."

exec erl \
  -name "agents@$HOSTNAME" \
  -setcookie agents_cluster_cookie \
  -proto_dist inet_tcp \
  -kernel inet_dist_listen_min 9100 \
  -kernel inet_dist_listen_max 9200 \
  -pa _build/default/lib/*/ebin \
  -eval "
    io:format('🔧 Starting applications...~n'),
    application:start(sasl),
    application:start(crypto), 
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(xmerl),
    application:start(inets),
    application:start(jsx),
    application:start(jiffy),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    application:start(uuid),
    application:start(hackney),
    application:start(gproc),
    application:start(openai),
    application:start(agents),
    application:start(agent_web),
    io:format('~n✅ MASTER NODE READY: ~p~n', [node()]),
    io:format('🌐 Web interface: http://localhost:8080~n'),
    io:format('🔗 Waiting for slave connections...~n'),
    io:format('~n📋 For slaves to connect:~n'),
    io:format('   ./start_slave.sh agents@$HOSTNAME~n'),
    io:format('~n')." \
  +JMsingle true \
  +MBas aobf \
  +MHas aobf \
  +MMmcs 30 \
  +P 1048576 \
  +Q 1048576 \
  +S 4:4 \
  +A 32 \
  +K true \
  +c true