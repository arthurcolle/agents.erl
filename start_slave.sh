#!/bin/bash

# Usage: ./start_slave.sh [master_node]
# Example: ./start_slave.sh agents@matrix.local

MASTER_NODE=${1:-"agents@matrix.local"}

echo "🤖 Starting Agents.erl SLAVE Node"
echo "=================================="
echo ""

# Get hostname
HOSTNAME=$(hostname)
if [[ "$HOSTNAME" != *.local ]]; then
    HOSTNAME="${HOSTNAME}.local"
fi

echo "📋 Slave Node Configuration:"
echo "   Hostname: $HOSTNAME"
echo "   Node: slave@$HOSTNAME"
echo "   Master: $MASTER_NODE"
echo "   Cookie: agents_cluster_cookie"
echo "   Ports: 9100-9200"
echo ""

# Kill existing processes
pkill -f "beam.*slave" 2>/dev/null

# Ensure directories exist
mkdir -p logs crash_dumps

echo "🚀 Starting slave node and connecting to master..."

exec erl \
  -name "slave@$HOSTNAME" \
  -setcookie agents_cluster_cookie \
  -proto_dist inet_tcp \
  -kernel inet_dist_listen_min 9100 \
  -kernel inet_dist_listen_max 9200 \
  -pa _build/default/lib/*/ebin \
  -eval "
    io:format('🔧 Connecting to master node: $MASTER_NODE~n'),
    case net_adm:ping('$MASTER_NODE') of
        pong ->
            io:format('✅ Connected to master!~n'),
            application:start(sasl),
            application:start(crypto),
            application:start(asn1), 
            application:start(public_key),
            application:start(ssl),
            application:start(xmerl),
            application:start(inets),
            application:start(jsx),
            application:start(jiffy),
            application:start(gproc),
            application:start(agents),
            io:format('~n✅ SLAVE NODE READY: ~p~n', [node()]),
            io:format('🤝 Connected to cluster: ~p~n', [nodes()]),
            io:format('🔄 Sharing workload with master~n');
        pang ->
            io:format('❌ Failed to connect to master: $MASTER_NODE~n'),
            io:format('   Make sure master is running first~n'),
            timer:sleep(3000),
            init:stop()
    end." \
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