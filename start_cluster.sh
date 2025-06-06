#!/bin/bash

# Start Erlang cluster with hot-swapping capabilities
# Usage: ./start_cluster.sh [node_name]

NODE_NAME=${1:-agents}
# Get local IP address for distributed mode
LOCAL_IP=$(ifconfig | grep inet | grep -v 127.0.0.1 | grep -v ::1 | head -1 | awk '{print $2}')
FULL_NODE_NAME="${NODE_NAME}@${LOCAL_IP}"

echo "Starting Erlang cluster node: $FULL_NODE_NAME"
echo "Cookie: agents_cluster_cookie"

# Ensure directories exist
mkdir -p logs crash_dumps

# Set environment variables for clustering
export ERL_EPMD_PORT=4369
export ERLANG_COOKIE=agents_cluster_cookie

# Start with clustering enabled
erl \
  -name "$FULL_NODE_NAME" \
  -setcookie agents_cluster_cookie \
  -proto_dist inet_tcp \
  -erl_epmd_port 4369 \
  -pa _build/default/lib/*/ebin \
  -config config/sys \
  -s openai_app \
  -s agents \
  -s agent_web \
  -eval "io:format('~n=== Cluster node ~s started ===~n', ['$FULL_NODE_NAME'])" \
  -eval "io:format('Use net_adm:ping('\''other_node@host'\'') to connect to other nodes~n')" \
  -eval "spawn(fun() -> timer:sleep(5000), io:format('🔍 Scanning for other nodes on network...~n'), [net_adm:ping(list_to_atom(\"agents@\" ++ IP)) || IP <- [\"192.168.1.\" ++ integer_to_list(N) || N <- lists:seq(1, 254)], IP =/= \"$LOCAL_IP\"] end)" \
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