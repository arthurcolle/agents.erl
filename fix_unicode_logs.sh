#!/bin/bash

# Quick fix to remove Unicode emojis from log messages that cause badarg errors

echo "🔧 Fixing Unicode issues in log messages..."

cd "$(dirname "$0")"

# Remove specific problematic emojis from meta_supervision_manager.erl
sed -i.bak 's/🔧 Starting Meta Supervision Manager/Starting Meta Supervision Manager/g' apps/agent_web/src/meta_supervision_manager.erl

# Remove other problematic emojis from key startup files
sed -i.bak 's/✅/✓/g; s/❌/✗/g; s/⚠️/WARNING/g; s/🔧/[TOOL]/g' apps/agent_web/src/meta_supervision_manager.erl
sed -i.bak 's/✅/✓/g; s/❌/✗/g; s/⚠️/WARNING/g; s/🔧/[TOOL]/g' apps/agent_web/src/auto_healing_coordinator.erl
sed -i.bak 's/📊/[STATS]/g' apps/agent_web/src/comprehensive_error_logger.erl

echo "✓ Unicode fixes applied"
echo "✓ System should start without badarg errors now"