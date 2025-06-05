# Comprehensive Backend ↔ Frontend Mapping

## Executive Summary
This document provides a complete mapping between all backend endpoints and frontend functionality, identifying working connections, broken connections, and missing implementations.

## 🎯 Critical Issues Identified

### 1. Route Registration Gap
- **Problem**: Current `agent_web_sup.erl` has minimal routing
- **Impact**: 90+ sophisticated handlers exist but are not registered
- **Solution**: Complete route registration needed

### 2. HTTP Method Mismatches
- **Problem**: Frontend expects GET, backend implements POST (and vice versa)
- **Impact**: API calls return 405 Method Not Allowed
- **Examples**: `/api/keys/check`, several MCP endpoints

### 3. Missing Response Standardization
- **Problem**: Inconsistent response formats across handlers
- **Impact**: Frontend expects uniform JSON structures
- **Solution**: Standardize all responses to use `api_response(T)` type

## 📊 Complete Endpoint Mapping

### ✅ WORKING CONNECTIONS

#### Agent Management Core
| Frontend Call | Backend Handler | Status | Notes |
|---------------|----------------|--------|--------|
| `GET /api/agents` | `agent_api_handler:list_agents/2` | ✅ Working | Pagination supported |
| `POST /api/agents` | `agent_api_handler:create_agent/2` | ✅ Working | Full CRUD |
| `DELETE /api/agents/{id}` | `agent_api_handler:delete_agent/3` | ✅ Working | ID validation |
| `GET /api/agents/{id}` | `agent_api_handler:get_agent/3` | ✅ Working | Metadata included |
| `POST /api/agents/{id}/chat` | `agent_chat_handler:init/2` | ✅ Working | Streaming support |

#### WebSocket Real-time
| Frontend Message | Backend Handler | Status | Notes |
|------------------|----------------|--------|--------|
| `create_stream` | `agent_ws_handler:websocket_handle/2` | ✅ Working | Agent subscription |
| `stream_chat` | `agent_ws_handler:websocket_handle/2` | ✅ Working | Live chat streaming |
| `get_system_metrics` | `agent_ws_handler:websocket_handle/2` | ✅ Working | Real-time metrics |
| `subscribe_monitoring` | `agent_ws_handler:websocket_handle/2` | ✅ Working | Agent monitoring |

#### System Health
| Frontend Call | Backend Handler | Status | Notes |
|---------------|----------------|--------|--------|
| `GET /api/system/health` | `system_health_handler:init/2` | ✅ Working | Basic health check |
| WebSocket metrics | `agent_ws_handler:send_system_metrics` | ✅ Working | Auto-updating |

---

### ❌ BROKEN CONNECTIONS

#### API Key Management
| Frontend Call | Backend Implementation | Issue | Fix Required |
|---------------|----------------------|-------|---------------|
| `GET /api/keys/check` | `POST /api/keys/check` | HTTP method mismatch | Change backend to GET |
| `GET /api/keys` | `api_keys_handler:init/2` | Route not registered | Add to routing table |
| `GET /api/keys/{service}` | `api_keys_handler:init/2` | Route not registered | Add to routing table |

#### MCP Management
| Frontend Call | Backend Handler | Issue | Fix Required |
|---------------|----------------|-------|---------------|
| `GET /api/mcp/local/servers` | `mcp_management_handler` | Route not registered | Add comprehensive MCP routes |
| `POST /api/mcp-servers` | `mcp_api_handler` | Different endpoint pattern | Standardize endpoints |
| `POST /api/mcp-servers/{id}/discover` | Multiple handlers | Conflicting implementations | Consolidate handlers |

#### Conversation Management
| Frontend Call | Backend Handler | Issue | Fix Required |
|---------------|----------------|-------|---------------|
| `GET /api/conversations` | `conversation_handler:init/2` | Basic implementation only | Enhance with full CRUD |
| `POST /api/conversations` | Missing implementation | No POST handler | Implement conversation creation |
| `DELETE /api/conversations` | Missing implementation | No DELETE handler | Implement conversation deletion |

#### Model Management
| Frontend Call | Backend Handler | Issue | Fix Required |
|---------------|----------------|-------|---------------|
| `GET /api/models/definitions` | `model_api_handler:init/2` | Route not registered | Add to routing table |
| `POST /api/models/definitions` | `model_api_handler:init/2` | Route not registered | Add to routing table |

---

### 🚧 MISSING IMPLEMENTATIONS

#### Fleet Management (Frontend expects, Backend partial)
| Frontend Component | Expected Endpoint | Backend Status | Notes |
|-------------------|------------------|----------------|--------|
| `FleetManagementDashboard.tsx` | `GET /api/fleet/status` | `fleet_management_handler` exists | Route not registered |
| Fleet metrics | `GET /api/fleet/metrics` | Handler exists | Route not registered |
| Fleet operations | `POST /api/fleet/*` | Multiple handlers exist | Routes not registered |

#### Crash Monitoring (Frontend expects, Backend exists)
| Frontend Component | Expected Endpoint | Backend Handler | Status |
|-------------------|------------------|----------------|--------|
| `CrashMonitorDashboard.tsx` | `GET /api/crashes` | `crash_report_api_handler` | Route not registered |
| Crash analysis | `GET /api/crashes/{id}/analysis` | Handler exists | Route not registered |
| Fix suggestions | `GET /api/crashes/{id}/fixes` | Handler exists | Route not registered |

#### Advanced Features (Backend rich, Frontend basic)
| Backend Capability | Frontend Component | Connection Status | Notes |
|-------------------|-------------------|------------------|--------|
| Quantum coordination | Missing | No frontend | Advanced backend feature |
| Self-healing | Missing | No frontend | Auto-recovery system |
| Workflow orchestration | Missing | No frontend | Complex workflow engine |
| Bulk operations | Missing | No frontend | Batch processing |

---

## 🔧 Data Structure Compatibility

### ✅ Compatible Structures

#### Agent Data
**Frontend TypeScript:**
```typescript
interface Agent {
  id: string;
  name?: string;
  type: string;
  status: string;
  model?: string;
}
```

**Backend Erlang:**
```erlang
#{
    id => binary(),
    name => binary(),
    type => simple | ai | template,
    status => active | idle | error,
    model => binary()
}
```
**Status: ✅ Compatible**

#### WebSocket Messages
**Frontend:**
```typescript
{ type: "create_stream", agent_id: string }
{ type: "stream_chat", message: string }
```

**Backend:**
```erlang
#{<<"type">> := <<"create_stream">>, <<"agent_id">> := AgentId}
#{<<"type">> := <<"stream_chat">>, <<"message">> := Message}
```
**Status: ✅ Compatible**

### ❌ Incompatible Structures

#### API Key Responses
**Frontend expects:**
```typescript
{
  requirements: Record<string, ApiRequirement>,
  missing: ApiKeyStatus[]
}
```

**Backend returns:**
```erlang
#{success => boolean(), data => term()}
```
**Status: ❌ Incompatible - Response format standardization needed**

#### Error Responses
**Frontend expects:**
```typescript
{ error: string, details?: any }
```

**Backend varies:**
```erlang
% Different handlers return different formats
#{error => binary()}  % Some handlers
#{success => false, error => binary()}  % Other handlers
```
**Status: ❌ Inconsistent - Standardization needed**

---

## 🛠️ Comprehensive Fix Plan

### Phase 1: Route Registration (Critical)
1. **Update `agent_web_sup.erl`** to register all existing handlers:
   ```erlang
   % Add 90+ missing routes
   {"/api/keys/[...]", api_keys_handler, []},
   {"/api/mcp/[...]", mcp_api_handler, []},
   {"/api/fleet/[...]", fleet_management_handler, []},
   {"/api/crashes/[...]", crash_report_api_handler, []},
   % ... all other routes
   ```

### Phase 2: HTTP Method Standardization
1. **Fix method mismatches:**
   - Change `/api/keys/check` from POST to GET
   - Standardize MCP endpoints
   - Review all REST endpoints for proper HTTP semantics

### Phase 3: Response Format Standardization
1. **Implement uniform response format:**
   ```erlang
   -type api_response(T) :: #{
       success := boolean(),
       data => T,
       error => binary(),
       timestamp := integer()
   }.
   ```

### Phase 4: Frontend Enhancement
1. **Add missing frontend components:**
   - Fleet management UI
   - Crash monitoring dashboard
   - Advanced workflow interface
   - Bulk operations panel

### Phase 5: Advanced Feature Integration
1. **Connect advanced backend features:**
   - Quantum coordination UI
   - Self-healing monitoring
   - Workflow orchestration interface
   - Performance analytics dashboard

---

## 📈 Implementation Priority

### High Priority (Immediate)
1. Route registration for existing handlers
2. HTTP method fixes for broken endpoints
3. Response format standardization
4. API key management fixes

### Medium Priority (Short-term)
1. Conversation management completion
2. Fleet management UI connection
3. Crash monitoring integration
4. Model management enhancement

### Low Priority (Long-term)
1. Advanced feature frontend development
2. Quantum coordination UI
3. Workflow orchestration interface
4. Performance optimization dashboards

---

## 🎯 Success Metrics

### Technical Metrics
- [ ] 100% of existing handlers have registered routes
- [ ] 0 HTTP method mismatches
- [ ] Uniform response format across all endpoints
- [ ] < 5% failed frontend API calls

### User Experience Metrics
- [ ] All UI components functional
- [ ] Real-time updates working
- [ ] Error messages informative
- [ ] No broken links or 404s

### System Health Metrics
- [ ] All advanced backend features accessible
- [ ] Monitoring and alerting functional
- [ ] Performance metrics visible
- [ ] Auto-healing systems operational

---

## 📋 Verification Checklist

### Backend Verification
- [ ] All handlers in `/src/` have corresponding routes
- [ ] HTTP methods match frontend expectations
- [ ] Response formats follow `api_response(T)` spec
- [ ] Error handling is consistent

### Frontend Verification
- [ ] All API calls succeed (no 404/405 errors)
- [ ] WebSocket connections stable
- [ ] Real-time updates working
- [ ] Error states handled gracefully

### Integration Verification
- [ ] End-to-end user flows working
- [ ] Agent creation → chat → monitoring cycle
- [ ] MCP server connection → tool usage
- [ ] Error occurrence → detection → resolution

This comprehensive mapping reveals that the backend is extremely sophisticated with 120+ handlers and advanced features, but the routing configuration is minimal, causing most frontend calls to fail. The core issue is infrastructure (routing) rather than missing functionality.