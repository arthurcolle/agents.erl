# Self-Scaffolding System - Known Issues

## Issues that need to be fixed for the system to work:

### 1. **SQLite3 Integration**
The Erlang sqlite3 library has a different API than what I used:
- Need to use `esqlite3` instead of `sqlite3` 
- API calls like `sqlite3:open/1` should be `esqlite:open/1`
- Need proper connection management

### 2. **YAML Parsing**
The `yamerl` library returns different data structures:
- Need to properly handle the nested tuple format `{str, Value}`, `{int, Value}`, etc.
- The `yaml_to_map/1` function needs refinement

### 3. **Missing HTTP Client Setup**
The `openapi_fetcher` uses `httpc:request/4` which requires:
- `inets` application to be started
- Proper SSL configuration for HTTPS requests

### 4. **ETS Table Creation**
The `endpoint_registry` creates ETS tables in `init/1` but:
- Need to handle table already exists errors
- Should use `ets:new/2` with proper options

### 5. **Missing Error Handling**
Several places lack proper error handling:
- Network failures in `openapi_fetcher`
- Database connection failures
- JSON/YAML parsing errors

### 6. **Integration Issues**
- The `agent_api_tools` module is not integrated into the agent tools system
- Need to add it to the agent's available tools list
- The tool execution framework needs to recognize these new tools

## Quick Fixes Needed:

```erlang
% 1. Fix SQLite3 usage - replace this pattern:
{ok, Db} = sqlite3:open(DbPath),
sqlite3:exec(Db, Query),
sqlite3:close(Db).

% With:
{ok, Db} = esqlite3:open(DbPath),
{ok, Statement} = esqlite3:prepare(Query, Db),
ok = esqlite3:exec(Statement, Db),
ok = esqlite3:close(Db).

% 2. Start inets for HTTP client:
application:ensure_started(inets),
application:ensure_started(ssl).

% 3. Fix ETS table creation:
case ets:info(endpoints_table) of
    undefined -> ets:new(endpoints_table, [named_table, set, public]);
    _ -> endpoints_table
end.
```

## To properly test:

1. Start with a simple module first:
```erlang
% Test just the supervisor
application:ensure_all_started(agent_web).
supervisor:which_children(self_scaffold_sup).
```

2. Test individual components:
```erlang
% Test endpoint registry alone
endpoint_registry:start_link().
endpoint_registry:register_endpoint(#{
    method => <<"GET">>,
    path => <<"/test">>,
    tag => <<"test">>
}).
```

3. Mock the OpenAPI fetch first before hitting real endpoints

## Honest Assessment:

While the architecture is sound and follows OTP principles, it needs debugging and testing to actually work. The main issues are:
- Library API mismatches (SQLite3, YAML)
- Missing application dependencies (inets)
- Integration gaps with the existing system

The hourly update mechanism via `scaffold_scheduler` should work once these issues are fixed.