# Streaming Function Calls - Mermaid Diagrams

## Overall System Flow

```mermaid
graph TB
    subgraph Client["Client Application"]
        UI[User Interface]
        WS[WebSocket Handler]
        REST[REST Client]
    end
    
    subgraph Agent["Agent Instance"]
        Router[Stream Router]
        Handler[Stream Handler]
        Acc[Accumulator]
    end
    
    subgraph Tools["Tool System"]
        Exec[Tool Executor]
        Para[Parallel Manager]
        Reg[Tool Registry]
    end
    
    subgraph API["OpenAI API"]
        Chat[Chat API]
        Resp[Responses API]
        SSE[SSE Stream]
    end
    
    UI --> WS
    UI --> REST
    WS --> Router
    REST --> Router
    Router --> Handler
    Handler --> Acc
    Handler --> Exec
    Exec --> Para
    Para --> Reg
    Router --> Chat
    Router --> Resp
    Chat --> SSE
    Resp --> SSE
    SSE --> Handler
    
    style Client fill:#1a1a1a,stroke:#00ff88
    style Agent fill:#2a2a2a,stroke:#00ff88
    style Tools fill:#1a2a2a,stroke:#00ff88
    style API fill:#2a1a1a,stroke:#00ff88
```

## Streaming Event Sequence

```mermaid
sequenceDiagram
    participant User
    participant Agent
    participant Handler
    participant OpenAI
    participant Tools
    
    User->>Agent: stream_chat(Message)
    Agent->>Handler: init_accumulator()
    Agent->>OpenAI: POST /chat/completions {stream: true}
    
    loop Streaming
        OpenAI-->>Handler: SSE: content delta
        Handler-->>User: {stream_token, Token}
        Handler->>Handler: accumulate content
    end
    
    OpenAI-->>Handler: SSE: function call start
    Handler-->>User: {stream_function_call_started}
    
    loop Function Arguments
        OpenAI-->>Handler: SSE: arguments delta
        Handler->>Handler: merge deltas
        Handler-->>User: {stream_function_arguments_delta}
    end
    
    OpenAI-->>Handler: SSE: function complete
    Handler-->>User: {stream_function_call_complete}
    
    Handler->>Tools: execute_tool_calls_parallel()
    Tools-->>Handler: Results
    Handler-->>User: {stream_tool_result}
    
    Handler->>OpenAI: Continue stream with results
    
    loop Continue Streaming
        OpenAI-->>Handler: SSE: content delta
        Handler-->>User: {stream_token, Token}
    end
    
    OpenAI-->>Handler: SSE: [DONE]
    Handler-->>User: {stream_complete}
```

## Function Call Accumulation State Machine

```mermaid
stateDiagram-v2
    [*] --> Idle: init_accumulator()
    
    Idle --> Streaming: stream_start
    Streaming --> Streaming: content_delta
    Streaming --> FunctionDetected: function_call_start
    
    FunctionDetected --> AccumulatingArgs: arguments_delta
    AccumulatingArgs --> AccumulatingArgs: more_deltas
    AccumulatingArgs --> FunctionReady: arguments_complete
    
    FunctionReady --> Executing: execute_tools()
    Executing --> ResultsReady: tool_results
    
    ResultsReady --> Streaming: continue_stream
    Streaming --> Complete: stream_done
    
    Complete --> [*]
    
    Streaming --> Error: error_event
    FunctionDetected --> Error: parse_error
    Executing --> Error: tool_error
    Error --> [*]
```

## Parallel Tool Execution Flow

```mermaid
graph TD
    subgraph Detection["Function Call Detection"]
        FC1[Function 1: search]
        FC2[Function 2: calculate]
        FC3[Function 3: read_file]
    end
    
    subgraph Execution["Parallel Execution"]
        P1[Process 1]
        P2[Process 2]
        P3[Process 3]
    end
    
    subgraph Tools["Tool Execution"]
        T1[HTTP API Call<br/>200ms]
        T2[Local Calculation<br/>5ms]
        T3[File I/O<br/>15ms]
    end
    
    subgraph Results["Results Collection"]
        R1[Weather: 72°F]
        R2[Result: $12.75]
        R3[Content: Hello]
    end
    
    FC1 --> P1
    FC2 --> P2
    FC3 --> P3
    
    P1 --> T1
    P2 --> T2
    P3 --> T3
    
    T1 --> R1
    T2 --> R2
    T3 --> R3
    
    R1 --> Collector[Results Collector]
    R2 --> Collector
    R3 --> Collector
    
    Collector --> Continue[Continue Streaming]
    
    style Detection fill:#3a3a3a,stroke:#00ff88
    style Execution fill:#2a3a2a,stroke:#ffaa00
    style Tools fill:#3a2a3a,stroke:#00aaff
    style Results fill:#3a3a2a,stroke:#00ff88
```

## Error Handling Flow

```mermaid
flowchart LR
    subgraph Errors["Error Types"]
        E1[Network Error]
        E2[Timeout Error]
        E3[Parse Error]
        E4[Tool Error]
        E5[API Error]
    end
    
    subgraph Handlers["Error Handlers"]
        H1[Retry Handler]
        H2[Timeout Handler]
        H3[Parse Recovery]
        H4[Tool Fallback]
        H5[API Fallback]
    end
    
    subgraph Recovery["Recovery Actions"]
        R1[Exponential Backoff]
        R2[Partial Response]
        R3[Skip & Continue]
        R4[Default Result]
        R5[Error Message]
    end
    
    E1 --> H1 --> R1
    E2 --> H2 --> R2
    E3 --> H3 --> R3
    E4 --> H4 --> R4
    E5 --> H5 --> R5
    
    R1 --> Final[Final Response]
    R2 --> Final
    R3 --> Final
    R4 --> Final
    R5 --> Final
```

## Data Flow Through Components

```mermaid
graph LR
    subgraph Input["Input Stream"]
        I1[SSE Event 1]
        I2[SSE Event 2]
        I3[SSE Event 3]
    end
    
    subgraph Parser["Event Parser"]
        P1[Decode JSON]
        P2[Extract Type]
        P3[Route Event]
    end
    
    subgraph Accumulator["Accumulator State"]
        A1[Content Buffer]
        A2[Tool Call Map]
        A3[Metadata]
    end
    
    subgraph Output["Output Events"]
        O1[stream_token]
        O2[stream_function_call]
        O3[stream_complete]
    end
    
    I1 --> P1
    I2 --> P1
    I3 --> P1
    
    P1 --> P2
    P2 --> P3
    
    P3 --> A1
    P3 --> A2
    P3 --> A3
    
    A1 --> O1
    A2 --> O2
    A3 --> O3
    
    style Input fill:#1a1a1a,stroke:#888
    style Parser fill:#2a2a2a,stroke:#00ff88
    style Accumulator fill:#3a3a3a,stroke:#00aaff
    style Output fill:#2a2a2a,stroke:#00ff88
```

## WebSocket Integration Example

```mermaid
sequenceDiagram
    participant Browser
    participant WebSocket
    participant Agent
    participant Stream
    
    Browser->>WebSocket: Connect ws://localhost:8080
    WebSocket-->>Browser: Connection established
    
    Browser->>WebSocket: {"action": "stream_chat", "message": "Hello"}
    WebSocket->>Agent: stream_chat(AgentPid, Message)
    Agent->>Stream: init streaming
    
    loop Streaming Events
        Stream-->>Agent: Event
        Agent-->>WebSocket: Format event
        WebSocket-->>Browser: JSON event
        Browser->>Browser: Update UI
    end
    
    Stream-->>Agent: Complete
    Agent-->>WebSocket: Final response
    WebSocket-->>Browser: {"type": "complete", "data": {...}}
```

## Performance Timeline

```mermaid
gantt
    title Streaming Function Call Timeline
    dateFormat X
    axisFormat %L
    
    section Setup
    Connection Setup    :done, setup, 0, 50
    
    section Initial Stream
    First Token        :done, first, 50, 200
    Content Streaming  :done, content, 200, 600
    
    section Function Detection
    Function Detection :done, detect, 600, 650
    Argument Streaming :done, args, 650, 750
    
    section Tool Execution
    Tool 1 (Search)    :active, tool1, 750, 950
    Tool 2 (Calculate) :active, tool2, 750, 800
    Tool 3 (File Read) :active, tool3, 750, 850
    
    section Final Stream
    Result Processing  :done, results, 950, 1000
    Continue Stream    :done, final, 1000, 1200
    Complete          :done, complete, 1200, 1250
```

## Architecture Layers

```mermaid
graph TB
    subgraph Presentation["Presentation Layer"]
        Web[Web UI]
        CLI[CLI Interface]
        API[REST API]
    end
    
    subgraph Application["Application Layer"]
        Agent[Agent Instance]
        Router[Request Router]
        Session[Session Manager]
    end
    
    subgraph Domain["Domain Layer"]
        Stream[Stream Handler]
        Tools[Tool Registry]
        Exec[Execution Engine]
    end
    
    subgraph Infrastructure["Infrastructure Layer"]
        HTTP[HTTP Client]
        WS[WebSocket Server]
        Store[State Store]
    end
    
    Web --> Router
    CLI --> Router
    API --> Router
    
    Router --> Agent
    Agent --> Session
    
    Agent --> Stream
    Stream --> Tools
    Tools --> Exec
    
    Stream --> HTTP
    Router --> WS
    Session --> Store
    
    style Presentation fill:#4a4a4a,stroke:#00ff88
    style Application fill:#3a3a3a,stroke:#00ff88
    style Domain fill:#2a2a2a,stroke:#00ff88
    style Infrastructure fill:#1a1a1a,stroke:#00ff88
```