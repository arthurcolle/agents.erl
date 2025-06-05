-module(dq).
-export([start_link/1, stop/1, enqueue/2]).

start_link(Options) ->
    missing_function_stubs:dq_start_link(Options).

stop(Pid) ->
    missing_function_stubs:dq_stop(Pid).

enqueue(Queue, Item) ->
    missing_function_stubs:dq_enqueue(Queue, Item).