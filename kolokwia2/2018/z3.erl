-module(z3).
-export([start/2, process/1]).

process(Next) ->
    receive
        Val -> 
            io:format("~w~n", [Val]),
            Next ! [lists:sum(Val) | Val],
            process(Next)
    end.

startProcess(1, Next) -> spawn (z3, process, [Next]);
startProcess(N, Next) -> startProcess(N-1, spawn(z3, process, [Next])).

start(N, K) ->
    First = startProcess(N, first),
    register (first, First),
    first ! [K].