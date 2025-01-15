-module(z3).
-export([start/2, process/2]).

process(N, Next) ->
    receive
        Val ->
            io:format("(~p, ~p)~n", [N, Val]),
            if
                N rem 2 == 0 ->
                   Next ! Val*2;
                true -> 
                    Next ! Val-1
            end,
            process(N, Next)
    end.

startProcess(1, Next) -> spawn(z3, process, [1, Next]);
startProcess(N, Next) -> startProcess(N-1, spawn(z3, process, [N, Next])).

start(N, X) ->
    First = startProcess(N, first),
    register(first, First),
    first ! X.