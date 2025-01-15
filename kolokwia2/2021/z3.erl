-module(z3).
-export([start/2, process/1]).

process(Next) ->
    receive
        List -> 
            io:format("~p~n", [List]),
            ListLength = length(List),
            if
                ListLength > 3 ->
                    Next ! [lists:nth(1, List) + lists:nth(2, List), lists:nth(1, List)];
                true -> 
                    Next ! [lists:nth(1, List) + lists:nth(2, List) | List]
            end,
            process(Next)
    end.

startProcesses(1, Next) -> spawn(z3, process, [Next]);
startProcesses(N, Next) -> startProcesses(N-1, spawn(z3, process, [Next])).
start(N, K) ->
    First = startProcesses(N, first),
    register(first, First),
    first ! [K, 1].