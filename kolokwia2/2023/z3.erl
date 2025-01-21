-module(z3).
-export([start/2, startProcess/4]).

receiveResponses(0, Vals) -> Vals;
receiveResponses(Reps, Vals) ->
    receive
        Val -> receiveResponses(Reps-1, [Val|Vals])
    end.

startProcess(N, X, Depth, PID) when Depth < N ->
    spawn(z3, startProcess, [N, 2*X+1, Depth+1, self()]),
    spawn(z3, startProcess, [N, 2*X, Depth+1, self()]),
    ReturnList = [X | lists:flatten(receiveResponses(2, []))],
    io:format("~p~n", [ReturnList]),
    if 
        Depth /= 0 ->
            PID ! ReturnList;
        true -> io:format("")
    end;

startProcess(_, X, _, PID) -> io:format("~p~n", [[X]]),
    PID ! X.

start(N, X) -> startProcess(N, X, 0, 0).