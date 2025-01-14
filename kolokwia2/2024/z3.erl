-module (z3).
-export([start/3, startProcess/5]).

startProcess(M, N, X, Depth, PID) when Depth < N ->
    lists:foreach(fun(I) -> spawn(z3, startProcess, [M, N, (X*M)+I, Depth+1, self()])
                    end, lists:seq(0, M-1)),
    Responses = lists:flatten(receiveResponses(M, [])),
    ReturnList = [X | Responses],
    io:format("~p~n", [ReturnList]),
    if 
        PID /= 0 ->
            PID ! ReturnList;
        true -> io:format("")
    end;    

startProcess(_, _, X, _, PID) ->
    io:format("~p~n", [[X]]),
    PID ! [X].


receiveResponses(0, Acc) -> Acc;
receiveResponses(N, Acc) ->
    receive 
        Msg -> receiveResponses(N-1, [Msg|Acc])
    end.

start(M, N, X) ->
    spawn(z3, startProcess, [M, N, X, 0, 0]).