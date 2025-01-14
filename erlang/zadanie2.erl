-module(zadanie2).
-export([start/0, processA/1, processB/0]).

processA(PID) ->
    PID ! czesc,
    io:format("Proces A wyslal atom czesc.~n").

processB() ->
    receive
        czesc ->
            io:format("Proces B otrzymał atom czesc.~n")
        end.

start() -> PID1 = spawn(zadanie2, processB, []),
    io:format("Start Procesu B, PID  = ~p~n", [PID1]),
    PID2 = spawn(zadanie2, processA, [PID1]),
    io:format("Start Procesu A, PID = ~p~n", [PID2]).