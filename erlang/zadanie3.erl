-module(zadanie3).
-export([start/1, server/0, client/2]).

server() ->
    receive
        X when is_integer(X) -> 
            io:format("~p~n", [X*X]),
            server()
        after 3000 -> io:format("Serwer konczy dzialanie.")
    end.

client(_,[]) -> ok;
client(PID,[X|XS]) ->
    PID ! X,
    client(PID, XS).
    
start(X) -> PID1 = spawn(zadanie3, server, []),
    io:format("Start serwera, PID = ~p~n", [PID1]),
    PID2 = spawn(zadanie3, client, [PID1, X]),
    io:format("Start klienta, PID = ~p, z lista ~p.~n", [PID2, X]). 