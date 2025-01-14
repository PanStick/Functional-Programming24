-module(zadanie4).
-export([start/1, server/0, client/2]).

server() ->
    receive
        X when is_integer(X) ->
            client ! X*X,
            server()
        after 3000 ->
            io:format("Serwer zakonczyl dzialanie~n")
    end.

client(_, []) -> ok;
client(PID, [X|XS]) ->
    PID ! X,
    io:format("Klient wyslal ~p.~n", [X]),
    receive
        Y when is_integer(Y) ->
            io:format("Klient dostal ~p.~n", [Y]),
            client(PID, XS)
    end.


start(X) -> PID1 = spawn(zadanie4, server, []),
    io:format("Start serwera, PID = ~p~n", [PID1]),
    PID2 = spawn(zadanie4, client, [PID1, X]),
    register(client, PID2),
    io:format("Start klienta, PID = ~p, z lista ~p.~n", [PID2, X]).