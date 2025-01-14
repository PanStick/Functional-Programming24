-module(zadanie5).
-export([startServer/0, startClient/1, server/0, client/1]).

startServer() ->
    register(server, spawn(zadanie5, server, [])).

server() ->
    receive
        X when is_number(X) ->
            io:format("Serwer otrzymal liczbe: ~p.~n", [X]),
            server();
        L when is_list(L) -> 
            io:format("Serwer otrzymal liste: ~w.~n", [L]),
            server();
        koniec -> 
            io:format("Serwer konczy dzialanie~n"),
            ok
    end.

client(X) -> server ! X.

startClient(X) -> 
    spawn(zadanie5, client, [X]).