-module(zadanie1).
-export([start/1, hello/2]).

hello(Msg,1) -> io:format("Proces ~p~n",[Msg]);
hello(Msg,N) -> io:format("Proces ~p~n",[Msg]),
hello(Msg,N-1).

start(N) -> spawn_processes(N, 1).

spawn_processes(0, _) -> ok;
spawn_processes(N, I) ->
    spawn(zadanie1, hello, [I, 5]),
    spawn_processes(N-1, I+1).