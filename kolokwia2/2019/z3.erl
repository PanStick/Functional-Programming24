-module(z3).
-export[(start/1), process1/0, process2/0, process3/0].

process1() ->
    receive
        Val -> 
            io:format("~p~n", [Val]),
            process2 ! (Val+1)
    end.

process2() ->
    receive
        Val -> 
            io:format("~p~n", [Val]),
            process3 ! (Val*2)
    end.
process3() ->
    receive
        Val -> 
            io:format("~p~n", [Val]),
            process1 ! (Val-3)
    end.

start(N) ->
    First = spawn(z3, process1, []),
    register(process1, First),
    Second = spawn(z3, process2, []),
    register(process2, Second),
    Third = spawn(z3, process3, []),
    register(process3, Third),
    process1 ! N.