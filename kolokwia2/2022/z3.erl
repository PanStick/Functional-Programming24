-module(z3).
-export([start/2, process/3]).

process(N, I, Next) ->
        receive
        Msg ->
            io:format("(~p, ~p)\n", [I, Msg]),
            if 
                is_number(Msg) ->
                    Next ! [I, Msg],
                    process(N, I, Next);

                is_list(Msg) ->
                    Sum = lists:sum(Msg),

                    if Sum > N*N ->
                        Next ! lists:nth(1, Msg),
                        process(N, I, Next);
                    true -> 
                        Next ! [lists:nth(1, Msg)*I | Msg],
                        process(N, I, Next)
                end
            end
    end.

startProcesses(N, 1, Next) -> spawn(z3, process, [N, 1, Next]);
startProcesses(N, I, Next) -> startProcesses(N, I-1, spawn(z3, process, [N, I, Next])).

start(N, X) ->
    First = startProcesses(N, N, first),
    register(first, First),
    first ! X.