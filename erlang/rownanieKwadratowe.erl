-module(rownanieKwadratowe).
-export([rownanieKwadratowe/3]).

rownanieKwadratowe(A, B, C) ->
    D = B * B - 4 * A * C,
    if D < 0 ->
           brakRozwiazan;
       D == 0 ->
           -B / (2 * A);
       true ->
           {(-B - math:sqrt(D)) / (2 * A), (-B + math:sqrt(D)) / (2 * A)}
    end.