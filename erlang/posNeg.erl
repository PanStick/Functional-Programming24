-module(posNeg).
-export([posNeg/1]).

posNeg(L) ->
    posNeg(L, 0, 0).
 
posNeg([], P, N) ->
    {P, N};
posNeg([A | REST], P, N) when A > 0 ->
    posNeg(REST, P + 1, N);
posNeg([A | REST], P, N) when A < 0 ->
    posNeg(REST, P, N + 1);
posNeg([_ | REST], P, N) ->
    posNeg(REST, P, N).