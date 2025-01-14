-module(split).
-export([split/1]).

split (L) -> {lists:filter(fun (X) -> is_integer (X) end, L), lists:filter(fun (X) -> not is_integer(X) end, L)}.