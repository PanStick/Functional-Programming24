-module(sublists).
-export([sublists/2]).

all_sublists([]) -> [[]];
all_sublists([H | T]) -> Podxs = all_sublists(T), Podxs ++ [ [H | Sub] || Sub <- Podxs ].

has_length(Len, List) -> erlang:length(List) =:= Len.

sublists(Len, List) ->
  Sublists = all_sublists(List),
  lists:filter(fun(Sub) -> has_length(Len, Sub) end, Sublists).