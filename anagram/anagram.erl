-module(anagram).
-export([find/2]).

find(Word, Candidates) ->
  Normed = normalize(Word),
  lists:filter(fun (Candidate) -> is_anagram(Normed, normalize(Candidate)) end,
               Candidates).

is_anagram({LA, SA}, {LB, SB}) when LA =/= LB andalso SA =:= SB -> true;
is_anagram(_, _) -> false.

normalize(Word) ->
  Lowered = string:to_lower(Word),
  {Lowered, lists:sort(Lowered)}.
