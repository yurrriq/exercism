-module(luhn).

-compile(export_all).

create(S) when is_list(S)    -> create(to_integer(S));
create(X) when is_integer(X) ->
  TenX     = X * 10,
  Checksum = rem10(checksum(TenX)),
  Result   = case Checksum =:= 0 of
               true  -> TenX;
               false -> TenX + 10 - Checksum
             end,
  string:join(partition_all(4, integer_to_list(Result)), " ").


valid(S) when is_list(S)    -> valid(to_integer(S));
valid(X) when is_integer(X) -> 0 =:= rem10(checksum(X)).

rem10(X) -> X rem 10.

checksum(X) -> rem10(lists:sum(digits(X))).

digits(X) ->
  Reversed   = lists:reverse(integer_to_list(X)),
  Integers   = lists:map(fun (C) -> list_to_integer([C]) end, Reversed),
  Length     = length(Integers),
  OneTwos    = lists:sublist(lists:flatten(lists:duplicate(round(Length / 2), [1,2])), Length),
  Multiplied = lists:zipwith(fun (Y,Z) -> Y * Z end, OneTwos, Integers),
  lists:map(fun sum_digits/1, Multiplied).

sum_digits(X) -> (X div 10) + (X rem 10).

to_integer(S) when is_list(S) -> list_to_integer(filter_spaces(S)).

filter_spaces(S) when is_list(S) -> lists:filter(fun not_space/1, S).

not_space($\ ) -> false;
not_space(_)   -> true.

%% Like Clojure's `partition-all', but Erlang-ier.
partition_all(0, L)  -> L;
partition_all(_, []) -> [];
partition_all(N, L) when N > length(L) ->
  partition_all(length(L), L);
partition_all(N, L)  ->
  [lists:sublist(L, N) | partition_all(N, lists:nthtail(N, L))].
