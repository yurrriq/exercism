-module(isbn_verifier).

-export([is_valid/1]).

%% @doc Determine whether a given ISBN is valid.
-spec is_valid(Isbn :: string()) -> boolean().
is_valid(Isbn) ->
    is_valid(Isbn, 10, 0).

-spec is_valid(
    IsbnPart :: string(), Position :: 0..10, Checksum :: non_neg_integer()
) -> boolean().
is_valid([], 0, Acc) ->
    Acc rem 11 =:= 0;
is_valid([$- | Rest], Position, Acc) ->
    is_valid(Rest, Position, Acc);
is_valid("X", 1, Acc) ->
    is_valid([], 0, Acc + 10);
is_valid([Char | Chars], Position, Acc) when Char >= $0 andalso Char =< $9 ->
    is_valid(Chars, Position - 1, Acc + ((Char - $0) * Position));
is_valid(_, _, _) ->
    false.
