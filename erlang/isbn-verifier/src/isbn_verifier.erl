-module(isbn_verifier).

-export([is_valid/1]).

%% @doc Determine whether a given ISBN is valid.
-spec is_valid(string()) -> boolean().
is_valid(Isbn) ->
    case checksum(Isbn, 10, 0) of
        error ->
            false;
        {ok, Checksum} ->
            Checksum rem 11 =:= 0
    end.

-spec checksum(string(), 0..10, non_neg_integer()) -> error | {ok, non_neg_integer()}.
checksum([], 0, Acc) ->
    {ok, Acc};
checksum([$- | Rest], Position, Acc) ->
    checksum(Rest, Position, Acc);
checksum("X", 1, Acc) ->
    {ok, Acc + 10};
checksum([Char | Chars], Position, Acc) ->
    case string:list_to_integer([Char]) of
        {error, no_integer} ->
            error;
        {Digit, _} ->
            checksum(Chars, Position - 1, Acc + (Digit * Position))
    end;
checksum(_, _, _) ->
    error.
