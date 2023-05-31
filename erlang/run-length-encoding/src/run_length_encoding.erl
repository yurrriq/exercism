-module(run_length_encoding).

-export([decode/1, encode/1]).

decode(String) ->
    decode_iter([], String).

decode_iter(Acc, []) ->
    lists:flatten(lists:reverse(Acc));
decode_iter(Acc, String) ->
    case lists:splitwith(fun(C) -> $0 =< C andalso C =< $9 end, String) of
        {[], [Char | Rest]} ->
            decode_iter([Char | Acc], Rest);
        {[$0 | _], _} ->
            erlang:error(bad_encoding);
        {"1", _} ->
            erlang:error(bad_encoding);
        {CountString, [Char | Rest]} ->
            Count = list_to_integer(CountString),
            decode_iter([lists:duplicate(Count, Char) | Acc], Rest)
    end.

encode([Char | String]) ->
    encode_iter({Char, 1}, [], String);
encode([]) ->
    [].

encode_iter({Char, Count}, Acc, [Char | String]) ->
    encode_iter({Char, Count + 1}, Acc, String);
encode_iter({Char, 1}, Acc, [NextChar | String]) ->
    encode_iter({NextChar, 1}, [Char | Acc], String);
encode_iter({Char, Count}, Acc, [NextChar | String]) ->
    encode_iter({NextChar, 1}, [Char, integer_to_list(Count) | Acc], String);
encode_iter({Char, 1}, Acc, []) ->
    lists:flatten(lists:reverse([Char | Acc]));
encode_iter({Char, Count}, Acc, []) ->
    lists:flatten(lists:reverse([Char, integer_to_list(Count) | Acc])).
