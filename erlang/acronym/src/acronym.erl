-module(acronym).

-export([abbreviate/1]).

abbreviate(Phrase) ->
    lists:flatmap(fun do_abbreviate/1, string:lexemes(Phrase, " _-")).

do_abbreviate(Word) ->
    case string:next_grapheme(Word) of
        [Letter | _] -> string:uppercase([Letter]);
        _ -> []
    end.
