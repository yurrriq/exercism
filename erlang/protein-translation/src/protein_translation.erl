-module(protein_translation).

-export([proteins/1]).

-define(AMINO_ACIDS, [
    {"AUG", methionine},
    {"UGG", tryptophan},
    {"UUU", phenylalanine},
    {"UUC", phenylalanine},
    {"UUA", leucine},
    {"UUG", leucine},
    {"UCU", serine},
    {"UCC", serine},
    {"UCA", serine},
    {"UCG", serine},
    {"UAU", tyrosine},
    {"UAC", tyrosine},
    {"UGU", cysteine},
    {"UGC", cysteine},
    {"UAA", stop},
    {"UAG", stop},
    {"UGA", stop}
]).

proteins(Strand) ->
    do_proteins(Strand, []).

do_proteins([X, Y, Z | Tail], Acc) ->
    case proplists:get_value([X, Y, Z], ?AMINO_ACIDS) of
        stop -> do_proteins([], Acc);
        undefined -> {error, badarg};
        Protein -> do_proteins(Tail, [Protein | Acc])
    end;
do_proteins([], Acc) ->
    {ok, lists:reverse(Acc)};
do_proteins(_, _) ->
    {error, badarg}.
