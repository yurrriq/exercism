-module(dna).

-export([count/2, nucleotide_counts/1]).

-define(NUCLEOTIDES, ["A", "T", "C", "G"]).

%% Given a DNA strand and a nucleotide, validate Nucleotide and
%% if valid return the number of occurrences of Nucleotide in Strand.
count(Strand, Nucleotide) ->
  %% No need to bind the result, since an invalid Nucleotide will throw.
  validate(Nucleotide),
  lists:foldl(fun (C, Sum) ->
                  %% None of the tests warrant validating the strand,
                  %% but I've defined it to return a valid nucleotide
                  %% or throw an error, so I figure why not use it.
                  case validate([C]) =:= Nucleotide of
                    true  -> Sum + 1;
                    false -> Sum
                  end
              end,
              0, Strand).

%% Given a DNA strand, return a list of tuples for each nucleotide in
%% ?NUCLEOTIDES of the form {Nucleotide, Count}.
nucleotide_counts(Strand) ->
  lists:map(fun (N) -> {N, count(Strand, N)} end, ?NUCLEOTIDES).

%% Given a nucleotide, return it if valid, otherwise throws an error.
validate(Nucleotide) ->
  case lists:member(Nucleotide, ?NUCLEOTIDES) of
    true  -> Nucleotide;
    false -> erlang:error("Invalid nucleotide")
  end.


%% Local Variables:
%% compile-command: "erlc *.erl; and erl -noshell -eval 'eunit:test(dna_tests, [verbose])' -s init stop"
%% erlang-indent-level: 2
%% End:
