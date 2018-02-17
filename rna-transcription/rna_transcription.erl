-module(rna_transcription).
-export([to_rna/1]).

transcribe(Nucleotide) ->
    case Nucleotide of
	$G -> $C;
	$C -> $G;
	$T -> $A;
	$A -> $U
    end.

to_rna(Strand) -> lists:map(fun transcribe/1, Strand).
