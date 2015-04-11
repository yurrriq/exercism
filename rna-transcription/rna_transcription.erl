-module(rna_transcription).
-export([to_rna/1]).

transcribe(Nucleotide) ->
    case Nucleotide of
	71 -> 67;   
	67 -> 71;
	84 -> 65;
	65 -> 85
    end.

to_rna(Strand) -> lists:map(fun transcribe/1, Strand).
