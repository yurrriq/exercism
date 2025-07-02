:- module(rna_transcription, [rna_transcription/2]).  /*

\PrologDialect{swiprolog}

\Predicate rna_complement/2(+Dna:char, -Rna:char).

True if \verb|Rna| is the RNA complement of the DNA nucleotide \verb|Dna|.

\PL*/
:- semidet(rna_complement/2).

%! rna_complement(+Dna:char, -Rna:char) is semidet.
%
% True if =Rna= is the RNA complement of the DNA nucleotide =Dna=.
rna_complement('G', 'C').
rna_complement('C', 'G').
rna_complement('T', 'A').
rna_complement('A', 'U').
/*PL

\PL*/
:- semidet(rna_transcription/2).
/*PL

\Predicate rna_transcription/2(+Dna:string, -Rna:string).

True if \verb|Rna| is the RNA complement of the DNA sequence \verb|Dna|.

\PL*/
%! rna_transcription(+Dna:string, -Rna:string) is semidet.
%
% True if =Rna= is the RNA complement of the DNA sequence =Dna=.
%
% @see rna_complement/2

rna_transcription(Dna, Rna) :-
    string_chars(Dna, Strand),
    maplist(rna_complement, Strand, Transcribed),
    string_chars(Rna, Transcribed).
/*PL

\EndProlog*/
