nucleotide_transcription('G', 'C').
nucleotide_transcription('C', 'G').
nucleotide_transcription('T', 'A').
nucleotide_transcription('A', 'U').

rna_transcription(Dna, Rna) :-
    string_chars(Dna, Strand),
    maplist(nucleotide_transcription, Strand, Transcribed),
    string_chars(Rna, Transcribed).
