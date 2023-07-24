def transcription:
  {
    "G": "C",
    "C": "G",
    "T": "A",
    "A": "U"
  }
;

def toRna:
  gsub("(?<nucleotide>[GCTA])"; transcription[.nucleotide])
;
