def transcription:
  {
    "G": "C",
    "C": "G",
    "T": "A",
    "A": "U"
  }
;

def transcribe:
  transcription[.]
;

def toRna:
  split("") |
  map(transcribe) |
  join("")
;
