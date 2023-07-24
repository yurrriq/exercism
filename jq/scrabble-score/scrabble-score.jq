def scores:
  {
    "AEIOULNRST": 1,
    "DG": 2,
    "BCMP": 3,
    "FHVWY": 4,
    "K": 5,
    "JX": 8,
    "QZ": 10
  } |
  with_entries({"key": (.key / "")[], value})
;

.word |
ascii_upcase |
scores as $scores |
reduce split("")[] as $letter (0; . + $scores[$letter])
