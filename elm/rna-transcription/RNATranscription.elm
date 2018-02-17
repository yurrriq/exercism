module RNATranscription exposing (toRNA)

import Result
import String

{-| Compute the RNA complement of a DNA strand.

    toRNA "CAT" == OK  "GUA"
    toRNA "DOG" == Err 'D'
-}
toRNA : String -> Result Char String
toRNA = String.foldr collect (Ok "")

{-| The accumulator function for `toRNA`.

    collect 'G' (Ok "ACAU") == Ok "CACAU"
    collect 'X' (Ok "AAA")  == Err 'X'
-}
collect : Char -> Result Char String -> Result Char String
collect = Result.map2 String.cons << transcribe

{-| Transcribe an individual DNA nucleotide to its RNA complement.

    transcribe 'A' == Ok  'U'
    transcribe 'X' == Err 'X'
-}
transcribe : Char -> Result Char Char
transcribe nucleotide =
  case nucleotide of
    'G' -> Ok  'C'
    'C' -> Ok  'G'
    'T' -> Ok  'A'
    'A' -> Ok  'U'
    _   -> Err nucleotide
