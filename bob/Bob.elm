module Bob exposing (hey)

import Char   as C
import String as S

hey : String -> String
hey s =
  let
    isYelled : Bool
    isYelled = (S.any C.isUpper s) && (s == S.toUpper s)

    isSpace : Char -> Bool
    isSpace c =
      case c of
        ' '  -> True
        '\t' -> True
        '\n' -> True
        '\r' -> True
        '\f' -> True
        '\v' -> True
        _    -> False

    isSilent : Bool
    isSilent = S.all isSpace s

    isQuestion : Bool
    isQuestion = "?" == S.right 1 s
  in
    if      isYelled   then "Whoa, chill out!"
    else if isSilent   then "Fine. Be that way!"
    else if isQuestion then "Sure."
    else                    "Whatever."
