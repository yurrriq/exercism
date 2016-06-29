module Bob exposing (hey)

import Char   exposing (isLower, isUpper)
import String exposing (any, endsWith, isEmpty, trim)

{-| Get a lackadaisical teenager's response.

    hey "What's up?"               == "Sure".
    hey "That doesn't make sense." == "Whatever."
-}
hey : String -> String
hey prompt =
  if      isYelled   prompt  then "Whoa, chill out!"
  else if isSilent   prompt  then "Fine. Be that way!"
  else if isQuestion prompt  then "Sure."
  else                            "Whatever."

{-| Determine if a string is empty or contains only whitespace.

    isSilent "  \t  " == True
    isSilent "Hill"   == False
    isSilent ""       == True
-}
isSilent : String -> Bool
isSilent = isEmpty << trim

{-| Determine if a string ends with a question mark.

    isQuestion "Oh?" == True
    isQuestion "No." == False
-}
isQuestion : String -> Bool
isQuestion = endsWith "?"

{-| Determine whether every letter is uppercase.

    isYelled "HEY!"    == True
    isYelled "H3Y."    == True
    isYelled "VÄRFÖR?" == True
-}
isYelled : String -> Bool
isYelled str = any isUpper str && notany isLower str

{-| Determine if *no* character satisfies a predicate.

    notany Char.isUpper "example" == True
    notany Char.isUpper "LIES"    == False
-}
notany : (Char -> Bool) -> String -> Bool
notany pred = not << any pred
