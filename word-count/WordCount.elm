module WordCount exposing (wordCount)

import Dict   exposing (Dict)
import List   exposing (filter, foldl)
import Regex  exposing (HowMany(..), Regex, regex)
import String exposing (isEmpty, toLower)

-- elm package install krisajenkins/elm-exts
import Exts.Maybe exposing (maybe)

{- Count the occurrences of each word in a phrase.

    wordCount "olly olly in come free" ==
      Dict.fromList [("come",1),("free",1),("in",1),("olly",2)]
-}
wordCount : String -> Dict String Int
wordCount = foldl (flip Dict.update maybeSucc) Dict.empty << words' << toLower

{- Break a string into words, splitting on chunks of non-alphanumerics.

    words' "How are \t you? \n Good?" == ["How","are","you","Good"]
    words' "testing, 1, 2 testing"    == ["testing","1","2","testing"]
-}
words' : String -> List String
words' = filter notEmpty << Regex.split All nonAlphaNum

-- A `Regex` that matches groups of non-alphanumeric characters.
nonAlphaNum : Regex
nonAlphaNum = regex "[^\\w\\d]+"

{- Determine if a string is not empty.

    notEmpty ""          == False
    notEmpty "the world" == True
-}
notEmpty : String -> Bool
notEmpty = not << isEmpty

{- Apply `succ` to a `Maybe number`, returning `1` if the value is `Nothing`.

    maybeSucc Nothing  == Just 1
    maybeSucc (Just 2) == Just 3
-}
maybeSucc : Maybe number -> Maybe number
maybeSucc = Just << maybe 1 succ

{- Return the successor of a number, i.e. add one to it.

    succ 1 == 2
    succ 2 == 3
-}
succ : number -> number
succ n = n + 1
