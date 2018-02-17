module Anagram exposing (detect)

import List   exposing (filter, sort)
import String exposing (toList, toLower)

{- Find anagrams for a word in a list of candidates.

    detect "master" ["stream","pigeon","maters"] == ["stream","maters"]
-}
detect : String -> List String -> List String
detect = filterCaseInsensitiveAnagramsFor << toLower

{- Filter a list of candidates for anagrams of a string.

    filterCaseInsensitiveAnagramsFor "ant" ["ant","tan","Tan","stand","at"]
      == ["tan"]
-}
filterCaseInsensitiveAnagramsFor : String -> List String -> List String
filterCaseInsensitiveAnagramsFor =
  filter << (flip (isCaseSensitiveAnagramOf << toLower))

{- Determine if two strings ar note equal but have the same letters.

    "allergy" `isCaseSensitiveAnagramOf` "gallery" == True
    "Allergy" `isCaseSensitiveAnagramOf` "Gallery" == False
    "self"    `isCaseSensitiveAnagramOf` "self"    == False
    "mass"    `isCaseSensitiveAnagramOf` "last"    == False
-}
isCaseSensitiveAnagramOf : String -> String -> Bool
isCaseSensitiveAnagramOf x y = x /= y && x `hasSameLettersAs` y

{- Determine if two strings have the same letters, case sensitive.

    "allergy" `hasSameLettersAs` "gallery" == True
    "Allergy" `hasSameLettersAs` "Gallery" == False
    "self"    `hasSameLettersAs` "self"    == True
    "mass"    `hasSameLettersAs` "last"    == False
-}
hasSameLettersAs : String -> String -> Bool
hasSameLettersAs = (==) `on` (sort << toList)

{- (*) `on` f = \x y -> f x * f y.

    sortBy (compare `on` fst)
-}
on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)
