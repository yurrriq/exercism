module Anagram
  ( anagramsFor
  , filterCaseSensitiveAnagramsFor
  , hasSameLettersAs
  , isCaseSensitiveAnagramOf
  ) where

import Prelude
import Data.Array (filter, sort)
import Data.Function (on)
import Data.String (fromCodePointArray, toCodePointArray, toLower)

-- | Given a string and an array of strings, return an subarray where each
-- | string is an anagram for the given string.
anagramsFor :: String -> Array String -> Array String
anagramsFor = filterCaseSensitiveAnagramsFor <<< toLower

-- | Given a lowercase string and an array of strings, return a subarray where
-- | each string in lowercase `isCaseSensitiveAnagramOf` of the given string.
filterCaseSensitiveAnagramsFor :: String -> Array String -> Array String
filterCaseSensitiveAnagramsFor word =
  filter (isCaseSensitiveAnagramOf word <<< toLower)

-- | Given two strings, return `True` if they are not equal but the first
-- | `hasSameLettersAs` the second, otherwise `False`.
isCaseSensitiveAnagramOf :: String -> String -> Boolean
isCaseSensitiveAnagramOf word candidate =
  (candidate /= word) &&
    (candidate `hasSameLettersAs` word)

-- | Given two strings, return `True` if they have the same letters, otherwise
-- | `False`.
hasSameLettersAs :: String -> String -> Boolean
hasSameLettersAs = (==) `on` (fromCodePointArray <<< sort <<< toCodePointArray)
