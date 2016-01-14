{-|
Module      : Anagram
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Selecting sublists of anagrams.
-}
module Anagram (
  anagramsFor,
  filterCaseSensitiveAnagramsFor,
  isCaseSensitiveAnagramOf,
  hasSameLettersAs
  -- * Non-pointless Version
  -- $alternate
  ) where

import           Control.Monad (ap)
import           Data.Char     (toLower)
import           Data.List     (sort)

-- | Given a string and a list of strings, returns a sublist where
-- each string is an anagram for the given string.
anagramsFor :: String -> [String] -> [String]
anagramsFor = filterCaseSensitiveAnagramsFor . map toLower

-- | Given a lowercase string and a list of strings, returns a sublist where
-- each string in lowercase 'isCaseSensitiveAnagramOf' of the given string.
filterCaseSensitiveAnagramsFor :: String -> [String] -> [String]
filterCaseSensitiveAnagramsFor =
  filter . flip (isCaseSensitiveAnagramOf . map toLower)

-- | Given two strings, returns @True@ if they are not equal
-- but the first 'hasSameLettersAs' the second, otherwise @False@.
isCaseSensitiveAnagramOf :: String -> String -> Bool
isCaseSensitiveAnagramOf = ap (ap . ((&&) .) . (/=)) hasSameLettersAs

-- | Given two strings, returns @True@ if they have the same letters,
-- otherwise @False@.
hasSameLettersAs :: String -> String -> Bool
hasSameLettersAs = (. sort) . (==) . sort

{- $alternate
@
anagramsFor :: String -> [String] -> [String]
anagramsFor word = filterCaseSensitiveAnagramsFor (map toLower word)

filterCaseSensitiveAnagramsFor :: String -> [String] -> [String]
filterCaseSensitiveAnagramsFor a =
  filter (\\b -> (map toLower b) \`isCaseSensitiveAnagramOf\` a)

isCaseSensitiveAnagramOf :: String -> String -> Bool
x \`isCaseSensitiveAnagramOf\` y = (x /= y) && (x \`hasSameLettersAs\` y)

hasSameLettersAs :: String -> String -> Bool
x \`hasSameLettersAs\` y = sort x == sort y
@
-}
