module Acronym (abbreviate) where

import           Data.Char       (isLetter, isUpper, toUpper)
import           Data.List.Split (wordsBy)


abbreviate :: String -> String
abbreviate = concatMap go . wordsBy (not . isLetter)
  where
    go :: String -> String
    go []     = []
    go (c:cs) = toUpper c : if allUpper cs then [] else uppers cs


allUpper :: String -> Bool
allUpper = all isUpper


uppers :: String -> String
uppers = filter isUpper
