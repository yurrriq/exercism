{-|
Module      : DNA
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Calculating nucleotide frequencies in DNA strings.
-}
module DNA where

import Data.Map.Strict (Map, fromListWith)

-- | Given a nucleotide and a DNA string, returns the frequency of
-- the given nucleotide in the given string, throwing an exception for any
-- invalid nucleotide.
count :: Char -> String -> Int
count c | valid c == c = length . filter ((c ==) . valid)

-- | Given a DNA string, returns a map from a nucleotide to its frequency
-- in the given string, throwing an exception for any invalid nucleotide.
nucleotideCounts :: String -> Map Char Int
nucleotideCounts = fromListWith (+) .
                   -- (zip "ACGT" (repeat 0) ++) .
                   ([('A', 0), ('C', 0), ('G', 0), ('T', 0)] ++) .
                   map (flip (,) 1 . valid)

-- | Given a nucleotide, returns it if valid, otherwise throws an exception.
valid :: Char -> Char
valid c | not $ c `elem` "ACGT" = error $ "invalid nucleotide " ++ show c
        | otherwise   = c
