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

import           Data.Map.Strict (Map, fromListWith)

-- | A nucleotide is a character.
type Nucleotide = Char

-- | A DNA string is a list of nucleotides.
type DNA = [Nucleotide]

-- | The frequency of a nucleotide in a DNA string is an integer.
type Frequency  = Int

-- | Given a nucleotide and a DNA string, returns the frequency of
-- the given nucleotide in the given string, throwing an exception for any
-- invalid nucleotide.
count :: Nucleotide -> DNA -> Frequency
count c | valid c == c = length . filter ((c ==) . valid)

-- | Given a DNA string, returns a map from a nucleotide to its frequency
-- in the given string, throwing an exception for any invalid nucleotide.
nucleotideCounts :: DNA -> Map Nucleotide Frequency
nucleotideCounts = fromListWith (+) .
                   -- (zip "ACGT" (repeat 0) ++) .
                   ([('A', 0), ('C', 0), ('G', 0), ('T', 0)] ++) .
                   map (flip (,) 1 . valid)

-- | Given a nucleotide, returns it if valid, otherwise throws an exception.
valid :: Nucleotide -> Nucleotide
valid c | isValid c = c
        | otherwise = throwInvalid c

-- | Given a nucleotide, returns @True@ if valid, otherwise @False@.
isValid :: Nucleotide -> Bool
isValid = (`elem` "ACGT")

-- | Given an invalid nucleotide, throwns an exception.
throwInvalid :: Nucleotide -> e
throwInvalid = error . ("invalid nucleotide " ++) . show
