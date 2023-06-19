{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : DNA
-- Copyright   : (c) Eric Bailey, 2015-2023
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Calculating nucleotide frequencies in DNA strings.
module DNA
  ( Nucleotide (..),
    nucleotideCounts,
  )
where

import Data.Foldable (foldlM)
import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map

data Nucleotide
  = -- | Adenine
    A
  | -- | Cytosine
    C
  | -- | Guanine
    G
  | -- | Thymine
    T
  deriving (Eq, Ord, Show)

-- | Given a DNA strand, return either the first invalid nucleotide found or a
-- map from a nucleotide to its frequency in the given strand.
nucleotideCounts :: String -> Either Char (Map Nucleotide Int)
nucleotideCounts = foldlM go emptyFrequencies
  where
    go counts character =
      (\nucleotide -> Map.insertWith (+) nucleotide 1 counts)
        <$> transcribe character

transcribe :: Char -> Either Char Nucleotide
transcribe = \case
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  x -> Left x

emptyFrequencies :: Map Nucleotide Int
emptyFrequencies = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
