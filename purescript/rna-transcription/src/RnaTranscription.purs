module RnaTranscription
  ( toRNA
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

toRNA :: String -> Maybe String
toRNA = map fromCharArray <<< traverse transcribeNucleotide <<< toCharArray

transcribeNucleotide :: Char -> Maybe Char
transcribeNucleotide nucleotide =
  case nucleotide of
    'G' -> Just 'C'
    'C' -> Just 'G'
    'T' -> Just 'A'
    'A' -> Just 'U'
    _ -> Nothing
