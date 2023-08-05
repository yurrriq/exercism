module RnaTranscription
  ( toRNA
  ) where

import Control.Semigroupoid ((<<<))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)

toRNA :: String -> Maybe String
toRNA = map fromCharArray <<< traverse transcribeNucleotide <<< toCharArray

transcribeNucleotide :: Char -> Maybe Char
transcribeNucleotide 'G' = Just 'C'
transcribeNucleotide 'C' = Just 'G'
transcribeNucleotide 'T' = Just 'A'
transcribeNucleotide 'A' = Just 'U'
transcribeNucleotide _ = Nothing
