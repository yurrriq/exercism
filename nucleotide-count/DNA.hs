module DNA (count, nucleotideCounts) where

import Control.Monad (ap)
import Data.List (find)
import Data.Map (Map, fromList)

count :: Char -> String -> Int
count nucleotide string =
  maybe (length $ filter (== nucleotide) string)
  throwInvalid $ find isInvalid (nucleotide:string)

nucleotideCounts :: String -> Map Char Int
nucleotideCounts string =
  maybe (fromList . ap zip (map (flip count string)) $ "ACGT")
  throwInvalid $ find isInvalid string

throwInvalid :: Char -> e
throwInvalid = (error . ("invalid nucleotide " ++) . show)

isInvalid :: Char -> Bool
isInvalid = not . (`elem` "ACGT")
