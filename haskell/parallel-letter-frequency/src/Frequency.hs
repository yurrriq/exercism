module Frequency
  ( frequency,
  )
where

import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.Char (isLetter, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

frequency :: Int -> [Text] -> Map Char Int
frequency _ [] = Map.empty
frequency n texts = Map.unionsWith (+) results
  where
    results = map letterFrequency texts `using` parListChunk m rdeepseq
    m = length texts `div` n

letterFrequency :: Text -> Map Char Int
letterFrequency = Text.foldl' go Map.empty
  where
    go seen c
      | isLetter c = Map.insertWith (+) (toLower c) 1 seen
      | otherwise = seen
