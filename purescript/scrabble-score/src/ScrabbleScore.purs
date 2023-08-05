module ScrabbleScore
  ( scoreWord
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Foldable (foldl)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

scoreWord :: String -> Int
scoreWord = foldl go 0 <<< toCharArray <<< toUpper
  where
  go wordScore = (wordScore + _) <<< scoreLetter

scoreLetter :: Char -> Int
scoreLetter letter = fromMaybe 0 (lookup letter points)

points :: Map Char Int
points = Map.fromFoldable $ concatMap go legend
  where
  go (Tuple n cs) = map (flip Tuple n) (toCharArray cs)

legend :: Array (Tuple Int String)
legend =
  [ Tuple 1 "AEIOULNRST"
  , Tuple 2 "DG"
  , Tuple 3 "BCMP"
  , Tuple 4 "FHVWY"
  , Tuple 5 "K"
  , Tuple 8 "JX"
  , Tuple 10 "QZ"
  ]
