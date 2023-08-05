module ScrabbleScore
  ( scoreWord
  ) where

import Prelude

import Data.Array (concatMap, filter, head)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.String (toUpper)
import Data.String.CodePoints (CodePoint, fromCodePointArray, toCodePointArray)
import Data.CodePoint.Unicode (isLetter)

scoreWord :: String -> Int
scoreWord = foldl go 0 <<< filter isLetter <<< toCodePointArray
  where
  go score letter = score + scoreLetter letter

scoreLetter :: CodePoint -> Int
scoreLetter letter = fromMaybe 0 (flip lookup points =<< maybeUpperLetter)
  where
  maybeUpperLetter = head (toCodePointArray (toUpper (fromCodePointArray [ letter ])))

points :: Map CodePoint Int
points = Map.fromFoldable $ concatMap (\(Tuple n cs) -> map (\c -> Tuple c n) (toCodePointArray cs)) legend

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
