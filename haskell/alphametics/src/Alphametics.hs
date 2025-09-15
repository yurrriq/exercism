{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Alphametics
  ( solve,
  )
where

import Control.Monad (MonadPlus (..), guard)
import Data.FastDigits (undigits)
import Data.List (permutations, uncons)
import Data.List.Extra (sumOn')
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Text.Trifecta
  ( Parser,
    Result (..),
    parseString,
    sepEndBy,
    some,
    string,
    upper,
  )

data Puzzle
  = Puzzle
  { summands :: [String],
    result :: String
  }
  deriving (Show)

solve :: String -> Maybe [(Char, Int)]
solve input =
  do
    Puzzle {..} <- parsePuzzle input
    let theWords = result : summands
        letters = foldMap Set.fromList theWords
    guard (Set.size letters <= 10)
    firsts <- mapM (fmap fst . uncons) theWords
    fmap Map.toList . listToMaybe $
      [ mapping
        | digits <- permutations [0 .. 9],
          let mapping = Map.fromList (zip (Set.toList letters) digits),
          all ((0 /=) . (mapping !)) firsts,
          sumOn' (wordValue mapping) summands == wordValue mapping result
      ]

wordValue :: Map Char Int -> String -> Integer
wordValue mapping = undigits @Int 10 . reverse . map (mapping !)

parsePuzzle :: (MonadPlus m) => String -> m Puzzle
parsePuzzle input =
  case parseString puzzle mempty input of
    Success result -> return result
    Failure _err -> mzero

puzzle :: Parser Puzzle
puzzle =
  Puzzle
    <$> (some upper `sepEndBy` string " + " <* string " == ")
    <*> some upper
