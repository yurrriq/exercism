-- |
-- Module      : ETL
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Converting Scrabble scoring systems.
module ETL (Letter, NewData, NewDatum, OldData, OldDatum, transform) where

import qualified Data.Char as Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A letter is a one-character-long string.
type Letter = String

-- | The old data is a map from score to list of letters.
type OldData = Map Int [Letter]

-- | An old datum is a pair of a score and a list of letters.
type OldDatum = (Int, [Letter])

-- | The new data is a map from letter to score.
type NewData = Map Letter Int

-- | A new datum is a pair of a letter and a score.
type NewDatum = (Letter, Int)

-- | Given some 'OldData', updates it to the new system and returns 'NewData'.
transform :: OldData -> NewData
transform = Map.fromList . concatMap transform' . Map.toList

-- | Given an 'OldDatum' @(score, letters)@, returns a list of 'NewDatum's for
-- each of the @letters@ with @score@ as every score.
transform' :: OldDatum -> [NewDatum]
transform' = uncurry $ map . flip ((,) . lowercase)

-- | Given a string, returns a string with each character converted
-- to lowercase. Also found in "Distribution.Simple.Utils".
lowercase :: String -> String
lowercase = map Char.toLower
