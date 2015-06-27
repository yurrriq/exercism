{-|
Module      : Scrabble
Copyright   : (c) Eric Bailey, 2015
License     : WTFPL

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Computing the Scrabble score for words.
-}
module Scrabble (scoreLetter, scoreWord) where

import Data.Char (isLetter, toUpper)
import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- | Given a letter, returns its 'Value'.
-- 
-- __Note__: Throws if the given character is non-alphabetic.
scoreLetter :: Char -> Int
scoreLetter = (points !) . toUpper

-- | Given a word, filters out non-alphabetic characters and returns
-- the total value of every letter. When the word contains no alphabetic
-- characters, returns 0.
scoreWord :: String -> Int
scoreWord = foldl' ((. scoreLetter) . (+)) 0 . filter isLetter

points :: Map Char Int
points = Map.fromList $ legend >>= uncurry (map . flip (,))

legend :: [(Int, String)]
legend = [(1,  "AEIOULNRST"),
          (2,  "DG"),
          (3,  "BCMP"),
          (4,  "FHVWY"),
          (5,  "K"),
          (8,  "JX"),
          (10, "QZ")]
