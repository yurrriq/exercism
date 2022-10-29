module Isogram
  ( isIsogram,
  )
where

import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram = go Set.empty . map toUpper . filter isAlpha
  where
    go _ "" = True
    go seen (c : cs)
      | c `Set.member` seen = False
      | otherwise = go (Set.insert c seen) cs
