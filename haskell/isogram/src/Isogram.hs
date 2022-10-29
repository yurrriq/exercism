module Isogram
  ( isIsogram,
  )
where

import Data.Char (isAlpha, toUpper)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram = (nub >>= (==)) . map toUpper . filter isAlpha
