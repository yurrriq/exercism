module ReverseString
  ( reverseString,
  )
where

import Data.List (foldl')

reverseString :: String -> String
reverseString = foldl' (flip (:)) ""
