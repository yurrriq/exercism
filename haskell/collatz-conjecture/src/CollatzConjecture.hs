-- |
-- Module      : CollatzConjecture
-- Description : ...
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module CollatzConjecture
  ( collatz,
  )
where

collatz :: Integer -> Maybe Integer
collatz = go 0
  where
    go steps n
      | n <= 0 = Nothing
      | n == 1 = Just steps
      | even n = go (steps + 1) (n `div` 2)
      | otherwise = go (steps + 1) (3 * n + 1)
