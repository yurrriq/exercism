-- |
-- Module      : CollatzConjecture
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module CollatzConjecture
  ( collatz,
  )
where

-- | Compute the number of steps required to reach 1.
collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just (fromIntegral (length (collatzSeq n)))

collatzSeq :: Integer -> [Integer]
collatzSeq = takeWhile (/= 1) . iterate collatzIter

collatzIter :: Integer -> Integer
collatzIter 1 = 1
collatzIter n
  | even n = n `div` 2
  | otherwise = 3 * n + 1
