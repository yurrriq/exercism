-- |
-- Module      : SumOfMultiples
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Calculating the sum of multiples.
module SumOfMultiples where

-- | Given a list of numbers and a number, returns the sum of all the multiples
-- of any of the numbers in the list up to but not including that number.
sumOfMultiples :: (Integral a) => [a] -> a -> a
sumOfMultiples multiples =
  sum
    . anyDivides multiples
    . fromOneToExclusive

-- | Given a number, @x@, returns the list of numbers from 1 to @x - 1@.
fromOneToExclusive :: Integral a => a -> [a]
fromOneToExclusive = enumFromTo 1 . pred

-- | Given two lists of numbers, @xs@ and @ys@, returns a possibly empty list
-- of every @y@ in @ys@ such that some @x@ in @xs@ 'divides' @y@.
--
-- \[
--   \{ y \in Y \colon \exists x \in X(x|y) \}
-- \]
anyDivides :: Integral a => [a] -> [a] -> [a]
anyDivides xs ys = [y | y <- ys, any (`divides` y) xs]

-- | Given two numbers, returns @True@ if the first divides the second,
-- otherwise @False@.
divides :: Integral a => a -> a -> Bool
0 `divides` _ = False
d `divides` n = n `rem` d == 0
