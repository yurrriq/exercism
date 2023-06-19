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

-- | Given a number, returns the sum of all the multiples of 3 or 5
-- (@'sumOfMultiples' [3,5]@) up to but not including that number.
sumOfMultiplesDefault :: Integral a => a -> a
sumOfMultiplesDefault = sumOfMultiples [3, 5]

-- | Given a list of numbers and a number, returns the sum of all the multiples
-- of any of the numbers in the list up to but not including that number.
sumOfMultiples :: (Integral a) => [a] -> a -> a
sumOfMultiples = (sum .) . (. fromOneToExclusive) . anyDivides

-- | Given a number, @x@, returns the list of numbers from 1 to @x - 1@.
fromOneToExclusive :: Integral a => a -> [a]
fromOneToExclusive = enumFromTo 1 . pred

-- | Given two lists of numbers, @xs@ and @ys@, returns a possibly empty list
-- of every @y@ in @ys@ such that some @x@ in @xs@ 'divides' @y@.

-- |
-- __List comprehension__:
--
-- > anyDivides xs ys = [y | y <- ys, any (`divides` y) xs]
--
-- __LaTeX__:
--
-- @
-- \{ y : y \in Y \quad \land \quad \exists \, x \; ( x \in X \land x\ |\ y ) \;\}
-- @
anyDivides :: Integral a => [a] -> [a] -> [a]
anyDivides = filter . flip (any . flip divides)

-- | Given two numbers, returns @True@ if the first divides the second,
-- otherwise @False@.
divides :: Integral a => a -> a -> Bool
d `divides` n = n `rem` d == 0
