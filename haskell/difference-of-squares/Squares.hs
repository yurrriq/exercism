-- |
-- Module      : Squares
-- Copyright   : Public Domain
-- License     : Unlicense
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Finding the difference between the sum of the squares
-- and the square of the sums of the first @n@ natural numbers.
module Squares (squareOfSums, sumOfSquares, difference) where

import Control.Monad (ap, join, liftM2)

-- | Given a number @n@, return the square of the sum of
-- the first @n@ natural numbers.
--
-- <<../squareOfSums.png>>
squareOfSums :: Integral a => a -> a
squareOfSums = join (*) . (`div` 2) . ap (*) succ

-- \left(\sum\limits_{k=1}^n k\right)^2 = \left(\frac{n(n + 1)}{2}\right)^2

-- | Given a number @n@, return the sum of the squares of
-- the first @n@ natural numbers.
--
-- <<../sumOfSquares.png>>
sumOfSquares :: Integral a => a -> a
sumOfSquares n = n * succ n * succ (2 * n) `div` 6

-- \sum\limits_{k=1}^n k^2 = \frac{n(n + 1)(2n + 1)}{6}

-- | Given a number @n@, return difference between the sum of the squares and
-- the square of the sum of the first @n@ natural numbers.
--
-- <<../difference.png>>
difference :: Integral a => a -> a
difference = liftM2 (-) squareOfSums sumOfSquares

-- \left(\sum\limits_{k=1}^n k\right)^2 - \sum\limits_{k=1}^n k^2
