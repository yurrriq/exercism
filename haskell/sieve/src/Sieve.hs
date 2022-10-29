module Sieve (primesUpTo) where

import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

-- Adapted from the work of Richard Bird, as found in
-- "The Genuine Sieve of Eratosthenes" by Melissa O'Neill.
-- http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = 2 : ([3 .. n] `minus` composites)
  where
    composites = union [multiples p | p <- primesUpTo n]

multiples :: Integral a => a -> [a]
multiples n = [n * n, n * n + n ..]

minus :: Integral a => [a] -> [a] -> [a]
xxs@(x : xs) `minus` yys@(y : ys) =
  case compare x y of
    LT -> x : (xs `minus` yys)
    EQ -> xs `minus` ys
    GT -> xxs `minus` ys
[] `minus` _ = []
xs `minus` [] = xs

union :: Integral a => [[a]] -> [a]
union = foldr merge []
  where
    merge (x : xs) ys = x : merge' xs ys
    merge [] ys = ys
    merge' xxs@(x : xs) yys@(y : ys) =
      case compare x y of
        LT -> x : merge' xs yys
        EQ -> x : merge' xs ys
        GT -> y : merge' xxs ys
    merge' [] ys = ys
    merge' xs [] = xs
