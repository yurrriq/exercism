{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : PrimeFactors
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Computing the prime factors of a given natural number.
-}

module PrimeFactors where

primeFactors :: Integral a => a -> [a]
primeFactors = flip go primes
  where
    go n pps@(p:ps)
      | n < 2               = []
      | n < p * p           = [n]
      | p `divides` n       = p : go (n `div` p) pps
      | not $ p `divides` n = go n ps
      | otherwise           = undefined
    go _ [] = undefined

divides :: Integral a => a -> a -> Bool
d `divides` n = n `rem` d == 0

-- Adapted from "The Genuine Sieve of Eratosthenes" by Melissa O'Neill
-- http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

primes :: Integral a => [a]
primes = 2 : ([3..] `minus` composites)
  where
    composites = union [multiples p | p <- primes]

multiples :: Integral a => a -> [a]
multiples n = map (n*) [n..]

minus :: Integral a => [a] -> [a] -> [a]
xxs@(x:xs) `minus` yys@(y:ys) = case compare x y of
  LT -> x : (xs `minus` yys)
  EQ -> xs `minus` ys
  GT -> xxs `minus` ys
[] `minus` _ = undefined
_ `minus` [] = undefined

union :: Integral a => [[a]] -> [a]
union = foldr merge []
  where
    merge (x:xs) ys = x:merge' xs ys
    merge [] _ = undefined
    merge' xxs@(x:xs) yys@(y:ys) = case compare x y of
      LT -> x : merge' xs yys
      EQ -> x : merge' xs ys
      GT -> y : merge' xxs ys
    merge' [] _ = undefined
    merge' _ [] = undefined
