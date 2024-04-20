module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)

type ProductWithFactorPairs = (Integer, FactorPairs)

type FactorPairs = [(Integer, Integer)]

largestPalindrome :: Integer -> Integer -> Maybe ProductWithFactorPairs
largestPalindrome from to =
  listToMaybe $
    concatMap
      (`palindromeProductsWithin` to)
      [to, to - 1 .. from]

smallestPalindrome :: Integer -> Integer -> Maybe ProductWithFactorPairs
smallestPalindrome from to =
  listToMaybe $
    concatMap
      (palindromeProductsWithin from)
      [from .. to]

palindromeProductsWithin :: Integer -> Integer -> [ProductWithFactorPairs]
palindromeProductsWithin from to =
  do
    x <- [from .. to]
    let y = from + to - x
    let xy = x * y
    guard (isPalindrome (show xy))
    pure (xy, factorPairsWithin from to xy)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

factorPairsWithin :: Integer -> Integer -> Integer -> FactorPairs
factorPairsWithin from to n =
  do
    x <- [from .. to]
    let (y, r) = n `divMod` x
    guard (r == 0 && from <= y && y <= to && x <= y)
    pure (x, y)
