module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)

type ProductWithFactorPairs = (Integer, [(Integer, Integer)])

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

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

palindromeProductsWithin :: Integer -> Integer -> [ProductWithFactorPairs]
palindromeProductsWithin from to =
  do
    x <- [from .. to]
    let y = from + to - x
    let xy = x * y
    guard (isPalindrome (show xy))
    pure (factorsWithin from to xy)

factorsWithin :: Integer -> Integer -> Integer -> ProductWithFactorPairs
factorsWithin from to n =
  ( n,
    do
      x <- [from .. to]
      let (y, r) = n `divMod` x
      guard (r == 0 && from <= y && y <= to && x <= y)
      pure (x, y)
  )
