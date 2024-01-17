module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import Control.Monad (guard)
import Data.List (sortBy)
import Data.Maybe (listToMaybe)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome from to =
  fmap (factorsWithin from to) . listToMaybe $
    concatMap
      (sortBy (flip compare) . flip palindromeProductsWithin to)
      [to, to - 1 .. from]

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome from to =
  fmap (factorsWithin from to) . listToMaybe $
    concatMap
      (palindromeProductsWithin from)
      [from .. to]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

palindromeProductsWithin :: Integer -> Integer -> [Integer]
palindromeProductsWithin from to =
  do
    x <- [from .. to]
    let y = from + to - x
    let xy = x * y
    guard (isPalindrome (show xy))
    pure xy

factorsWithin :: Integer -> Integer -> Integer -> (Integer, [(Integer, Integer)])
factorsWithin from to n =
  ( n,
    do
      x <- [from .. to]
      let (y, r) = n `divMod` x
      guard (r == 0 && from <= y && y <= to && x <= y)
      pure (x, y)
  )
