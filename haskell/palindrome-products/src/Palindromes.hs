module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import Control.Monad (guard)
import Data.List (tails)
import Data.Maybe (listToMaybe)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome from to =
  do
    let palindromeProducts = palindromeProductsWithin from to
    guard (not (null palindromeProducts))
    pure (maximum palindromeProducts)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome from to = listToMaybe (palindromeProductsWithin from to)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

palindromeProductsWithin :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
palindromeProductsWithin from to =
  do
    x : ys <- tails [from .. to]
    y <- x : ys
    let xy = x * y
    guard (isPalindrome (show xy))
    let products = productsWithin from to xy
    guard (not (null products))
    pure (xy, products)

productsWithin :: Integer -> Integer -> Integer -> [(Integer, Integer)]
productsWithin from to n =
  do
    x <- [from .. to]
    let (y, r) = n `divMod` x
    guard (r == 0 && from <= y && y <= to)
    pure (x, y)
