module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import Data.Bits ((.&.))
import Data.List (tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  Map.lookupMax $ palindromeProductsWithin minFactor maxFactor

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  Map.lookupMin $ palindromeProductsWithin minFactor maxFactor

palindromeProductsWithin :: Integer -> Integer -> Map Integer [(Integer, Integer)]
palindromeProductsWithin minFactor maxFactor =
  Map.fromListWith (++) $
    [ (xy, [(x, y)])
      | x : ys <- tails [minFactor .. maxFactor],
        y <- x : ys,
        let xy = x * y,
        isPalindrome (show xy)
    ]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = fward == bward
  where
    fward = take (q + (r .&. 1)) xs
    bward = reverse $ drop q xs
    (q, r) = length xs `divMod` 2
