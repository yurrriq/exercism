-- |
-- Module      : Series
-- Copyright   : Public Domain
-- License     : Unlicense
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Calculating the largest product for a series of consecutive digits of length n.
module Series (digits, slices, largestProduct) where

import Control.Monad (ap, liftM2)
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Function.Pointless ((.:))
import Data.List (genericDrop, genericTake, tails)

-- | Given a string, convert each numeric character to its value.
digits :: Integral a => String -> [a]
digits = map (fromIntegral . digitToInt)

-- | Given a string and a length @n@, call 'digits' on the string and
-- return a list of slices of length @n@.
slices :: Integral a => a -> String -> [[a]]
slices n = go . digits
  where
    go = map (genericTake n) . dropLast n . tails

-- | Given a list of lists of numbers, compute the product of the numbers in
-- each of the nested lists and return the largest.
largestProduct :: Integral a => a -> String -> a
largestProduct = (findLargest . map product) .: slices

-- | Given a list of numbers, return the largest. If list is empty, return 1.
findLargest :: Integral a => [a] -> a
findLargest = ap (liftM2 bool maximum (const 1)) null

-- | Return all but the last element of a given list.
-- dropLast1 :: [a] -> [a]
-- dropLast1 = dropLast 1

-- | Given a number @n@ and a list @l@, return a list of
-- all but the last @n@ elements in @l@.
dropLast :: Integral a => a -> [b] -> [b]
dropLast = ap (zipWith const) . genericDrop

-- Local Variables:
-- compile-command: "runhaskell -Wall *_test.hs"
-- End:
