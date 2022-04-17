-- |
-- Module      : PerfectNumbers
-- Description : Determine if a number is perfect, abundant, or deficient.
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
--
-- Determine if a number is 'Perfect', 'Abundant', or 'Deficient' based on
-- Nicomachus's (60 - 120 CE) classification scheme for positive integers.
--
-- The Greek mathematician
-- [Nicomachus](https://en.wikipedia.org/wiki/Nicomachus) devised a
-- classification scheme for positive integers, identifying each as belonging
-- uniquely to the categories of 'Perfect', 'Abundant', or 'Deficient' based on
-- their [aliquot sum](https://en.wikipedia.org/wiki/Aliquot_sum).
--
-- The 'aliquotSum' is defined as the sum of the factors of a number not
-- including the number itself.
module PerfectNumbers where

-- | Nicomachus's classification scheme for positive integers.
data Classification
  = -- | The aliquot sum is less than the number.
    Deficient
  | -- | The aliquot sum is equal to the number.
    Perfect
  | -- | The aliquot sum is great than the number.
    Abundant
  deriving (Eq, Show)

-- | Classify an integer based on Nicomachus's scheme.
--
-- * 'Perfect': 'aliquotSum'@ n == n@
--
--     * @6@ is a 'Perfect' number because @(1 + 2 + 3) == 6@
--
--         >>> classify 6
--         Just Perfect
--
--     * @28@ is a 'Perfect' number because @(1 + 2 + 4 + 7 + 14) == 28@
--
--         >>> classify 28
--         Just Perfect
--
-- * 'Abundant': 'aliquotSum'@ n > n@
--
--     * @12@ is an 'Abundant' number because @(1 + 2 + 3 + 4 + 6) == 16@
--
--         >>> classify 12
--         Just Abundant
--
--     * @24@ is an 'Abundant' number because @(1 + 2 + 3 + 4 + 6 + 8 + 12) == 36@.
--
--         >>> classify 24
--         Just Abundant
--
-- * 'Deficient': 'aliquotSum'@ n < n@
--
--     * 8 is a deficient number because (1 + 2 + 4) = 7
--
--         >>> classify 8
--         Just Deficient
--
--     * Prime numbers are deficient.
--
--         >>> all (== Just Deficient) (map classify [2, 3, 5, 7, 11, 13, 17])
--         True
classify :: Int -> Maybe Classification
classify n
  | n > 0 =
    case compare (aliquotSum n) n of
      EQ -> pure Perfect
      GT -> pure Abundant
      LT -> pure Deficient
  | otherwise = Nothing

-- | The sum of the factors of a number not including the number itself.
--
-- \[
-- \sum_{d=1}^{n-1} \begin{cases}
-- d & \text{if } d \mid n \\
-- 0 & \text{otherwise}
-- \end{cases}
-- \]
--
-- >>> aliquotSum 15 == 1 + 3 + 5
-- True
aliquotSum :: Int -> Int
aliquotSum n = sum [d | d <- [1 .. n -1], d .|. n]

-- | Whether @n@ is divisible by @d@, i.e. @True@ if \(d \mid n\).
--
-- >>> 9 .|. 108
-- True
(.|.) :: Integral a => a -> a -> Bool
d .|. n = n `rem` d == 0

infixr 2 .|.

{-# SPECIALIZE INLINE (.|.) :: Int -> Int -> Bool #-}
