module Prime
  ( nth,
  )
where

import Data.List (nubBy)

-- | Compute the nth prime.
nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just (primes !! pred n)

-- | Naively compute an infinite list of primes.
primes :: [Integer]
primes = nubBy (((> 1) .) . gcd) [2 ..]
