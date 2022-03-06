module Prime
  ( nth,
  )
where

-- | Compute the nth prime.
nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just (primes !! pred n)

-- | Compute an infinite list of primes, using optimal trial division.
primes :: [Integer]
primes = 2 : 3 : filter isPrime [5, 7 ..]

-- | Test for primality, using optimal trial divison.
isPrime :: Integer -> Bool
isPrime n = foldr (\p r -> p * p > n || rem n p > 0 && r) True primes
