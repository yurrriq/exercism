module Triplet
  ( Triplet
  , isPythagorean
  , mkTriplet
  , pythagoreanTriplets
  ) where


import           Control.Monad (guard, join)


newtype Triplet a =
  Triplet (a, a, a)
  deriving (Eq, Show)


isPythagorean :: Triplet Int -> Bool
isPythagorean (Triplet (a, b, c)) = square a + square b == square c


mkTriplet :: Int -> Int -> Int -> Triplet Int
mkTriplet a b c
  | b < a = mkTriplet b a c
  | c < b = mkTriplet a c b
  | otherwise = Triplet (a, b, c)


pythagoreanTriplets :: Int -> Int -> [Triplet Int]
pythagoreanTriplets minFactor maxFactor =
  do a <- [minFactor .. maxFactor]
     b <- [a .. maxFactor]
     c <- [b .. maxFactor]
     let triplet = mkTriplet a b c
     guard (isPythagorean triplet)
     pure triplet


square :: Integral a => a -> a
square = join (*)
