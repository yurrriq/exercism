module ArmstrongNumbers
  ( armstrong,
  )
where

import Data.List (unfoldr)
import Data.Monoid (Sum (..))
import Data.Tuple (swap)

armstrong :: (Integral a) => a -> Bool
armstrong 0 = True
armstrong n = foldMap (Sum . (^ k)) ds == Sum n
  where
    k = length ds
    ds = toDigits 10 n

toDigits :: (Integral a) => a -> a -> [a]
toDigits base = unfoldr go
  where
    go 0 = Nothing
    go m = Just . swap $ m `divMod` base
