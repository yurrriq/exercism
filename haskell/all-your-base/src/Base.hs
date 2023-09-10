module Base (rebase) where

import Control.Monad (foldM)
import Data.List (unfoldr)
import Data.Tuple (swap)

rebase :: (Integral a) => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits =
  do
    ibase <- maybeBase inputBase
    obase <- maybeBase outputBase
    toDigits obase <$> fromDigits ibase inputDigits

fromDigits :: (Integral a) => a -> [a] -> Maybe a
fromDigits base = foldM go 0
  where
    go acc n =
      if isNonNegative n && n < base
        then Just (n + acc * base)
        else Nothing

toDigits :: (Integral a) => a -> a -> [a]
toDigits base = reverse . unfoldr go
  where
    go 0 = Nothing
    go n = Just . swap $ n `divMod` base

isNonNegative :: (Integral a) => a -> Bool
isNonNegative = (>= 0)

maybeBase :: (Integral a) => a -> Maybe a
maybeBase n =
  if n < 2
    then Nothing
    else Just n
