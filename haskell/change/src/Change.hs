module Change
  ( findFewestCoins,
  )
where

import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)

-- | Given an amount and a list of coin denominations, determine the shortest
-- list of coins such that the value is equal to the desired amount.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins _ [] = Nothing
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise =
    forMaybes [1 .. fromInteger (target `div` minimum coins)] $ \n ->
      knapsack n target coins

-- | Using at max @n@ items from a list @xs@, allowing for any non-negative
-- number of copies, find a list of items @ys@ such that @sum ys == goal@.
knapsack :: Int -> Integer -> [Integer] -> Maybe [Integer]
knapsack 0 _ _ = Nothing
knapsack 1 goal xs = pure <$> find (== goal) xs
knapsack n goal xs =
  forMaybes xs $ \x ->
    (x :) <$> knapsack (pred n) (goal - x) (filter (>= x) xs)

forMaybes :: [a] -> (a -> Maybe b) -> Maybe b
forMaybes xs f = listToMaybe (mapMaybe f xs)
