module Change
  ( findFewestCoins,
  )
where

import Control.Monad (msum)
import Data.Group (Group, invert)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Sum (..), getSum)

-- | Given an amount and a list of coin denominations, determine the shortest
-- list of coins such that the value is equal to the desired amount.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins _ [] = Nothing
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise = fmap (map getSum) unboundedKnapsack
  where
    unboundedKnapsack =
      msum
        [ knapsack n goal xs
          | n <- [1 .. fromInteger (target `div` minimum coins)]
        ]
    goal = Sum target
    xs = map Sum coins

-- | Using at max @n@ items from a list @xs@, allowing for any non-negative
-- number of copies, find a list of items @ys@ such that @mconcat ys == goal@.
knapsack :: (Eq a, Ord a, Group a) => Int -> a -> [a] -> Maybe [a]
knapsack 0 _ _ = Nothing
knapsack 1 goal xs = pure <$> find (== goal) xs
knapsack n goal xs =
  listToMaybe . flip mapMaybe xs $ \x ->
    (x :) <$> knapsack (pred n) (goal <> invert x) (filter (>= x) xs)
