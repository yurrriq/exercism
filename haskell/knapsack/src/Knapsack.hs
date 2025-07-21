module Knapsack
  ( maximumValue,
  )
where

import Control.Arrow ((***))
import Data.Semigroup (Max (..))

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight items =
  getMax $
    foldMap (Max . fst) $
      knapsack maxWeight items

knapsack :: (Ord a, Num a, Num b) => a -> [(a, b)] -> [(b, [(a, b)])]
knapsack _ [] = [(0, [])]
knapsack maxWeight ((weight, value) : items)
  | weight > maxWeight = excluded
  | otherwise = included ++ excluded
  where
    excluded = knapsack maxWeight items
    included =
      ((value +) *** ((weight, value) :))
        <$> knapsack (maxWeight - weight) items
