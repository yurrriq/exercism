module Accumulate where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f coll = [(f x) | x <- coll]
