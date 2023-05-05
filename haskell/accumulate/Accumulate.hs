-- |
-- Module      : Accumulate
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- The 'accumulate' operation.
module Accumulate where

-- | Given a collection and an operation to perform on each element
-- of the collection, returns a new collection containing the result
-- of applying that operation to each element of the input collection.
accumulate :: (a -> b) -> [a] -> [b]
accumulate f coll = [f x | x <- coll]
