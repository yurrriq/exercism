-- |
-- Module      : Hamming
-- Copyright   : (c) Eric Bailey, 2016
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Computing the Hamming distance between two DNA strands.
module Hamming (distance) where

import Data.Bool (bool)
import Data.Function.Pointless ((.:))

distance :: String -> String -> Int
distance = foldr (bool id (+ 1)) 0 .: zipWith (/=)
