{-# LANGUAGE BlockArguments #-}

module Spiral
  ( spiral,
  )
where

import Data.Functor ((<&>))

spiral :: Int -> [[Int]]
spiral size =
  let maxIndex = size - 1
   in [0 .. maxIndex] <&> \x ->
        [0 .. maxIndex] <&> \y ->
          (\(layer, offset) -> 4 * layer * (size - layer) + offset + 1)
            case (y >= x, y < maxIndex - x) of
              (True, True) -> (x, y - x)
              (True, False) -> (maxIndex - y, x + 3 * y - 2 * maxIndex)
              (False, False) -> (maxIndex - x, 5 * x - y - 2 * maxIndex)
              (False, True) -> (y, 4 * maxIndex - x - 7 * y)
