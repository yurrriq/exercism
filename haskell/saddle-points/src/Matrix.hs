module Matrix
  ( saddlePoints,
  )
where

import Data.Array (Array, Ix, assocs, bounds, (!))

saddlePoints :: (Ix i, Enum i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix =
  [ (y, x)
    | ((y, x), saddlePoint) <- assocs matrix,
      saddlePoint == maximum [matrix ! (y, col) | col <- [xLower .. xUpper]],
      saddlePoint == minimum [matrix ! (row, x) | row <- [yLower .. yUpper]]
  ]
  where
    ((yLower, xLower), (yUpper, xUpper)) = bounds matrix
