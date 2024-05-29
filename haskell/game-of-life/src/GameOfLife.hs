module GameOfLife
  ( tick,
  )
where

import Control.Lens (ifoldl', imap)
import Data.Ix (Ix, inRange)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2 (..))

tick :: [[Int]] -> [[Int]]
tick input = imap (imap . go) input
  where
    go y x cell
      | numLiveNeighbors == 3 = 1
      | cell == 1 && numLiveNeighbors == 2 = 1
      | otherwise = 0
      where
        numLiveNeighbors = Set.size (liveNeighbors (V2 x y))
    liveNeighbors = Set.intersection grid . neighborsInRange (pure 0, maxCell)
    (maxCell, grid) = mkGrid input

mkGrid :: [[Int]] -> (V2 Int, Set (V2 Int))
mkGrid = ifoldl' (ifoldl' . go) (pure 0, Set.empty)
  where
    go y x (_, grid) 1 = (V2 x y, Set.insert (V2 x y) grid)
    go y x (_, grid) _ = (V2 x y, grid)

neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = Set.filter (inRange range) (neighborsOf point)

neighborsOf :: (Applicative f, Num a, Num (f a), Ord (f a), Traversable f) => f a -> Set (f a)
neighborsOf = Set.fromList . flip map adjacencies . (+)

adjacencies :: (Applicative f, Num a, Eq (f a), Traversable f) => [f a]
adjacencies = filter (/= pure 0) $ sequenceA (pure [-1, 0, 1])
