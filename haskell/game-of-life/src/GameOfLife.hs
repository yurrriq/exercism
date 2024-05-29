module GameOfLife
  ( tick,
  )
where

import Control.Lens (ifoldl', imap)
import Data.Ix (Ix, inRange)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2 (..))

type Cell = Int

type Coordinates = V2 Int

type Generation = (Coordinates, Set Coordinates)

tick :: [[Cell]] -> [[Cell]]
tick input = imap (imap . tickCell (mkGrid input)) input

tickCell :: Generation -> Int -> Int -> Cell -> Cell
tickCell generation y x cell
  | numLiveNeighbors == 3 = 1
  | cell == 1 && numLiveNeighbors == 2 = 1
  | otherwise = 0
  where
    numLiveNeighbors = Set.size (liveNeighbors generation (V2 x y))

liveNeighbors :: Generation -> Coordinates -> Set Coordinates
liveNeighbors (maxCell, grid) =
  Set.intersection grid . neighborsInRange (pure 0, maxCell)

mkGrid :: [[Cell]] -> Generation
mkGrid = ifoldl' (ifoldl' . go) (pure 0, Set.empty)
  where
    go y x (_, grid) 1 = (V2 x y, Set.insert (V2 x y) grid)
    go y x (_, grid) _ = (V2 x y, grid)

neighborsInRange ::
  (Applicative f, Traversable f, Num a, Num (f a), Ix (f a)) =>
  (f a, f a) ->
  f a ->
  Set (f a)
neighborsInRange range point = Set.filter (inRange range) (neighborsOf point)

neighborsOf ::
  (Applicative f, Traversable f, Num a, Num (f a), Ord (f a)) =>
  f a ->
  Set (f a)
neighborsOf = Set.fromList . flip map adjacencies . (+)

adjacencies :: (Applicative f, Num a, Eq (f a), Traversable f) => [f a]
adjacencies = filter (/= pure 0) $ sequenceA (pure [-1, 0, 1])
