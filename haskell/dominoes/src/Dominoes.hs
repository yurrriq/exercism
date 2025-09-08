{-# LANGUAGE DataKinds #-}

module Dominoes
  ( chain,
  )
where

import Control.Monad (MonadPlus, guard, join, msum, mzero, (>=>))
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Bitraversable (bitraverse)
import GHC.Generics (Generic)
import Refined (FromTo (..), Refined, refine, unrefine)

type Tile = (Pips, Pips)

newtype Pips
  = Pips {unPips :: Refined (FromTo 1 6) Int}
  deriving (Eq, Generic, Ord)

instance Show Pips where
  show :: Pips -> String
  show = show . unrefine . unPips

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain = mkTiles >=> chainTiles >=> unTiles

chainTiles :: (MonadPlus m) => [Tile] -> m [Tile]
chainTiles [] = return []
chainTiles tiles =
  tiles `chainTilesWith` \(tile, rest) ->
    (tile :) <$> chainTile tile rest

chainTilesWith :: (MonadPlus m) => [a] -> ((a, [a]) -> m b) -> m b
chainTilesWith tiles f = msum (map f (holes tiles))

chainTile :: (MonadPlus m) => Tile -> [Tile] -> m [Tile]
chainTile start tiles
  | null tiles =
      [] <$ guard (uncurry (==) start)
  | otherwise =
      tiles `chainTilesWith` \(tile, rest) ->
        do
          next@(_, right) <- start `append` tile
          (next :) <$> chainTile (second (const right) start) rest

append :: (MonadPlus m) => Tile -> Tile -> m Tile
append (_, start) (left, right)
  | left == start = return (left, right)
  | right == start = return (right, left)
  | otherwise = mzero

mkTiles :: (Traversable t, MonadPlus m) => t (Int, Int) -> m (t Tile)
mkTiles = traverse mkTile

mkTile :: (MonadPlus m) => (Int, Int) -> m Tile
mkTile =
  either (const mzero) return
    . bitraverse (fmap Pips . refine) (fmap Pips . refine)

unTiles :: (Traversable t, MonadPlus m) => t Tile -> m (t (Int, Int))
unTiles = return . fmap unTile

unTile :: Tile -> (Int, Int)
unTile = both (unrefine . unPips)

holes :: [a] -> [(a, [a])]
holes [] = []
holes (x : xs) = (x, xs) : map (second (x :)) (holes xs)

both :: (Bifunctor p) => (a -> b) -> p a a -> p b b
both = join bimap
