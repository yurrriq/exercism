{-# LANGUAGE LambdaCase #-}
{-|
Module      : Robot
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Simulating the movement of robots.
-}

module Robot (Bearing(..),
              Robot,
              bearing,
              coordinates,
              mkRobot,
              simulate,
              turnLeft,
              turnRight) where

import           Control.Arrow (first, second, (&&&))
import           Control.Lens  (Lens', over, set, view)
import           Control.Monad (foldM)
import           Data.Maybe    (fromJust)

-- | A bearing is a cardinal direction.
data Bearing = North | East | South | West deriving (Bounded, Enum, Eq, Show)

type Coordinates = (Int, Int)

data Direction = Leftward | Rightward

data Robot = Robot { _bearing     :: Bearing,
                     _coordinates :: Coordinates }
           deriving (Eq, Show)

-- GETTERS FOR THE TESTS

bearing :: Robot -> Bearing
bearing = view bearing'

coordinates :: Robot -> Coordinates
coordinates = view coordinates'


mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate r = fromJust . foldM go r
  where
    go        = (Just .) . flip interpret
    interpret = \case
      'A' -> advance
      'L' -> bearing' `over` turnLeft
      'R' -> bearing' `over` turnRight
      _   -> id

turnLeft :: Bearing -> Bearing
turnLeft = turn Leftward

turnRight :: Bearing -> Bearing
turnRight = turn Rightward


-- LENSES

bearing' :: Lens' Robot Bearing
bearing' f (Robot b c) = (`Robot` c) <$> f b

coordinates' :: Lens' Robot Coordinates
coordinates' f (Robot b c) = Robot b <$> f c

turn :: Direction -> Bearing -> Bearing
turn dir = toEnum . (`mod` 4) . (4 +) . turn' dir . fromEnum
  where turn' = \case Leftward -> pred; Rightward -> succ

advance :: Robot -> Robot
advance = set coordinates' =<< advance'
  where
    advance'  = uncurry (\case
                           North -> second succ
                           East  -> first succ
                           South -> second pred
                           West  -> first pred) .
                (bearing &&& coordinates)
