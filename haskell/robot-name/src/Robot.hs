{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Robot
-- Copyright   : (c) Eric Bailey, 2015-2023
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Managing robot factory settings.
module Robot
  ( Robot,
    initialState,
    mkRobot,
    resetName,
    robotName,
  )
where

import Control.Lens (views)
import Control.Lens.TH (makeFields)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.State (StateT, gets, liftIO, modify)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

type RobotFactory a = StateT RunState IO a

type RunState = Set String

initialState :: RunState
initialState = Set.empty

-- | A robot has a name that can be read ('robotName') and reset ('resetName').
newtype Robot = Robot {_robotName :: IORef String}

makeFields ''Robot

-- | Create a 'Robot' and give it a random name.
mkRobot :: RobotFactory Robot
mkRobot =
  do
    robot <- Robot <$> liftIO (newIORef mempty)
    resetName robot
    pure robot

-- | Given a 'Robot' @r@, generate a random name and atomically overwrite @r@'s
-- name.
resetName :: Robot -> RobotFactory ()
resetName robot =
  do
    modify . Set.delete =<< liftIO (robotName robot)
    newName <- randomName
    modify (Set.insert newName)
    liftIO (views name (`atomicWriteIORef` newName) robot)

-- | Given a 'Robot', return its name.
robotName :: Robot -> IO String
robotName = views name readIORef

-- | Generate a random robot name, such as RX837 or BC811.
randomName :: RobotFactory String
randomName =
  flip iterateUntil (mapM randomRIO format)
    =<< gets (flip Set.notMember)
  where
    format = [letter, letter, digit, digit, digit]
    letter = ('A', 'Z')
    digit = ('0', '9')
