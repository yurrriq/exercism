-- |
-- Module      : Robot
-- Copyright   : (c) Eric Bailey, 2015
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

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Monad.State (StateT, get, liftIO, modify)
-- import Data.List (delete)
import System.Random (randomRIO)

-- | A robot has a name that can be read ('robotName') and reset ('resetName').
newtype Robot = Robot {name :: MVar String}

type RunState = [String]

initialState :: RunState
initialState = []

-- | Creates a 'Robot' and gives it a random name.
mkRobot :: StateT RunState IO Robot
mkRobot = Robot <$> liftIO (randomName >>= newMVar)

-- | Given a 'Robot' @r@, generates a random name and atomically overwrites
-- @r@'s name.
resetName :: Robot -> StateT RunState IO ()
resetName robot =
  do
    seen <- get
    newName <- liftIO randomName
    if newName `elem` seen
      then resetName robot
      else do
        oldName <- liftIO $ swapMVar (name robot) newName
        modify (oldName :)

-- | Given a 'Robot', atomically reads and returns its name.
robotName :: Robot -> IO String
robotName = readMVar . name

-- | Generates a random robot name, such as RX837 or BC811.
randomName :: IO String
randomName = mapM randomRIO [a, a, d, d, d]
  where
    a = ('A', 'Z')
    d = ('0', '9')
