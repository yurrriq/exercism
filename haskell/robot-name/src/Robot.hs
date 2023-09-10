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

import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVarIO,
    writeTVar,
  )
import Control.Monad.State (StateT, gets, liftIO, modify)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- | A robot has a name that can be read ('robotName') and reset ('resetName').
newtype Robot = Robot {_robotName :: TVar String}

type RunState = Set String

initialState :: RunState
initialState = Set.empty

type RobotFactory a = StateT RunState IO a

-- | Creates a 'Robot' and gives it a random name.
mkRobot :: RobotFactory Robot
mkRobot =
  do
    name <- randomName
    robot <- Robot <$> liftIO (newTVarIO name)
    modify (Set.insert name)
    pure robot

-- | Given a 'Robot' @r@, generates a random name and atomically overwrites
-- @r@'s name.
resetName :: Robot -> RobotFactory ()
resetName = (randomName >>=) . resetName'
  where
    resetName' = (liftIO . atomically) .: (writeTVar . _robotName)

-- | Given a 'Robot', atomically reads and returns its name.
robotName :: Robot -> IO String
robotName = readTVarIO . _robotName

-- | Generates a random robot name, such as RX837 or BC811.
randomName :: RobotFactory String
randomName =
  do
    isUsed <- gets (flip Set.member)
    name <- mapM randomRIO [a, a, d, d, d]
    if isUsed name
      then randomName
      else pure name
  where
    a = ('A', 'Z')
    d = ('0', '9')

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
