{-|
Module      : Robot
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Managing robot factory settings.
-}
module Robot (Robot, mkRobot, resetName, robotName) where

import Control.Concurrent.STM
import Control.Monad (liftM)
import Data.Char (isDigit)
import System.Random (randomRIO)

-- | A robot has a name that can be read ('robotName') and reset ('resetName').
data Robot = Robot { name :: TVar String }

-- | Creates a 'Robot' and gives it a random name.
mkRobot :: IO Robot
mkRobot = liftM Robot $ randomName >>= atomically . newTVar

-- | Given a 'Robot', generates a randome name and atomically overwrites the
-- given 'Robot''s name.
resetName :: Robot -> IO ()
resetName = (randomName >>=) . resetName'
  where resetName' :: Robot -> String -> IO ()
        resetName' = atomically .: (writeTVar . name)

-- | Given a 'Robot', atomically reads and returns its name.
robotName :: Robot -> IO String
robotName = atomically . readTVar . name

randomChar :: Char -> IO Char
randomChar x
  | isDigit x = randomRIO ('0','9')
  | otherwise = randomRIO ('A','Z')

-- | Generates a random robot name, such as RX837 or BC811.
randomName :: IO String
randomName = mapM randomChar "AA000"

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
