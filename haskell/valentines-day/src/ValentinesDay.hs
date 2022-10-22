module ValentinesDay
  ( Approval (..),
    Cuisine (..),
    Genre (..),
    Activity (..),
    rateActivity,
  )
where

import Prelude hiding (Maybe)

data Approval
  = Yes
  | No
  | Maybe
  deriving (Eq, Show)

data Cuisine
  = Korean
  | Turkish
  deriving (Eq, Show)

data Genre
  = Crime
  | Horror
  | Romance
  | Thriller
  deriving (Eq, Show)

data Activity
  = BoardGame
  | Chill
  | Movie Genre
  | Restaurant Cuisine
  | Walk Int
  deriving (Eq, Show)

rateActivity :: Activity -> Approval
rateActivity (Movie Romance) = Yes
rateActivity (Restaurant Korean) = Yes
rateActivity (Restaurant Turkish) = Maybe
rateActivity (Walk distance) = rateWalkDistance distance
rateActivity _ = No

rateWalkDistance :: Int -> Approval
rateWalkDistance distance
  | distance < 3 = Yes
  | distance <= 5 = Maybe
  | otherwise = No
