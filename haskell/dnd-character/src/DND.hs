{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : DND
-- Description : Generate D&D characters.
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

import Control.Monad (replicateM)
import Data.List (sort)
import Test.QuickCheck (Gen, chooseInt, discard, shuffle)

instance MonadFail Gen where
  fail = discard

-- | A character has six abilities and hit points.
data Character = Character
  { strength :: Int,
    dexterity :: Int,
    constitution :: Int,
    intelligence :: Int,
    wisdom :: Int,
    charisma :: Int,
    hitpoints :: Int
  }
  deriving (Show, Eq)

-- | Given an ability score, compute its modifier.
--
-- >>> modifier 3
-- -4
modifier :: Int -> Int
modifier con = (con - 10) `div` 2

-- | Generate a random ability score, i.e. roll 4d6, drop the lowest, and sum.
ability :: Gen Int
ability = sum . tail . sort <$> replicateM 4 (chooseInt (1, 6))

-- | Generate a random character.
character :: Gen Character
character =
  do
    rolls <- drop 2 . sort <$> replicateM 8 ability
    [str, dex, con, int, wis, cha] <- shuffle rolls
    pure $ Character str dex con int wis cha (10 + modifier con)
