-- |
-- Module      : LuciansLusciousLasagna
-- Description : Cook a brilliant lasagna from a cooking book.
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module LuciansLusciousLasagna
  ( elapsedTimeInMinutes,
    expectedMinutesInOven,
    preparationTimeInMinutes,
  )
where

-- | The lasagna should be in the oven for 40 minutes.
--
-- >>> expectedMinutesInOven
-- 40
expectedMinutesInOven :: (Num a) => a
expectedMinutesInOven = 40
{-# SPECIALIZE INLINE expectedMinutesInOven :: Int #-}

-- | It takes 2 minutes to prepare each layer.
--
-- >>> preparationTimeInMinutes 3
-- 6
preparationTimeInMinutes :: (Num a) => a -> a
preparationTimeInMinutes = (2 *)
{-# SPECIALIZE INLINE preparationTimeInMinutes :: Int -> Int #-}

elapsedTimeInMinutes :: (Num a) => a -> a -> a
elapsedTimeInMinutes = (+) . preparationTimeInMinutes
{-# SPECIALIZE INLINE elapsedTimeInMinutes :: Int -> Int -> Int #-}
