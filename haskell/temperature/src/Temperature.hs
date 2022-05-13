-- |
-- Module      : Temperature
-- Description : Convert temperatures between Celsius and Fahrenheit.
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module Temperature
  ( tempToC,
    tempToF,
  )
where

-- | Convert a temperature in Fahrenheit to its equivalent in Celsius.
--
-- >>> tempToC 32
-- 0.0
tempToC :: (Integral a, RealFrac b) => a -> b
tempToC temp = (fromIntegral temp - 32) / 1.8
{-# SPECIALIZE INLINE tempToC :: Integer -> Float #-}

-- | Convert a temperature in Celsius to its equivalent in Fahrenheit.
--
-- >>> tempToF 4
-- 40
tempToF :: (Integral a, RealFrac b) => b -> a
tempToF temp = ceiling (temp * 1.8 + 32)
{-# SPECIALIZE INLINE tempToF :: Float -> Integer #-}
