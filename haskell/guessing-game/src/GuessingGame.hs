-- |
-- Module      : GuessingGame
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- A number guessing game.
module GuessingGame
  ( reply,
  )
where

-- | Reply to a 'guess', responding differently depending on how close the guess
-- was to the chosen number (@42@).
reply :: Int -> String
reply guess
  | guess == 42 = "Correct"
  | guess == 41 || guess == 43 = "So close"
  | guess < 41 = "Too low"
  | otherwise = "Too high"
