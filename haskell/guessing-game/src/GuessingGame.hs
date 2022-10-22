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
reply 42 = "Correct"
reply guess
  | guess < 41 = "Too low"
  | guess > 43 = "Too high"
  | otherwise = "So close"
