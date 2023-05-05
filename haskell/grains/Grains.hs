-- -*- compile-command: "runhaskell grains_test.hs" -*-

-- |
-- Module      : Grains
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Calculating the number of grains of wheat on a chessboard,
-- given that the number on each square doubles.
module Grains
  ( square,
    total,

    -- ** Recursive Alternatives
    -- $recursive
  )
where

-- |
-- Given the number of a square on a chess board, where one grain is placed on
-- the first square and the number of grains on each subsequent square doubles,
-- returns the number of grains of wheat on a given square.
square :: Integer -> Integer
square = (2 ^) . subtract 1

-- | Returns the total number of grains on the entire chessboard.
total :: Integer
total = pred $ square 65

-- $recursive
-- @
-- square' :: Integer -> Integer
-- square' 1 = 1
-- square' x = 2 * square' (x - 1)
-- @
-- @
-- total' :: Integer
-- total'    = sum (map square' [1..64])
-- @
