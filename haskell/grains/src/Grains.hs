{-# LANGUAGE CPP #-}

-- |
-- Module      : Grains
-- Copyright   : (c) Eric Bailey, 2015-2023
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
  )
where

-- |
-- Given the number of a square on a chess board, where one grain is placed on
-- the first square and the number of grains on each subsequent square doubles,
-- return the number of grains of wheat on a given square.
square :: Integer -> Maybe Integer
#ifdef RECURSIVE
square 1 = Just 1
square n
  | n >= 1 && n <= 64 = Just . (2 *) <$> square' (n - 1)
  | otherwise = Nothing

square' :: Integer -> Integer
square' = (2 *) . square . pred
# else
square n
  | n >= 1 && n <= 64 = Just (square' n)
  | otherwise = Nothing

square' :: Integer -> Integer
square' = (2 ^) . pred
#endif

-- | Return the total number of grains on the entire chessboard.
total :: Integer
#ifdef RECURSIVE
total = fromJust (sum (mapM square [1..64])
#else
total = pred (square' 65)
#endif
