{-# LANGUAGE LambdaCase #-}
{-|
Module      : Queens
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Determining if two queens on a chess board can attack each other.
-}

module Queens (boardString, canAttack) where

import           Data.Function           (on)
import           Data.Function.Pointless ((.:))

type Position = (Int, Int)

boardString :: Maybe Position -> Maybe Position -> String
boardString w b = concat [[square (x,y), ws y] | x <- [0..7], y <- [0..7]]
  where
    square pos
      | Just pos == w = 'W'
      | Just pos == b = 'B'
      | otherwise    = '_'
    ws = \case 7 -> '\n'; _ -> ' '

canAttack :: Position -> Position -> Bool
canAttack a b = any (\f -> f a b) [sameRow, sameCol, sameDiag]
  where
    sameCol = (==) `on` snd
    sameDiag c d = go fst c d == go snd c d where go f = abs .: (-) `on` f
    sameRow = (==) `on` fst
