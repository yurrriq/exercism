{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

module Bowling
  ( score,
    BowlingError (..),
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first, second)
import Data.Finite (Finite)
import Data.Fix (unfoldFixM)
import Data.Functor ((<&>))
import Data.Functor.Foldable (ListF (..), cata)
import Data.Ix (inRange)

data BowlingError
  = IncompleteGame
  | InvalidRoll
      { rollIndex :: Int,
        rollValue :: Roll
      }
  deriving (Eq, Show)

type Roll = Int

type Score = Finite 301

score :: [Int] -> Either BowlingError Int
score = fmap fromIntegral . score'

score' :: [Roll] -> Either BowlingError Score
score' rolls =
  unfoldFixM frameCoalgM (0, Just 0, rolls)
    >>= fmap fst . flip (cata scoreAlg) rolls

type Scoring = [Roll] -> Either BowlingError (Score, [Roll])

data Frame
  = Strike
  | Spare
  | Open Roll Roll
  | TenthFrame Roll Roll (Maybe Roll)
  deriving (Show)

type FrameF = ListF Frame

scoreAlg :: FrameF Scoring -> Scoring
scoreAlg Nil = Right . (0,)
scoreAlg (Cons frame next) =
  scoreFrame frame
    >=> bimap (+) next
    >>> uncurry (second . first)

scoreFrame :: Frame -> Scoring
scoreFrame Strike (10 : rolls@(x : y : _)) = Right (10 + fromIntegral (x + y), rolls)
scoreFrame Spare (_ : _ : rolls@(x : _)) = Right (10 + fromIntegral x, rolls)
scoreFrame (Open x y) (_ : _ : rolls) = Right (fromIntegral (x + y), rolls)
scoreFrame (TenthFrame x y Nothing) (_ : _ : rolls) = Right (fromIntegral (x + y), rolls)
scoreFrame (TenthFrame x y (Just z)) (_ : _ : _ : rolls) = Right (fromIntegral (x + y + z), rolls)
scoreFrame _ _ = Left IncompleteGame

type FrameParsing = Either BowlingError (ListF Frame FrameState)

type FrameState = (Int, Maybe (Finite 10), [Roll])

frameCoalgM :: FrameState -> FrameParsing
frameCoalgM (i, Just frame, rolls@(roll : _))
  | invalid roll = Left $ InvalidRoll i roll
  | frame < 9 = parseFrame (i, Just frame, rolls)
  | frame == 9 = parseTenthFrame (i, Nothing, rolls)
  | otherwise = Left $ InvalidRoll i roll
frameCoalgM (_, Nothing, []) = Right Nil
frameCoalgM (_, _, []) = Left IncompleteGame
frameCoalgM (i, _, roll : _) = Left $ InvalidRoll i roll

parseFrame :: FrameState -> FrameParsing
parseFrame (i, frame, 10 : rolls) =
  Right $ Cons Strike (i + 1, frame <&> (+ 1), rolls)
parseFrame (i, frame, x : y : zs)
  | invalid y = Left $ InvalidRoll (i + 1) y
  | invalid (x + y) = Left $ InvalidRoll (i + 1) y
  | x + y == 10 = Right $ Cons Spare (i + 2, frame <&> (+ 1), zs)
  | otherwise = Right $ Cons (Open x y) (i + 2, frame <&> (+ 1), zs)
parseFrame _ = Left IncompleteGame

parseTenthFrame :: FrameState -> FrameParsing
parseTenthFrame (_, _, [10, 10]) = Left IncompleteGame
parseTenthFrame (i, _, [x, y])
  | invalid (x + y) = Left $ InvalidRoll (i + 1) y
  | x + y == 10 = Left IncompleteGame
  | otherwise = Right $ Cons (TenthFrame x y Nothing) (i + 2, Nothing, [])
parseTenthFrame (i, _, x : y : z : rolls)
  | invalid y = Left $ InvalidRoll (i + 1) y
  | invalid z = Left $ InvalidRoll (i + 2) z
  | x + y == 10 = Right $ Cons (TenthFrame x y (Just z)) (i + 3, Nothing, rolls)
  | x == 10 =
      if
        | y /= 10 && y + z > 10 -> Left $ InvalidRoll (i + 2) z
        | null rolls -> Right $ Cons (TenthFrame x y (Just z)) (i + 3, Nothing, [])
        | otherwise -> Left $ InvalidRoll (i + 3) z
  | otherwise = Left $ InvalidRoll (i + 2) x
parseTenthFrame _ = Left IncompleteGame

-- | An invalid roll is outside the range \((0, 10)\).
invalid :: Roll -> Bool
invalid = not . inRange (0, 10)
