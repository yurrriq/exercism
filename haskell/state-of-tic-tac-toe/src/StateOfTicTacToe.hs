{-# LANGUAGE LambdaCase #-}

module StateOfTicTacToe
  ( gameState,
    GameState (..),
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Lens (ifoldl')
import Data.Bits (popCount, shiftL, (.&.), (.|.))
import Text.Trifecta (Parser, Result (..), char, count, newline, parseString)

data GameState
  = WinX
  | WinO
  | Draw
  | Ongoing
  | Impossible
  deriving (Eq, Show)

data GamePlayer
  = X
  | O
  deriving (Eq, Show)

-- | Analyse a Tic Tac Toe game state.
gameState :: [String] -> GameState
gameState input = maybe Impossible go (parseStringMay (grid 3) (unlines input))
  where
    go (xs, os)
      | numOs > numXs || abs (numXs - numOs) > 1 || (wonX && wonO) = Impossible
      | wonX = WinX
      | wonO = WinO
      | xs .|. os == 511 = Draw
      | otherwise = Ongoing
      where
        numXs = popCount xs
        numOs = popCount os
        wonX = isWinner xs
        wonO = isWinner os

-- | Parse a k-by-k Tic Tac Toe board, representing the Xs and Os as bit maps.
grid :: Int -> Parser (Int, Int)
grid k = ifoldl' (ifoldl' . go) (0, 0) <$> count k (row <* newline)
  where
    go y x acc = \case
      Just X -> first (shiftL 1 (k * y + x) .|.) acc
      Just O -> second (shiftL 1 (k * y + x) .|.) acc
      Nothing -> acc
    row = count k square

-- | Parse a square maybe containing an X or an O.
square :: Parser (Maybe GamePlayer)
square =
  Just <$> (X <$ char 'X' <|> O <$ char 'O')
    <|> Nothing <$ char ' '

-- | Determine if a given bit map represents a Tic Tac Toe win.
isWinner :: Int -> Bool
isWinner turns = any (\win -> turns .&. win == win) waysToWin

-- | Ways to win at Tic Tac Toe: top row, middle row, bottom row, left column,
-- middle column, right column, falling diagonal, and rising diagonal.
waysToWin :: [Int]
waysToWin = [7, 56, 448, 73, 146, 292, 273, 84]

-- | Run the parser on the input and return 'Just' the result if successful.
parseStringMay :: Parser a -> String -> Maybe a
parseStringMay parser input =
  case parseString parser mempty input of
    Success result -> Just result
    Failure _ -> Nothing
