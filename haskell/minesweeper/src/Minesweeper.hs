module Minesweeper
  ( annotate,
  )
where

import Control.Lens (ifoldl')
import Data.Char (intToDigit)
import Data.Ix (Ix, inRange)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Linear (V2 (..))

annotate :: [String] -> [String]
annotate rowStrings =
  [ [unMineCount (annotatedBoard M.! V2 y x) | x <- [0 .. width - 1]]
    | y <- [0 .. height - 1]
  ]
  where
    annotatedBoard = M.foldrWithKey go board board
    board = mkBoard rowStrings
    go coords Nothing annotated =
      foldr (M.adjust (fmap succ)) annotated (neighborsOf' coords)
    go _ _ annotated = annotated
    neighborsOf' = neighborsInRange (pure 0, V2 (height - 1) (width - 1))
    height = length rowStrings
    width = length (head rowStrings)

mkBoard :: [String] -> Map (V2 Int) (Maybe Int)
mkBoard = ifoldl' (ifoldl' . go) M.empty
  where
    go y x minefield char = M.insert (V2 y x) (mineCount char) minefield

mineCount :: Char -> Maybe Int
mineCount '*' = Nothing
mineCount _ = Just 0

unMineCount :: Maybe Int -> Char
unMineCount Nothing = '*'
unMineCount (Just 0) = ' '
unMineCount (Just n) = intToDigit n

neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = S.filter (inRange range) (neighborsOf point)

neighborsOf :: (Applicative f, Num a, Num (f a), Ord (f a), Traversable f) => f a -> Set (f a)
neighborsOf = Set.fromList . flip map adjacencies . (+)

adjacencies :: (Applicative f, Num a, Eq (f a), Traversable f) => [f a]
adjacencies = filter (/= pure 0) $ sequenceA (pure [-1, 0, 1])
