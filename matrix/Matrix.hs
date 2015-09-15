{-# LANGUAGE LambdaCase #-}

module Matrix ( Matrix, fromString, fromList
              , rows, cols, row, column, shape
              , flatten, reshape, transpose)
       where

import           Control.Arrow ((&&&))
import           Control.Monad (ap)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

data Matrix a = Matrix
  { cells :: Vector a
  , rows  :: Int
  , cols  :: Int
  }
  deriving (Eq, Show)

matrix :: (Int, Int) -> Vector a -> Matrix a
matrix (x, y) v
  | x * y == V.length v = Matrix { cells = v, rows = x, cols = y }
  | otherwise          = error "Incorrect number of cells."

empty :: Matrix a
empty = matrix (0, 0) V.empty

-- TODO: redo
fromString :: Read a => String -> Matrix a
fromString = fromList . map readRow . lines
  where readRow = (\case [] -> []; (x, xs):_ -> x : readRow xs) . reads

fromList :: [[a]] -> Matrix a
fromList []         = empty
fromList ([]:_)     = empty
fromList xxs@(xs:_) = matrix (length xxs, length xs) $
                      V.fromList (concat xxs)

row :: Int -> Matrix a -> Vector a
row n = (flip (V.slice =<< (n *)) . cells) `ap` cols
-- row n m = (\x -> V.slice (n * x) x $ cells m) (cols m)

column :: Int -> Matrix a -> Vector a
column n m = takeNth (cols m) (V.drop n (cells m))

-- TODO: cleanup
-- NOTE: this is NOT efficient
transpose :: Matrix a -> Matrix a
transpose = go =<< cols
  where go n m = matrix (n, rows m) $
                 V.concatMap (`column` m) $
                 V.fromList [0..pred n]
-- transpose m = (matrix . flip (,) (rows m)) `ap`
--               (V.concatMap (`column` m) . V.fromList . enumFromTo 0 . pred) $
--               cols m
-- transpose m = (\n ->
--                  matrix (n, rows m) $
--                  V.concatMap (`column` m) (V.fromList [0..pred n]))
--               (cols m)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape = (. cells) . matrix

flatten :: Matrix a -> Vector a
flatten = cells

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols -- <3 that

-- | Return a list of every nth element of a given list.
takeNth :: Int -> Vector a -> Vector a
takeNth n v | V.null v  = V.empty
            | otherwise = V.head v `V.cons` takeNth n (V.drop n v)




-- NOTE: Data.Vector.backpermute
