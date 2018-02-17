{-# LANGUAGE LambdaCase #-}

{-|
Module      : Matrix
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : stable
Portability : portable

Creating and manipulating 2D matrices.
-}

module Matrix ( Matrix{-(..}-}, Shape, empty, matrix, fromString, fromList
              , rows, cols, shape, row, column
              , flatten, reshape, transpose)
       where

import           Control.Arrow ((&&&))
import           Control.Monad (ap)
-- import           Data.List.Split (chop)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

-- | A 2D matrix.
data Matrix a = Matrix
  { cells :: Vector a -- ^ A flat vector of cells.
  , rows  :: Int      -- ^ The number of rows.
  , cols  :: Int      -- ^ The number of columns.
  }
  deriving (Eq, Show)

-- | A 'Shape' is a pair representing the number of 'rows' and 'cols'
-- in a 'Matrix'.
type Shape = (Int, Int)

-- | The empty 'Matrix'.
empty :: Matrix a
empty = matrix (0, 0) V.empty

-- | Given a 'Shape' and a vector of cells, create and return a 'Matrix'.
matrix :: Shape -> Vector a -> Matrix a
matrix (x, y) v
  | x * y == V.length v = Matrix { cells = v, rows = x, cols = y }
  | otherwise          = error "Incorrect number of cells."

-- | Given a string representing a 'Matrix', with rows delimited by newlines and
-- columns delimited by whitespace, create and return the 'Matrix'.
fromString :: Read a => String -> Matrix a
fromString = fromList . map readRow . lines
  where readRow = (\case [] -> []; (x, xs):_ -> x : readRow xs) . reads
-- chop :: ([a] -> (b, [a])) -> [a] -> [b]

-- | Given a list of rows (lists of items), create and return a 'Matrix'.
fromList :: [[a]] -> Matrix a
fromList []         = empty
fromList ([]:_)     = empty
fromList xxs@(xs:_) = matrix (length xxs, length xs) $
                      V.fromList (concat xxs)

-- | Given a 'Matrix' @m@, return @(@'rows'@ m, @'cols'@ m)@.
shape :: Matrix a -> Shape
shape = rows &&& cols

-- | Given an index @n@ and a 'Matrix', return the vector of items at row @n@ in
-- the given matrix.
row :: Int -> Matrix a -> Vector a
row n = (dice . cells) `ap` cols
  where dice = flip (V.slice =<< (n *))
-- where dice v x = V.slice (n * x) x v
-- row n m = (\x -> V.slice (n * x) x $ cells m) (cols m)

-- | Given an index @n@ and a 'Matrix', return the vector of itmes at column @n@
-- in the given matrix.
column :: Int -> Matrix a -> Vector a
column n m = takeNth (cols m) (V.drop n (cells m))

-- | Given a 'Matrix', return a copy, rotated 90° to the right.
transpose :: Matrix a -> Matrix a
transpose = go =<< cols
  where go n m = matrix (n, rows m) $
                 V.concatMap (`column` m) $
                 V.fromList [0..pred n]


-- | Given a 'Shape' @s@ and a 'Matrix', return 'matrix'@ s (@'cells'@ m)@.
reshape :: Shape -> Matrix a -> Matrix a
reshape = (. cells) . matrix

-- | Given a 'Matrix', return its 'cells'.
flatten :: Matrix a -> Vector a
flatten = cells

-- | Return a vector of every nth element of a given vector.
--
-- See also: <http://yurrriq.codes/haxkell/clojure-list/Data-List-Clojure.html#v:takeNth Data.List.Clojure.takeNth>
takeNth :: Int -> Vector a -> Vector a
takeNth n v | V.null v  = V.empty
            | otherwise = V.head v `V.cons` takeNth n (V.drop n v)
