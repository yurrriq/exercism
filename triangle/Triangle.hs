{-|
Module      : Triangle
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Determining if a triangle is illogical, equilateral, isosceles, or scalene.
-}
module Triangle (TriangleType(..), triangleType) where

import Control.Monad (ap)

-- | Qualifying triangles.
data TriangleType =
  -- | Fails the
  -- <https://en.wikipedia.org/wiki/Triangle_inequality triangle inequality>.
  --
  -- <<illogical.png>>
  Illogical
  -- | All sides are equal.
  --
  -- <<equilateral.png>>
  | Equilateral
  -- | Two sides are equal.
  --
  -- <<isosceles.png>>
  | Isosceles
  -- | Passes the
  -- <https://en.wikipedia.org/wiki/Triangle_inequality triangle inequality>.
  --
  -- <<scalene.png>>
  | Scalene
  deriving (Eq, Show)

-- | Given thre sides, returns a 'TriangleType' describing the quality of the
-- given triangle.
triangleType :: Int -> Int -> Int -> TriangleType
triangleType x y z
  | isIllogical   = Illogical
  | isEquilateral = Equilateral
  | isIsosceles   = Isosceles
  | otherwise     = Scalene
  where isIllogical   = any (uncurry ((. sum) . (>=))) picked
        isEquilateral = and . ap (zipWith (==)) tail $ sides
        isIsosceles   = any (uncurry elem) picked
        picked        = picks sides
        sides         = [x,y,z]

-- | Given a list, returns a list of pairs of each element in the list,
-- and a list of every other element.
-- See also:
-- <http://stackoverflow.com/a/12872133 Answer on Stack Overflow>
picks :: [a] -> [(a, [a])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]
