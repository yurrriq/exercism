module Triangle where

import Control.Monad (ap)

data TriangleType = Illogical | Equilateral | Isosceles | Scalene
  deriving (Eq, Show)

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

-- http://stackoverflow.com/a/12872133
picks :: [a] -> [(a, [a])]
picks [] = []
picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]
