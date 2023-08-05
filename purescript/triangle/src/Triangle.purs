module Triangle
  ( Triangle(Equilateral, Isosceles, Scalene)
  , triangleKind
  ) where

import Prelude
import Data.Either (Either(..))

-- import Data.Generic.Rep (class Generic)
-- import Data.Show.Generic (genericShow)

data Triangle
  = Equilateral
  | Isosceles
  | Scalene

derive instance eqTriangle :: Eq Triangle

-- derive instance genericTriangle :: Generic Triangle _

-- instance showTriangle :: Show Triangle where
--   show = genericShow

instance showTriangle :: Show Triangle where
  show Equilateral = "Equilateral"
  show Isosceles = "Isosceles"
  show Scalene = "Scalene"

triangleKind :: Int -> Int -> Int -> Either String Triangle
triangleKind a b c
  | a <= 0 || b <= 0 || c <= 0 =
      Left "Invalid lengths"
  | a == b && b == c =
      Right Equilateral
  | a + b < c || a + c < b || b + c < a =
      Left "Violates inequality"
  | a == b || a == c || b == c =
      Right Isosceles
  | otherwise =
      Right Scalene
