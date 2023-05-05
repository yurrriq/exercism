module Allergies where

import Data.Bits (testBit)

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Bounded, Enum, Eq, Show)

allergens :: [Allergen]
allergens = [minBound ..]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = flip testBit . fromEnum

allergies :: Int -> [Allergen]
allergies = (`filter` allergens) . flip isAllergicTo
