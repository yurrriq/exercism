module Allergies where

import           Data.Bits (testBit)
-- import           Control.Monad (ap)
-- import           Data.Bits     (shiftL, testBit, (.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Bounded, Enum, Eq, Show)

allergens :: [Allergen]
allergens = [(minBound :: Allergen) ..]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = flip testBit . fromEnum
-- isAllergicTo = isAllergicTo' . allergenToInt
--   where isAllergicTo' = flip $ ap (==) . flip (.&.)
--         allergenToInt = shiftL 1 . fromEnum

allergies :: Int -> [Allergen]
allergies = (`filter` allergens) . flip isAllergicTo
