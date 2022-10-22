module ResistorColors
  ( Color (..),
    Resistor (..),
    label,
    ohms,
  )
where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor
  { bands :: (Color, Color, Color)
  }
  deriving (Show)

label :: Resistor -> Text
label = labelOhms . ohms

ohms :: Resistor -> Int
ohms (Resistor (tens, ones, zeros)) =
  (10 * fromEnum tens + fromEnum ones) * (10 ^ fromEnum zeros)

labelOhms :: Int -> Text
labelOhms 0 = "0 ohms"
labelOhms n =
  mkLabel
    . fromMaybe (n, "ohms")
    . listToMaybe
    $ mapMaybe go labels
  where
    go (divisor, unit) =
      case n `divMod` divisor of
        (n', 0) -> Just (n', unit)
        _ -> Nothing

labels :: [(Int, Text)]
labels =
  [ (1000000000, "gigaohms"),
    (1000000, "megaohms"),
    (1000, "kiloohms")
  ]

mkLabel :: (Int, Text) -> Text
mkLabel (n', unit) = T.unwords [T.pack (show n'), unit]
