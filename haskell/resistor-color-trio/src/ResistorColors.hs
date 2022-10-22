module ResistorColors (Color (..), Resistor (..), label, ohms) where

import Control.Arrow ((>>>))
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
label resistor =
  case value `divMod` 1000000000 of
    (gigaohms, 0) | gigaohms > 0 -> mkLabel gigaohms "gigaohms"
    _ ->
      case value `divMod` 1000000 of
        (megaohms, 0) | megaohms > 0 -> mkLabel megaohms "megaohms"
        _ ->
          case value `divMod` 1000 of
            (kiloohms, 0) | kiloohms > 0 -> mkLabel kiloohms "kiloohms"
            _ -> mkLabel value "ohms"
  where
    value = ohms resistor
    mkLabel n unit = T.unwords [T.pack (show n), unit]

ohms :: Resistor -> Int
ohms = bands >>> \(x, y, zeros) -> (fromEnum x * 10 + fromEnum y) * (10 ^ fromEnum zeros)
