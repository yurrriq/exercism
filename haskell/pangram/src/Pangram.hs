module Pangram (isPangram) where

import qualified Data.Char as Char

isPangram :: String -> Bool
isPangram =
  flip all alphabet
    . flip elem
    . map Char.toLower

alphabet :: String
alphabet = ['a' .. 'z']
