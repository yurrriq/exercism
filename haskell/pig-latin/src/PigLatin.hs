module PigLatin
  ( translate,
  )
where

import Data.Char (toLower)

translate :: String -> String
translate = unwords . map go . words
  where
    go word@('x' : 'r' : _) = word <> "ay"
    go word@('y' : 't' : _) = word <> "ay"
    go word =
      case span isConsonant word of
        ("q", 'u' : rest) -> rest <> "quay"
        (consonants@(_ : "q"), 'u' : rest) -> rest <> consonants <> "uay"
        ("", 'y' : rest) -> rest <> "yay"
        (consonants, rest@('y' : _)) -> rest <> consonants <> "ay"
        (consonants, rest) -> rest <> consonants <> "ay"

isConsonant :: Char -> Bool
isConsonant = (`elem` ("bcdfghjklmnpqrstvwxz" :: String)) . toLower
