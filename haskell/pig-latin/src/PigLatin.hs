{-# LANGUAGE LambdaCase #-}

module PigLatin
  ( translate,
  )
where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word@('x' : 'r' : _) = word <> "ay"
translateWord word@('y' : 't' : _) = word <> "ay"
translateWord word =
  case break (`elem` vowels) word of
    ("q", 'u' : rest) -> rest <> "quay"
    (consonants@(_ : "q"), 'u' : rest) -> rest <> consonants <> "uay"
    ("", 'y' : rest) -> rest <> "yay"
    (consonants, rest@('y' : _)) -> rest <> consonants <> "ay"
    (consonants, rest) -> rest <> consonants <> "ay"

vowels :: [Char]
vowels = "aeiouy"
