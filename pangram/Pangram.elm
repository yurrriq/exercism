module Pangram exposing (isPangram)

import String

isPangram : String -> Bool
isPangram sentence =
  let
    sentenceLower : String
    sentenceLower = String.toLower sentence
    usesLetter : Char -> Bool
    usesLetter = flip containsChar sentenceLower
  in
    String.all usesLetter alphabet

containsChar : Char -> String -> Bool
containsChar = String.contains << String.fromChar

alphabet : String
alphabet = "abcdefghijklmnopqrstuvwxyz"
