{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Affine
  ( decode,
    encode,
  )
where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.Char (chr, isDigit, isLetter, ord, toLower)
import Data.Functor (($>), (<&>))
import Data.List.Extra (chunksOf)
import Data.Mod (invertMod)

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText =
  invertMod @26 (fromIntegral a) <&> \a' ->
    cipherText >>= \c ->
      decodeLetter (fromEnum a', b) c
        <|> (guard (isDigit c) $> c)

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText =
  guard (gcd a 26 == 1)
    $> ( unwords . chunksOf 5 $
           do
             c <- plainText
             encodeLetter (a, b) c <|> handleDigit c
       )

decodeLetter :: (Alternative t) => (Int, Int) -> Char -> t Char
decodeLetter (a', b) c =
  guard (isLetter c)
    $> toLetter (fromEnum a' * (letterValue c - b))

encodeLetter :: (Alternative t) => (Int, Int) -> Char -> t Char
encodeLetter (a, b) c =
  guard (isLetter c)
    $> toLetter (a * letterValue c + b)

handleDigit :: (Alternative t) => Char -> t Char
handleDigit c = guard (isDigit c) $> c

toLetter :: Int -> Char
toLetter n = chr (97 + n `mod` 26)

letterValue :: Char -> Int
letterValue = subtract 97 . ord . toLower
