-- |
-- Module      : Cipher
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- The Vigenère cipher using lowercase letters.
module Cipher
  ( caesarDecode,
    caesarEncode,
    caesarEncodeRandom,
  )
where

import Control.Monad.Random (getRandomRs)
import Data.Char (chr, ord)

-- | Perform [Vigenère](https://en.wikipedia.org/wiki/Vigenère_cipher)
-- decryption with a given key and ciphertext.
caesarDecode :: String -> String -> String
caesarDecode = cipher negate

-- | Perform [Vigenère](https://en.wikipedia.org/wiki/Vigenère_cipher)
-- encryption with a given key and message.
caesarEncode :: String -> String -> String
caesarEncode = cipher id

-- | Perform [Vigenère](https://en.wikipedia.org/wiki/Vigenère_cipher)
-- encryption with a pseudo-random 100-character key.
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text =
  do
    key <- take 100 <$> getRandomRs ('a', 'z')
    pure (key, caesarEncode key text)

-- | A [Vigenère cipher](https://en.wikipedia.org/wiki/Vigenère_cipher) with a
-- given @key@ and @text@, precomposing @f@ with 'shift'.
--
-- @
--     'caesarEncode' = cipher id
--     'caesarDecode' = cipher negate
-- @
cipher :: (Int -> Int) -> String -> String -> String
cipher _ _ "" = ""
cipher _ "" text = text
cipher f key text = zipWith (shift . f) offsets text
  where
    offsets = cycle (map letterToInt key)

-- | Shift a lowercase letter by @n@ letters.
--
-- > shift 3 'a' == 'd'
-- > shift 2 'z' == 'b'
shift :: Int -> Char -> Char
shift n c = intToLetter ((letterToInt c + n) `mod` 26)

-- | Convert a lowercase letter to an int within @[0, 25]@.
letterToInt :: Char -> Int
letterToInt c = ord c - 97

-- | Convert an int within @[0, 25]@ to a lowercase letter.
intToLetter :: Int -> Char
intToLetter n = chr (97 + n)
