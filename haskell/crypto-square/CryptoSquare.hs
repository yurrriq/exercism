module CryptoSquare
  ( ciphertext,
    plaintextSegments,
    normalizePlaintext,
    squareSize,
    normalizeCiphertext,
  )
where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

plaintextSegments :: String -> [String]
plaintextSegments = (chunksOf =<< squareSize) . normalizePlaintext

normalizePlaintext :: String -> String
normalizePlaintext = mapMaybe (iffAp isAlphaNum toLower)

squareSize :: String -> Int
squareSize = ceiling . (sqrt :: Double -> Double) . fromIntegral . length

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments

iff :: (a -> Bool) -> a -> Maybe a
iff = ap (bool Nothing . Just)

iffAp :: (a -> Bool) -> (a -> b) -> a -> Maybe b
iffAp p f x = iff p x >> (Just . f) x
