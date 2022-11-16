module RotationalCipher
  ( rotate,
  )
where

import Data.Char (chr, isAlpha, isLower, ord)

-- | Rotate a string's letters by a given number of letters.
--
-- > rotate 5 "omg" == "trl"
-- > rotate 0 "c" == "c"
-- > rotate 26 "Cool" == "Cool"
-- > rotate 13 "The quick brown fox jumps over the lazy dog." == "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
rotate :: Int -> String -> String
rotate n
  | n `mod` 26 == 0 = id
  | otherwise = map (shift n)

-- | Shift a letter by a specified number of letters.
--
-- > shift 3 'a' == 'd'
-- > shift 2 'z' == 'b'
-- > shift 8 'E' == 'M'
-- > shift 13 '.' == '.'
shift :: Int -> Char -> Char
shift n c
  | isAlpha c =
    if isLower c
      then shift' 97 n c
      else shift' 65 n c
  | otherwise = c

-- | Shift a letter by a specified number of letters.
--
-- Use @shift' 97@ for lowercase and @shift' 65@ for uppercase letters,
-- since @ord ''a'' == 97@ and @ord ''A'' == 65@.
shift' :: Int -> Int -> Char -> Char
shift' ordA n c = chr (ordA + ((ord c - ordA + n) `mod` 26))
