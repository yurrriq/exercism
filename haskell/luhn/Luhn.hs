module Luhn
  ( create,
    isValid,
    checkDigit,
    checksum,
    addends,
  )
where

import Data.Bool (bool)
import Data.Char (digitToInt)

{-
-- ====================================================================
--                          Generic imports
-- ====================================================================

import           Data.Bits  (Bits, toIntegralSized)
import           Data.Maybe (fromJust)
-}

-- ====================================================================
--                       Integer implementation
-- ====================================================================

create :: Integer -> Integer
create x = bool (tenx + 10 - cksum) tenx (cksum == 0)
  where
    cksum = rem10 (checksum tenx)
    tenx = x * 10

isValid :: Integer -> Bool
isValid = (10 `divides`) . checksum

checkDigit :: Integer -> Integer
checkDigit = toInteger . digitToInt . last . show

checksum :: Integer -> Integer
checksum = toInteger . rem10 . sum . digits

digits :: Integer -> [Integer]
digits =
  map sumDigits
    . zipWith (*) (cycle [1, 2])
    . map (toInteger . digitToInt)
    . reverse
    . show

addends :: Integer -> [Integer]
addends = reverse . digits

{-
-- ====================================================================
--                       Generic implementation
-- ====================================================================

create :: (Integral a, Bits a, Show a) => a -> a
create x = bool (tenx + 10 - cksum) tenx (cksum == 0)
  where cksum = rem10 (checksum tenx)
        tenx  = x * 10

isValid :: (Integral a, Bits a, Show a) => a -> Bool
isValid = (10 `divides`) . checksum

checkDigit :: (Bits a, Integral a, Show a) => a -> a
checkDigit = toIntegral . digitToInt . last . show

checksum :: (Integral a, Bits a, Show a) => a -> a
checksum = toIntegral . rem10 . sum . digits

digits :: (Integral a, Bits a, Show a) => a -> [a]
digits = map sumDigits .
         zipWith (*) (cycle [1,2]) .
         map (toIntegral . digitToInt) .
         reverse . show

addends :: (Integral a, Bits a, Show a) => a -> [a]
addends = reverse . digits

-- ====================================================================
--                      Generic helper function
-- ====================================================================

toIntegral :: (Integral a, Integral b, Bits a, Bits b) => a -> b
toIntegral = fromJust . toIntegralSized
-}

-- ====================================================================
--                          Helper functions
-- ====================================================================

sumDigits :: Integral a => a -> a
sumDigits = uncurry (+) . (`divMod` 10)

-- | Given two numbers, returns @True@ if the first divides the second,
-- otherwise @False@.
divides :: Integral a => a -> a -> Bool
d `divides` n = n `rem` d == 0

rem10 :: Integral a => a -> a
rem10 = (`rem` 10)
