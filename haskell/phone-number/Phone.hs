-- |
-- Module      : Phone
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Sanitizing user-entered phone numbers.
module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

-- | Given a 'String' representing a phone number, sanitizes it by calling
-- 'number' on it and 'take's the first 3 characters.
areaCode :: String -> String
areaCode = take 3 . number

-- | Given a 'String' representing a phone number, attempts to sanitize it
-- and return a 'String' of only digits, representing the area code, exchange
-- and subscriber number. If the given phone number is invalid,
-- returns @"0000000000"@.
number :: String -> String
number x
  | numDigits == 10 = digits
  | numDigits == 11 && head x == '1' = tail digits
  | otherwise = "0000000000"
  where
    digits = filter isDigit x
    numDigits = length digits

-- | Given a 'String' representing a phone number, calls 'number' on it and
-- formats the result, returning a 'String'.
prettyPrint :: String -> String
prettyPrint =
  ( \(area, rest) ->
      "("
        ++ area
        ++ ") "
        ++ ((\(prefix, line) -> prefix ++ "-" ++ line) . splitAt 3) rest
  )
    . splitAt 3
    . number
