module Phone where

import Data.Char (isDigit)

areaCode :: String -> String
areaCode = take 3 . number

number :: String -> String
number x
  | numDigits == 10                  = digits
  | numDigits == 11 && head x == '1' = tail digits
  | otherwise                        = "0000000000"
  where digits    = filter isDigit x
        numDigits = length digits

prettyPrint :: String -> String
prettyPrint = (\ (area, rest) ->
                 "(" ++ area ++ ") " ++
                 ((\(prefix, line) -> prefix ++ "-" ++ line) . splitAt 3) rest) .
              splitAt 3 . number
