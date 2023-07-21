{-# LANGUAGE NumericUnderscores #-}

module Say
  ( inEnglish,
  )
where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = Nothing
  | n == 0 = Just "zero"
  | n > 999_999_999_999 = Nothing
  | otherwise =
      Just . unwords $
        map appendLabel . filter ((/= 0) . fst) $
          labelled
  where
    labelled = zip splits (drop (length labels - length splits) labels)
    labels = [Just "billion", Just "million", Just "thousand", Nothing]
    splits = splitThousands n
    appendLabel (x, Nothing) = sayU1000 x
    appendLabel (x, Just lbl) = sayU1000 x <> " " <> lbl

sayU1000 :: Integer -> String
sayU1000 n
  | n < 100 = sayU100 n
  | otherwise =
      case n `divMod` 100 of
        (q, 0) -> sayU100 q <> " hundred"
        (q, r) -> sayU100 q <> " hundred " <> sayU100 r

sayU100 :: Integer -> String
sayU100 n
  | n < 20 = sayU20 n
  | otherwise =
      case n `divMod` 10 of
        (q, 0) -> sayTens q
        (q, r) -> sayTens q <> "-" <> sayU10 r

sayU20 :: Integer -> String
sayU20 n
  | n < 10 = sayU10 n
  | otherwise = case n of
      10 -> "ten"
      11 -> "eleven"
      12 -> "twelve"
      13 -> "thirteen"
      15 -> "fifteen"
      18 -> "eighteen"
      _ -> sayU10 (n - 10) <> "teen"

sayU10 :: Integer -> String
sayU10 = (english !!) . pred . fromInteger
  where
    english = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

sayTens :: Integer -> String
sayTens = (english !!) . pred . pred . fromInteger
  where
    english = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

splitThousands :: Integer -> [Integer]
splitThousands = reverse . go
  where
    go n
      | n > 999 = let (q, r) = n `quotRem` 1_000 in r : go q
      | otherwise = [n]
