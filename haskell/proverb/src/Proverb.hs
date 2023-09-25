module Proverb
  ( recite,
  )
where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite inputs@(input : _)
  | null proverb = "And all for the want of a " <> input <> "."
  | otherwise = intercalate "\n" $ proverb ++ [recite [input]]
  where
    proverb =
      [ "For want of a " <> want <> " the " <> loss <> " was lost."
        | (want, loss) <- zip inputs (tail inputs)
      ]
