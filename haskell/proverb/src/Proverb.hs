module Proverb
  ( recite,
  )
where

recite :: [String] -> String
recite [] = ""
recite [input] = "And all for the want of a " <> input <> "."
recite inputs@(input : _) =
  concatMap mkLine (zip inputs (tail inputs)) <> recite [input]
  where
    mkLine (want, loss) =
      "For want of a " <> want <> " the " <> loss <> " was lost.\n"
