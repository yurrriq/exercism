{-# LANGUAGE LambdaCase #-}

module House where

import Data.Function.Pointless ((.:))
import Data.List (tails)

rhyme :: String
rhyme =
  unlines $
    map strophe $
      (reverse . init . tails)
        [ ["horse and the hound and the horn", "belonged to"],
          ["farmer sowing his corn", "kept"],
          ["rooster that crowed in the morn", "woke"],
          ["priest all shaven and shorn", "married"],
          ["man all tattered and torn", "kissed"],
          ["maiden all forlorn", "milked"],
          ["cow with the crumpled horn", "tossed"],
          ["dog", "worried"],
          ["cat", "killed"],
          ["rat", "ate"],
          ["malt", "lay in"],
          ["house", "Jack built"]
        ]

strophe :: [[String]] -> String
strophe = thisIs . (`go` ".\n")
  where
    go :: [[String]] -> ShowS
    go = \case
      ([n, v] : []) -> the n . space . that v
      ([n, v] : t) -> the n . lineBreak . that v . go t
      _ -> undefined

    thisIs :: ShowS
    thisIs = ("This is" ++)
    space :: ShowS
    space = (" " ++)
    lineBreak :: ShowS
    lineBreak = ("\n" ++)
    the :: String -> ShowS
    the = (" the " ++) .: (++)
    that :: String -> ShowS
    that = ("that " ++) .: (++)
