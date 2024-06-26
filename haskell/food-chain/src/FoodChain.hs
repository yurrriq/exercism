module FoodChain
  ( song,
  )
where

import Data.List (intercalate)

song :: String
song =
  intercalate
    "\n"
    [ stanza ["fly"],
      stanza ["spider", "fly"],
      stanza ["bird", "spider", "fly"],
      stanza ["cat", "bird", "spider", "fly"],
      stanza ["dog", "cat", "bird", "spider", "fly"],
      stanza ["goat", "dog", "cat", "bird", "spider", "fly"],
      stanza ["cow", "goat", "dog", "cat", "bird", "spider", "fly"],
      stanza ["horse"]
    ]

stanza :: [String] -> String
stanza [] = []
stanza (food : foods) = unlines $ firstLine food <> go (food : foods)
  where
    go ["fly"] = ["I don't know why she swallowed the fly. Perhaps she'll die."]
    go (this : that : rest)
      | that == "spider" = "She swallowed the " <> this <> " to catch the spider that wriggled and jiggled and tickled inside her." : go (that : rest)
      | otherwise = "She swallowed the " <> this <> " to catch the " <> that <> "." : go (that : rest)
    go _ = []

firstLine :: String -> [String]
firstLine "fly" =
  ["I know an old lady who swallowed a fly."]
firstLine "spider" =
  [ "I know an old lady who swallowed a spider.",
    "It wriggled and jiggled and tickled inside her."
  ]
firstLine "bird" =
  [ "I know an old lady who swallowed a bird.",
    "How absurd to swallow a bird!"
  ]
firstLine "cat" =
  [ "I know an old lady who swallowed a cat.",
    "Imagine that, to swallow a cat!"
  ]
firstLine "dog" =
  [ "I know an old lady who swallowed a dog.",
    "What a hog, to swallow a dog!"
  ]
firstLine "horse" =
  [ "I know an old lady who swallowed a horse.",
    "She's dead, of course!"
  ]
firstLine "cow" =
  [ "I know an old lady who swallowed a cow.",
    "I don't know how she swallowed a cow!"
  ]
firstLine "goat" =
  [ "I know an old lady who swallowed a goat.",
    "Just opened her throat and swallowed a goat!"
  ]
firstLine _ = undefined
