{ name = "scrabble-score"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "test-unit"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
