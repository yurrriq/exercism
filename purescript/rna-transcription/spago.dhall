{ name = "rna-transcription"
, dependencies =
  [ "effect"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
