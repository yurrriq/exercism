{ name = "phone-number"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "strings"
  , "test-unit"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
