{ name = "anagram"
, dependencies = [ "arrays", "effect", "prelude", "strings", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
