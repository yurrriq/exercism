{ name = "triangle"
, dependencies = [ "effect", "either", "prelude", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
