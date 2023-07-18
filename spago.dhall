{ name = "node-zlib"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "functions"
  , "node-buffer"
  , "node-streams"
  , "prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
}
