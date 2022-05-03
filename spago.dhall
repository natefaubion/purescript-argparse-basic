{ name = "argparse-basic"
, license = "MIT"
, repository = "https://github.com/natefaubion/purescript-argparse-basic.git"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "record"
  , "strings"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
