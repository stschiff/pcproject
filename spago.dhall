{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "pcproject"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "maybe"
  , "prelude"
  , "stringutils"
  , "tuples"
  , "uint"
  , "web-encoding"
  , "web-events"
  , "web-file"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
