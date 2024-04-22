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
{ name = "ride-request"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "free"
  , "functions"
  , "halogen-vdom"
  , "integers"
  , "js-uri"
  , "lists"
  , "lite-decode"
  , "maybe"
  , "monoid-extras"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "otp-reader"
  , "prelude"
  , "presto"
  , "presto-dom"
  , "profunctor-lenses"
  , "st"
  , "strings"
  , "tailrec"
  , "tracker"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-html"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
