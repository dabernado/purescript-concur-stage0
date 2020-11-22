{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-stage0"
, dependencies = [ "console", "effect", "psci-support", "concur-core", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
