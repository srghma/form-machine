let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall
        sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220224/packages.dhall
        sha256:67cc3d4f0e8fb72bb1413ba94ddd72a3ceb0783eb725e3b22ad7568b3b581163

in  upstream
  with polyform =
      mkPackage
        [ "newtype"
        , "ordered-collections"
        , "variant"
        , "profunctor"
        , "invariant"
        , "foreign-object"
        , "run"
        , "transformers"
        , "validation"
        , "foreign"
        ]
        "https://github.com/purescript-polyform/polyform.git"
        "v0.9.0"
  with polyform-batteries-core = ../batteries-core/spago.dhall as Location
  with polyform-batteries-env = ../batteries-env/spago.dhall as Location
  with polyform-batteries-json = ../batteries-json/spago.dhall as Location
  with polyform-batteries-urlencoded =
      ../batteries-urlencoded/spago.dhall as Location
