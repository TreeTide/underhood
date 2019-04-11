package(default_visibility = ["//visibility:public"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
)

# Expand below list on-demand. Need to add to //:ghc.nix as well.

# GHC-bundled libraries
haskell_import(name = "base")

haskell_import(name = "bytestring")

haskell_import(name = "clock")

haskell_import(name = "containers")

haskell_import(name = "criterion")

haskell_import(name = "mtl")

haskell_import(name = "text")

haskell_import(name = "time")

haskell_import(name = "transformers")

# Non-GHC-bundled libraries
haskell_import(name = "aeson")

haskell_import(name = "align")

haskell_import(name = "async")

haskell_import(name = "base64-bytestring")

haskell_import(name = "boxes")

haskell_import(name = "conduit")

haskell_import(name = "cookie")

haskell_import(name = "cryptonite")

haskell_import(name = "data-default")

haskell_import(name = "deriving-compat")

# Can have problems with large dir listings. Prefer variant from
# streaming-commons, or its conduit equivalent.
#
# Note that streaming one yields dirs only in random order.
haskell_import(name = "directory")

# For working with legacy stuff only!
# Prefer 'safe-exceptions' or 'unliftio' rather.
haskell_import(name = "exceptions")

haskell_import(name = "fast-logger")

haskell_import(name = "free")

haskell_import(name = "errors")

haskell_import(name = "groom")

haskell_import(name = "hedgehog")

haskell_import(name = "HTTP")

haskell_import(name = "html-conduit")

haskell_import(name = "http-api-data")

haskell_import(name = "http-client")

haskell_import(name = "http-types")

haskell_import(name = "lens")

haskell_import(name = "neat-interpolation")

haskell_import(name = "network")

haskell_import(name = "network-uri")

haskell_import(name = "optparse-applicative")

haskell_import(name = "postgresql-simple")

haskell_import(name = "protolude")

haskell_import(name = "prometheus-client")

haskell_import(name = "prometheus-metrics-ghc")

haskell_import(name = "recursion-schemes")

haskell_import(name = "resourcet")

# See https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell.
# For new code consider using 'unliftio'.
# For legacy third-party code might need to use 'exceptions'.
haskell_import(name = "safe-exceptions")

# For quick one-shot stuff. Prefer 'servant' for more complicated or production
# things.
haskell_import(name = "scotty")

haskell_import(name = "servant")

haskell_import(name = "servant-client")

haskell_import(name = "servant-server")

haskell_import(name = "split")

haskell_import(name = "stm-conduit")

haskell_import(name = "tagsoup")

haskell_import(name = "tasty")

haskell_import(name = "tasty-hedgehog")

haskell_import(name = "tasty-hunit")

haskell_import(name = "tasty-silver")

haskell_import(name = "text-format")

haskell_import(name = "text-icu")

haskell_import(name = "unix")

haskell_import(name = "unliftio-core")

haskell_import(name = "unliftio")

haskell_import(name = "unordered-containers")

haskell_import(name = "vector")

haskell_import(name = "wai")

haskell_import(name = "wai-middleware-prometheus")

haskell_import(name = "warp")

# Easy-to-use HTTP client lib.
haskell_import(name = "wreq")

# Pretty unpenetrable streaming combinators, resort to DOM (Cursor) usage.
# Might use hexml instead? Could research.
haskell_import(name = "xml-conduit")
