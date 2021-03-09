package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
    "haskell_library",
)

# Expand below list on-demand. Need to add to //:ghc.nix as well.

# GHC-bundled libraries
haskell_toolchain_library(name = "base")

haskell_toolchain_library(name = "bytestring")

haskell_toolchain_library(name = "clock")

haskell_toolchain_library(name = "containers")

haskell_toolchain_library(name = "criterion")

haskell_toolchain_library(name = "mtl")

haskell_toolchain_library(name = "text")

# Note: we have local modifications, build directly until upstreamed.
# Actually we need the library to be used in the repl, for fast local
# development. Can go back to toolchain lib after.
#
# haskell_toolchain_library(name = "text-offset")
haskell_library(
    name = "text-offset",
    srcs = ["TextOffset.hs"],
    deps = [
        ":base",
        ":text",
        ":vector",
    ],
)

haskell_toolchain_library(name = "time")

haskell_toolchain_library(name = "transformers")

# Non-GHC-bundled libraries
haskell_toolchain_library(name = "aeson")

haskell_toolchain_library(name = "align")

haskell_toolchain_library(name = "async")

haskell_toolchain_library(name = "base64-bytestring")

haskell_toolchain_library(name = "boxes")

haskell_toolchain_library(name = "conduit")

haskell_toolchain_library(name = "cookie")

haskell_toolchain_library(name = "cryptonite")

haskell_toolchain_library(name = "data-default")

haskell_toolchain_library(name = "deriving-compat")

# Can have problems with large dir listings. Prefer variant from
# streaming-commons, or its conduit equivalent.
#
# Note that streaming one yields dirs only in random order.
haskell_toolchain_library(name = "directory")

# For working with legacy stuff only!
# Prefer 'safe-exceptions' or 'unliftio' rather.
haskell_toolchain_library(name = "exceptions")

haskell_toolchain_library(name = "fast-logger")

haskell_toolchain_library(name = "free")

haskell_toolchain_library(name = "errors")

haskell_toolchain_library(name = "groom")

haskell_toolchain_library(name = "hedgehog")

haskell_toolchain_library(name = "HTTP")

haskell_toolchain_library(name = "html-conduit")

haskell_toolchain_library(name = "http-api-data")

haskell_toolchain_library(name = "http-client")

haskell_toolchain_library(name = "http-types")

haskell_toolchain_library(name = "lens")

haskell_toolchain_library(name = "neat-interpolation")

haskell_toolchain_library(name = "network")

haskell_toolchain_library(name = "network-uri")

haskell_toolchain_library(name = "optparse-applicative")

haskell_toolchain_library(name = "postgresql-simple")

haskell_toolchain_library(name = "protolude")

haskell_toolchain_library(name = "prometheus-client")

haskell_toolchain_library(name = "prometheus-metrics-ghc")

haskell_toolchain_library(name = "recursion-schemes")

haskell_toolchain_library(name = "resourcet")

haskell_toolchain_library(name = "resource-pool")

# See https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell.
# For new code consider using 'unliftio'.
# For legacy third-party code might need to use 'exceptions'.
haskell_toolchain_library(name = "safe-exceptions")

# For quick one-shot stuff. Prefer 'servant' for more complicated or production
# things.
haskell_toolchain_library(name = "scotty")

haskell_toolchain_library(name = "servant")

haskell_toolchain_library(name = "servant-client")

haskell_toolchain_library(name = "servant-server")

haskell_toolchain_library(name = "split")

haskell_toolchain_library(name = "stm-conduit")

haskell_toolchain_library(name = "tagsoup")

haskell_toolchain_library(name = "tasty")

haskell_toolchain_library(name = "tasty-hedgehog")

haskell_toolchain_library(name = "tasty-hunit")

haskell_toolchain_library(name = "tasty-silver")

haskell_toolchain_library(name = "text-format")

haskell_toolchain_library(name = "text-icu")

haskell_toolchain_library(name = "unix")

haskell_toolchain_library(name = "unliftio-core")

haskell_toolchain_library(name = "unliftio")

haskell_toolchain_library(name = "unordered-containers")

haskell_toolchain_library(name = "vector")

haskell_toolchain_library(name = "wai")

haskell_toolchain_library(name = "wai-middleware-prometheus")

haskell_toolchain_library(name = "warp")

# Easy-to-use HTTP client lib.
haskell_toolchain_library(name = "wreq")

# Pretty unpenetrable streaming combinators, resort to DOM (Cursor) usage.
# Might use hexml instead? Could research.
haskell_toolchain_library(name = "xml-conduit")
