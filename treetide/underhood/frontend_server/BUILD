package(default_visibility = ["//visibility:public"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "extended",
    "prebuilt",
)

haskell_library(
    name = "types",
    srcs = ["types.hs"],
    deps = [
        "//treetide/underhood/kythe_api",
        "//treetide/underhood:frontend_api",
        prebuilt("servant-client"),
        prebuilt("servant-server"),
    ],
)

haskell_binary(
    name = "frontend_server",
    srcs = ["frontend_server.hs"],
    deps = [
        ":types",
        "//treetide/underhood/kythe_api",
        "//treetide/underhood/kythe_api:convert",
        "//treetide/underhood:frontend_api",
        extended("optparse-applicative"),
        prebuilt("base"),
        prebuilt("base64-bytestring"),
        prebuilt("containers"),
        prebuilt("data-default"),
        prebuilt("deriving-compat"),
        prebuilt("free"),
        prebuilt("groom"),
        prebuilt("http-client"),
        prebuilt("protolude"),
        prebuilt("recursion-schemes"),
        prebuilt("servant-client"),
        prebuilt("servant-server"),
        prebuilt("text"),
        prebuilt("wai"),
        prebuilt("warp"),
    ],
)
