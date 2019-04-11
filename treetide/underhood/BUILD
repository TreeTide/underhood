package(default_visibility = ["//visibility:public"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "prebuilt",
)

haskell_library(
    name = "frontend_api",
    srcs = ["frontend_api.hs"],
    deps = [
        prebuilt("aeson"),
        prebuilt("protolude"),
        prebuilt("servant"),
        prebuilt("text"),
    ],
)
