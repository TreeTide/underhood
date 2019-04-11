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
    name = "json_naming",
    srcs = ["json_naming.hs"],
    deps = [
        prebuilt("aeson"),
        prebuilt("base"),
    ],
)

