package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
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

