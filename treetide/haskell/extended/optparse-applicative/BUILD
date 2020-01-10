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
    name = "extended",
    srcs = ["Extended.hs"],
    deps = [
        prebuilt("base"),
        prebuilt("optparse-applicative"),
        prebuilt("text"),
    ],
)

