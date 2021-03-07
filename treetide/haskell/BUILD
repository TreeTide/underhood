package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_binary",
    "haskell_library",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "prebuilt",
)

haskell_library(
    name = "pg_pool",
    srcs = ["pg_pool.hs"],
    deps = [
        prebuilt("protolude"),
        prebuilt("postgresql-simple"),
        prebuilt("resource-pool"),
        prebuilt("time"),
    ],
)

