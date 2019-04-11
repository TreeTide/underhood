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
    name = "extended",
    srcs = ["Prometheus/Extended.hs"],
    deps = [
        prebuilt("base"),
        prebuilt("clock"),
        prebuilt("prometheus-client"),
        prebuilt("text"),
    ],
)

