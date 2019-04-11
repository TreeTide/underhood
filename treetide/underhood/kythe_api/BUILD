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
    name = "kythe_api",
    srcs = ["kythe_api.hs"],
    deps = [
        "//treetide/haskell/json_naming",
        prebuilt("aeson"),
        prebuilt("base"),
        prebuilt("containers"),
        prebuilt("data-default"),
        prebuilt("servant"),
        prebuilt("text"),
    ],
)

haskell_library(
    name = "convert",
    srcs = ["convert.hs"],
    deps = [
        ":kythe_api",
        prebuilt("base"),
        prebuilt("http-types"),
        prebuilt("network-uri"),
        prebuilt("protolude"),
        prebuilt("text"),
    ],
)
