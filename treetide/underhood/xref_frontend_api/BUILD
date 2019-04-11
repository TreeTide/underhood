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
    name = "xref_frontend_api",
    srcs = ["XrefFrontendApi.hs"],
    deps = [
        prebuilt("aeson"),
        prebuilt("protolude"),
        prebuilt("servant"),
        prebuilt("text"),
    ],
)
