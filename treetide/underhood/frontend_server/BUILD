package(default_visibility = ["//visibility:public"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
)
load(
    "//treetide/haskell/build_defs:build.bzl",
    "extended",
    "prebuilt",
)

haskell_binary(
    name = "frontend_server",
    srcs = ["FrontendServer.hs"],
    deps = [
        "//treetide/underhood/kythe_api",
        "//treetide/underhood/kythe_api:convert",
        "//treetide/underhood/xref_frontend_api",
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
