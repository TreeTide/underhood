package(default_visibility = ["//visibility:public"])

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_repl",
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

haskell_binary(
    name = "frontend_server_postgres",
    srcs = ["frontend_server_postgres.hs"],
    deps = [
        ":types",
        "//treetide/haskell:pg_pool",
        "//treetide/underhood/kythe_api",
        "//treetide/underhood/kythe_api:convert",
        "//treetide/underhood:frontend_api",
        extended("optparse-applicative"),
        extended("postgresql-simple"),
        prebuilt("base"),
        prebuilt("base64-bytestring"),
        prebuilt("containers"),
        prebuilt("data-default"),
        prebuilt("deriving-compat"),
        prebuilt("free"),
        prebuilt("groom"),
        prebuilt("protolude"),
        prebuilt("recursion-schemes"),
        prebuilt("servant-server"),
        prebuilt("text"),
        prebuilt("wai"),
        prebuilt("warp"),
    ],
)

haskell_repl(
    name = "frontend_server_postgres_repl",
    deps = [
        ":frontend_server_postgres",
    ]
)

