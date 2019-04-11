workspace(name = "treetide")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

#
# Nix / Haskell
#

http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-513b14c0b3ece6d05b159455bb2df7335c759156",
    urls = ["https://github.com/tweag/rules_haskell/archive/513b14c0b3ece6d05b159455bb2df7335c759156.tar.gz"],
    sha256 = "c9f0be2ddfc4820d83141e399668f5277dee5ba5e3b7cc6d278e7f116f3568d9",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.5.2",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.2.tar.gz"],
    sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository", "nixpkgs_package")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:nixpkgs.nix",
)

nixpkgs_package(
    name = "ghc",
    build_file = "@io_tweag_rules_haskell//haskell:ghc.BUILD",
    nix_file = "//:ghc.nix",
    repositories = {"nixpkgs": "@nixpkgs//:nixpkgs.nix"},
)

register_toolchains("//:ghc")

#
# Go & Buildifier
#

# buildifier is written in Go and hence needs rules_go to be built.
# See https://github.com/bazelbuild/rules_go for the up to date setup instructions.
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "f87fa87475ea107b3c69196f39c82b7bbf58fe27c62a338684c20ca17d1d8613",
    url = "https://github.com/bazelbuild/rules_go/releases/download/0.16.2/rules_go-0.16.2.tar.gz",
)

load("@io_bazel_rules_go//go:def.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    strip_prefix = "buildtools-1f3678932acd38e2c0a4e2b2db526d30a8f540a6",
    url = "https://github.com/bazelbuild/buildtools/archive/1f3678932acd38e2c0a4e2b2db526d30a8f540a6.zip",
    sha256 = "50959f8806b0bd596c21b5d0b6c81d6305704579f1cc6836c44c541e4d7c0416",
)

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()
