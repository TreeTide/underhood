workspace(name = "treetide")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

#
# Nix / Haskell
#

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-a3f0cab25c8be147d79dd45d0c9d01390582727b",
    urls = ["https://github.com/tweag/rules_haskell/archive/a3f0cab25c8be147d79dd45d0c9d01390582727b.tar.gz"],
    sha256 = "c408b8339f8add9e2d5382b29b371e7f68259b3dd5d163cbf7db4e482c2fff8f",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.6.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.6.0.tar.gz"],
    sha256 = "f5af641e16fcff5b24f1a9ba5d93cab5ad26500271df59ede344f1a56fc3b17d",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository", "nixpkgs_package", "nixpkgs_python_configure")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:nixpkgs.nix",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
haskell_register_ghc_nixpkgs(
    #locale_archive = "@glibc_locales//:locale-archive",
    attribute_path = "ghc",
    repositories = {
        "ghc": "@//:ghc.nix",
    },
    nix_file_content = '{ghc = import <ghc>;}',
    compiler_flags =
        [
            "-threaded",  # Use multicore runtime
            "-rtsopts",   # Enable specifying runtime options on command line.

            # Set default RTS options.
            # -maxN<X>: use up to X cores if available.
            # -qn4: only use 4 cores for parallel GC.
            # -A64m: use larger allocation area.
            # -n4m: use allocation chunks, which can be beneficial on multicore.
            # See https://simonmar.github.io/posts/2015-07-28-optimising-garbage-collection-overhead-in-sigma.html.
            # -T: make gc stats available in-program
            # -s: print summary GC statistics to stderr on exit.
            "-with-rtsopts=-maxN8 -qn4 -A64m -n4m -T -s",

            # Switch on useful extra warnings, and make warnings compilation
            # error.
            "-Wall",
            "-Werror",
            "-Wcompat",
            "-Wincomplete-record-updates",
            "-Wincomplete-uni-patterns",
            "-Wredundant-constraints",
        ],
    version = "8.6.5",
)

# Note: will move to toolchains.bzl in HEAD
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_toolchains")
rules_haskell_toolchains(version = "8.6.5")

#
# Go & Buildifier
#

# buildifier is written in Go and hence needs rules_go to be built.
# See https://github.com/bazelbuild/rules_go for the up to date setup instructions.
http_archive(
    name = "io_bazel_rules_go",
    urls = [
      "https://storage.googleapis.com/bazel-mirror/github.com/bazelbuild/rules_go/releases/download/v0.20.3/rules_go-v0.20.3.tar.gz",
      "https://github.com/bazelbuild/rules_go/releases/download/v0.20.3/rules_go-v0.20.3.tar.gz",
    ],
    sha256 = "e88471aea3a3a4f19ec1310a55ba94772d087e9ce46e41ae38ecebe17935de7b",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies", "go_register_toolchains")

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
