package(default_visibility = ["//visibility:public"])

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_toolchain",
)

haskell_toolchain(
    name = "ghc",
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
    tools = "@ghc//:bin",
    version = "8.6.3",
)

load("@com_github_bazelbuild_buildtools//buildifier:def.bzl", "buildifier")

buildifier(
    name = "buildifier",
)
