let
  pkgs = (import ./default.nix).pkgs;
  # Note: below would also work if called via Bazel/rules_nix. But to avoid
  # chance of instantiating manually with the unpinned nixpkgs, we specify
  # explicitly.
  #
  #     pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override (old: {
     overrides =
      with pkgs;
      lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
        # Needs version override due to containers upper bound.
        # foobar = haskell.lib.doJailbreak super.foobar;
     });
  });
in haskellPackages.ghcWithPackages (p: with p; [
  # Note: contrary to the remarks here, see //treetide/thirdparty/haskell/BUILD
  # for more specific remarks and pointers about these libs.
  aeson
  align
  async
  base64-bytestring
  boxes
  clock
  conduit
  containers
  cookie
  criterion
  cryptonite
  data-default
  deriving-compat
  directory
  errors
  exceptions   # for dealing legacy code only
  fast-logger  # TODO(robinp): ditch?
  free
  groom
  hedgehog
  HTTP
  html-conduit
  http-api-data
  http-client
  http-types
  lens
  mtl
  neat-interpolation
  network
  network-uri
  optparse-applicative
  postgresql-simple
  protolude
  prometheus-client
  prometheus-metrics-ghc
  recursion-schemes
  resourcet
  safe-exceptions
  scotty     # prefer servant
  servant
  servant-client
  servant-server
  split
  stm-conduit
  tagsoup
  tasty
  tasty-hedgehog
  tasty-hunit
  tasty-silver
  text-format
  text-icu
  time
  unix
  unliftio-core
  unliftio
  unordered-containers
  vector
  wai
  wai-middleware-prometheus
  warp
  wreq
  xml-conduit
])
