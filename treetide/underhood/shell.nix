{ pkgs ? (import ../../default.nix).pkgs
}:

with pkgs;
stdenv.mkDerivation {
  name = "dev-shell";
  buildInputs =
    [ bazel
      nodejs
    ];
}
