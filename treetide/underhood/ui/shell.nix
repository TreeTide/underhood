{ pkgs ? (import ../../../default.nix).pkgs
}:

with pkgs;
stdenv.mkDerivation {
  name = "ui-shell";
  buildInputs = [
    nodejs
    chromium
  ];
}
