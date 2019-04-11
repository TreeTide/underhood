{ pkgs ? (import ../default.nix).pkgs
}:

with pkgs;
stdenv.mkDerivation {
  name = "prod-shell";

  buildInputs = [
    # the unstable is usually less buggy since bugs are fixed in it. YMMV.
    # If you need more recent, pass in different pkgs at the top.
    nixopsUnstable
  ];

  shellHook = ''
    ttops() {
      TT=$(readlink -m ..)
      TT_PKGS=$(nix-instantiate --eval -E "(import ../nix/fetch.nix).nixpkgs" | tr -d '"')
      echo === Pinned nixpkgs to $TT_PKGS ===
      NIX_PATH=nixpkgs=$TT_PKGS:tt=$TT nixops $@
    }
  '';
}
