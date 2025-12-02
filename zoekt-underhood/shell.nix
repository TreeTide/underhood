{ pkgs ? import <nixpkgs> {}
}:

pkgs.mkShell {
  name = "build-shell";

  nativeBuildInputs = [
    pkgs.go_1_23
    pkgs.universal-ctags
  ];
}
