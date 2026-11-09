{ pkgs ? import <nixpkgs> {}
}:

pkgs.mkShell {
  name = "build-shell";

  nativeBuildInputs = [
    pkgs.go_1_27
    pkgs.universal-ctags
  ];
}
