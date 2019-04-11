{ pkgs ? (import ./default.nix).pkgs
}:

with pkgs;
stdenv.mkDerivation {
  name = "dev-shell";
  buildInputs = [
    bazel
    (import ./ghc.nix)
  ] ++ (import ./tooling.nix);

  shellHook = ''
    # Some code-formatting helpers.
    brittanyPreviousCommit () {
      git log --stat HEAD~.. --name-only --format=format:"" | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
      git diff
    }
    brittanyChanged () {
      git status --porcelain | grep '^\s*[AM]' | awk '{print $2}' | grep '\.hs$' | xargs -n1 brittany --write-mode inplace
    }
    hlintApply () {
      hlint --refactor --refactor-options="-i -s" $@
    }
  '';
}
