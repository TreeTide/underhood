# Pinned nixpkgs, see
# https://nmattia.com/posts/2019-01-15-easy-peasy-nix-versions.html.

with { fetch = import ./nix/fetch.nix; };
rec {
  pkgs = import fetch.nixpkgs {};
  npmBuild = pkgs.callPackage (import fetch.nixNpmBuildpackage) {};

  # Merged from old setup, TODO(robinp): move away.
  paths =
    (with pkgs.lib.lists;
    with pkgs.lib.strings;
    let
      diffPath = sep: a: b:
          let an = splitString "/" (toString a);
              bn = splitString "/" (toString b);
          in concatStringsSep sep (drop (length an) bn);

      sanitizeF = diffPath "_" ./.;
    in { diffBasedName = sanitizeF; }
    );
}
