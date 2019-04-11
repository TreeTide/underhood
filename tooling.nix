# Packages that are not required for building, just as tools.  For example
# linters, formatters, etc.

let
  pkgs = (import ./default.nix).pkgs;
  haskellPackages = pkgs.haskellPackages.override (old: {
     overrides =
      with pkgs;
      lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
        # For brittany
        multistate = haskell.lib.doJailbreak super.multistate;
        # See https://github.com/NixOS/nixpkgs/issues/54534.
        brittany = haskell.lib.doJailbreak (self.callCabal2nix "brittany"
              (pkgs.fetchFromGitHub {
                owner  = "lspitzner";
                repo   = "brittany";
                rev    = "6c187da8f8166d595f36d6aaf419370283b3d1e9";
                sha256 = "0nmnxprbwws3w1sh63p80qj09rkrgn9888g7iim5p8611qyhdgky";
                }) {});
     });
  });
in with haskellPackages; [
  brittany
  
  apply-refact  # for hlint
  hlint
]
