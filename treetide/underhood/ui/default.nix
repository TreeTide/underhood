{ pkgs ? (import <tt>).pkgs
, nodejs ? pkgs."nodejs-10_x"
}:

with rec {
  compiled_js =
    with {
      nodeBuilt = (import ./node-default.nix { inherit pkgs nodejs; }).package;
    };
    pkgs.stdenv.mkDerivation {
      name = "underhood-js-bundle";

      src = nodeBuilt;
      unpackPhase = "";

      nativeBuildInputs = [
        # See below why this doesn't work.
        # pkgs.removeReferencesTo
      ];

      # TODO --mode production outputs files, but chokes. Possibly some webpack
      #     misconfiguration.
      buildCommand = ''
        cd $src/lib/node_modules/underhood-ui;
        ./node_modules/webpack/bin/webpack.js --mode development --output-path $out

        # Hack to remove some Vue-injected source paths, so node stuff won't be
        # picked up as runtime dependency. See
        # nixpkgs/pkgs/build-support/remove-references-to/default.nix.
        # Actually that doesn't work, since the injected paths don't have the
        # /nix/store prefix, which the script expects, so going with manual hack.
        #find $out -type f -exec remove-references-to -t $src '{}' \;

        # Note: didn't verify recently if this is still needed. Check eventually.
        find $out -type f -name '*.js' | xargs sed -i 's/options.__file=".../options.__file="eee/g'
      '';
    };
  static_resources = pkgs.stdenv.mkDerivation {
    name = "underhood-ui-bundle";
     src = ./static;
     buildInputs = [ compiled_js ];
     builder = pkgs.writeText "build.sh" ''
       source $stdenv/setup;
       mkdir $out;
       cp -r ${compiled_js}/* $out/;
       cp -r $src/* $out/;
     '';
  };
};
{ inherit static_resources;
}
