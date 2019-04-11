{ npmBuild ? (import <tt>).npmBuild
, pkgs ? (import <tt>).pkgs
}:

with rec {
  compiled_js = npmBuild.buildNpmPackage {
    # We abuse buildNpmPackage, which seems to be intended to build server-side
    # JS code, into building frontend code.

    name = "underhood-js-bundle";

    src = pkgs.lib.sources.sourceByRegex ./.
            [ "package.*json"
              "src.*"
              "webpack.config.js"
            ];

    npmBuildMore = "npm run build:prod";

    # Disable npmPackPhase which we don't need and is buggy?
    preInstallPhases = [];

    # We don't need the original install magic, just copy the outputs.
    installPhase = ''
      mkdir -p $out;
      cp dist/* $out/;
    '';
  };
  static_resources = pkgs.stdenv.mkDerivation {
    name = "underhood-ui-bundle";
     src = ./static;
     buildInputs = [ compiled_js ];
     builder = pkgs.writeText "build.sh" ''
       source $stdenv/setup;
       mkdir $out;
       cp ${compiled_js}/* $out/;
       cp -r $src/* $out/;
     '';
  };
};
{ inherit static_resources;
}
