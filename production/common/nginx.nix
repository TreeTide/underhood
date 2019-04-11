# To be used as a baseline configuration, to be amended by mixing in other
# options using 'lib.mkMerge'.
#
# NOTE: lib.mkMerge composes in reverse order, so don't forget to reverse
# the list to get expected config append order.

{ domainName
, staticResourcesRoot
, enableSSL
, acmeMaintainerEmail ? ""
, debugRules ? false
}:

assert enableSSL -> (acmeMaintainerEmail != "");

{ pkgs }:

{
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  security.acme.certs = if enableSSL
    then {
      ${domainName} = {
        email = acmeMaintainerEmail;
        extraDomains = {
          "www.${domainName}" = null;
        };
      };
    }
    else {};

  services.nginx = {
    enable = true;

    # See https://github.com/NixOS/nixpkgs/issues/25485.
    # Patches nignx to use nix-store hashes as etags (instead of dates).
    package = with pkgs; nginx.overrideAttrs (super: {
      patches = (super.patches or []) ++ [(fetchpatch {
        url = https://gitlab.com/yegortimoshenko/patches/raw/cc67b35dc7cf68a18ba98043b54ed3d10374c486/nginx/nix-etag-1.15.4.patch;
        sha256 = "16pa1vwcm7kibkjsxpk3szaa2vnxdinml6v0fp4038i2qaav113k";
      })];
    });

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    # For debugging rules:
    appendConfig = if debugRules
      then "error_log /var/spool/nginx/logs/debug.log debug;"
      else "";

    virtualHosts."www.${domainName}" = {
      extraConfig = ''
        return 301 $scheme://${domainName}$request_uri;
      '';
    };
    virtualHosts.${domainName} = {
      enableACME = enableSSL;
      # addSSL = true;
      forceSSL = enableSSL;

      root = staticResourcesRoot;

      # Last-modified is set to epoch in nix-store, so disable it.
      extraConfig = ''
        if_modified_since off;
        add_header Last-Modified "";
      '';
    };
  };

}
