{ enableSSL
}:
{ config, pkgs, lib, ... }:

with rec {
  uh_domain = "codeunderhood.com";
  acme_maintainer = "robinp@treetide.com";

  prod_user = "underhood";

  # For now this is not hermetic, a 'bazel build' needs to be executed upfront.
  binaries = import <tt/treetide/underhood/wrapup_binaries.nix> {};

  frontend = import <tt/treetide/underhood/ui> {};

  frontend_server_port = 8081;

  # Note: for now you have to copy and start the kythe api server manually
  # to the server. TODO(robinp): nix automation to come.
  kythe_api_port = 8080;

  #
  # Nixos config fragments
  #

  underhood_prod_user = {
    users.users.${prod_user} = {};
  };

  frontend_server_service = {
    systemd.services.underhood_frontend_server = {
      description = "Serves frontend API requests.";
      after       =
        [ "network.target"
        ];
      before      =
        [ "nginx.service"
        ];
      serviceConfig = {
          ExecStart = "${binaries}/bin/treetide/underhood/frontend_server/frontend_server --port ${toString frontend_server_port} --kythe_api_port ${toString kythe_api_port}";
          Restart = "always";
          RestartSec = "10s";
          # Stop restart attempts after too many consecutive failures.
          # TODO(robinp): should raise some error notification after limit.
          StartLimitInterval = "1min";
          StartLimitBurst    = "5";
          User = prod_user;
        };
    };
  };
};

# Combine config fragments.
# See https://github.com/NixOS/nixpkgs/issues/56449 about reverse.
#
lib.mkMerge (lib.lists.reverseList
  [ (import <tt/production/common/base_configuration.nix> { inherit config pkgs; })
    (import <tt/production/common/nixos_nixops.nix> {})
    underhood_prod_user
    frontend_server_service

    (import <tt/production/common/nginx.nix> {
      domainName = uh_domain;
      staticResourcesRoot = frontend.static_resources;
      inherit enableSSL;
      acmeMaintainerEmail = acme_maintainer;
    } { inherit pkgs; })

    {
      services.nginx = {
        virtualHosts.${uh_domain} = {
          locations."/api/".extraConfig = ''
            proxy_pass http://127.0.0.1:${toString frontend_server_port};
          '';
        };
      };
    }
    {
      users.users.root.openssh.authorizedKeys.keys =
        [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC54/KOweZt/FkdLFWkKCWKVppoU1nANbta8ClZ5tAXex/4qwsMtbm9v7tyjsjPlpYS11LgDPEInaz+vGBXFskZA0g5lrnKHPo1mMiSFmfXo/CzPzZRiOI6IWJreT6E2SXniKH1erOIjE1DP/iM5pfMMjqG4R+rTNzGKBy0f0qQlDq2+z8VwXfjEipBCQ3GTU9s15e3h6wT7Nxn0QT37gi+o2eU+kJbD7Sv1AXIleLjC5pc6Trblg+oiIz64Y4N2aGwYN+n4skDZ5zjUsmU/s7VBMqSI2QoURtJfqzsPXzj9XVoWIY4Fbilwp/fyrTFYoCS1kbJni15Qp82kHbxOenr ron@rigo"
        ];
    }
  ])
