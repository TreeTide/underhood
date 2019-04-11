{
  underhood-main =
    { config, pkgs, ... }:
    with {
      underhood_ip = "116.203.6.232";
      underhood_domain = "codeunderhood.com";
    };
    {
      deployment.targetHost = underhood_ip;
      networking.hostName = underhood_domain;
      networking.interfaces =
        { ens3 =
          { useDHCP = true; # for non-floating address
            ipv4.addresses =
              [ { address = underhood_ip; # for hetzner floating ip
                  prefixLength = 32;
                }
              ];
          };
        };
      imports =
        [ <tt/production/common/hetzner-kvm-small.nix>
          (import ../underhood_image.nix { enableSSL = true; })
          ({...}:
           {
            users.users.root.openssh.authorizedKeys.keys =
              [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC54/KOweZt/FkdLFWkKCWKVppoU1nANbta8ClZ5tAXex/4qwsMtbm9v7tyjsjPlpYS11LgDPEInaz+vGBXFskZA0g5lrnKHPo1mMiSFmfXo/CzPzZRiOI6IWJreT6E2SXniKH1erOIjE1DP/iM5pfMMjqG4R+rTNzGKBy0f0qQlDq2+z8VwXfjEipBCQ3GTU9s15e3h6wT7Nxn0QT37gi+o2eU+kJbD7Sv1AXIleLjC5pc6Trblg+oiIz64Y4N2aGwYN+n4skDZ5zjUsmU/s7VBMqSI2QoURtJfqzsPXzj9XVoWIY4Fbilwp/fyrTFYoCS1kbJni15Qp82kHbxOenr ron@rigo"
              ];
           }
          )
        ];
    };
  network.description = "CodeUnderHood serving (prod).";
}
