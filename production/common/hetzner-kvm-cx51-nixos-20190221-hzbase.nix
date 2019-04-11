{ config, lib, pkgs, ... }:

# This file was generated when installing NixOS on Hetzner. Use the machine
# type and snapshot given in the filename when provisioning new instances.
{
  imports =
    [ <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ];

  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.grub =
    { enable = true;
      version = 2;
      device = "/dev/sda";
    };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/dc3ccc65-2b5f-45c4-b6eb-334101308476";
      fsType = "ext4";
    };

  fileSystems."/data" =
    { device = "/dev/disk/by-uuid/878e915a-f0d5-4c61-9264-57574c9e004b";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/3d5652de-5f19-4262-9993-1584374571ea"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
}
