{ config, lib, pkgs, ... }:

# This file was generated when installing NixOS on the Hetzner machine.
# Bootloader options were moved here from configuration.nix.
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
    { device = "/dev/disk/by-uuid/ab318254-4a14-43f2-8db8-1901174aaae2";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/64cb7897-0318-4479-870a-b743e472e6d9"; }
    ];

  nix.maxJobs = lib.mkDefault 1;
}
