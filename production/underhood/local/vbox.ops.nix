# Nixops config for the Pyurlex serving stack.
# Use
#
#     ttops create -d my-underhood-vm vbox.ops.nix
#     ttops deploy -d my-underhood-vm
#
# to provision a virtualbox (see //production/README.nixops.md for 'ttops').
#
# Note: if you just use nixpkgs, but not nixos, don't install VirtualBox through
# nixpkgs. It would have version out of sync with your kernel. Rather use your
# package manager.

{
  underhood-main =
    (import <tt/production/common/nixops_vbox_machine.nix> {
      machine_memory_mbs = 4096;
      machine_content = (import ../underhood_image.nix
        { enableSSL = false;
        });
    });

  network.description = "CodeUnderHood serving (vbox).";
}
