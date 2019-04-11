# machine_content: the nixos config function to be applied to the machine
{ machine_content
, machine_memory_mbs ? 1024
}:

{ config, pkgs, ... }:
{
  deployment.targetEnv = "virtualbox";
  deployment.virtualbox = {
    memorySize = machine_memory_mbs; # MBs
    vcpu = 2;
    headless = true;
    sharedFolders = {
      # Can mount in guest using
      #   mount -t vboxsf exchange /my/mount_point
      exchange = {
        hostPath = "/tmp/vbox_exchange";
        readOnly = false;
      };
    };
  };
  # virtualisation.virtualbox.host.addNetworkInterface = true;
  virtualisation.virtualbox.guest.enable = true;
  imports = [ machine_content ];
}
