{ config, pkgs, ... }:

{
  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Setting time zone to be uniform.
  time.timeZone = "UTC";

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    permitRootLogin = "prohibit-password";
  };

  # Temporary while messing with nixops.
  # Important, the image should include this so the first nixops-deploy can
  # get access. But then in the nixops-managed config this is not needed,
  # since nixops generates a keypair to manage the machine.
  #
  # services.openssh.permitRootLogin = "yes";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
