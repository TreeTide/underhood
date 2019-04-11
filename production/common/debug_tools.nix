# Set of tools commonly used to troubleshoot things.
{ pkgs }:

{
  environment.systemPackages =
    with pkgs;
    [ curl
      iotop
      multitail
      ncdu
      strace
      tcpdump
      vim
      wget
    ];
}
