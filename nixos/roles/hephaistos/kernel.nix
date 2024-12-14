{ pkgs, config, ... }:
{
  boot = {
    zfs.package = config.boot.kernelPackages.zfs.userspaceTools;
    kernelPackages = pkgs.linuxPackages_6_12;
  };
}
