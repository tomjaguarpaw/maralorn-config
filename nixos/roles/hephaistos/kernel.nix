{ pkgs, config, ... }:
{
  boot = {
    zfs.package = config.boot.kernelPackages.zfs_unstable.userspaceTools;
    kernelPackages = pkgs.linuxPackages_6_11;
  };
}
