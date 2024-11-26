{ pkgs, ... }:
{
  boot = {
    zfs.package = pkgs.zfs_unstable;
    kernelPackages = pkgs.linuxPackages_6_11;
  };
}
