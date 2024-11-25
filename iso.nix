{ pkgs, ... }:
{

  environment.systemPackages = [
    pkgs.helix
    pkgs.fd
    pkgs.htop
    pkgs.tree
    pkgs.lazygit
  ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  boot = {
    zfs.package = pkgs.zfs_unstable;
    kernelPackages = pkgs.linuxPackages_latest;
  };

  #boot.supportedFilesystems = [ "bcachefs" ];
  console = {
    earlySetup = true;
    keyMap = "neo";
  };
}
