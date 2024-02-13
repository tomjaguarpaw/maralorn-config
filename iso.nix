{ pkgs, ... }:
{

  nixpkgs.overlays = [
    (_: super: {
      zfs = super.zfs.overrideAttrs (_: {
        meta.platforms = [ ];
      });
    })
  ];

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
    "repl-flake"
  ];

  boot = {
    supportedFilesystems = [ "bcachefs" ];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  #boot.supportedFilesystems = [ "bcachefs" ];
  console = {
    earlySetup = true;
    font = "${pkgs.spleen}/share/consolefonts/spleen-6x12.psfu";
    keyMap = "neo";
  };
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
