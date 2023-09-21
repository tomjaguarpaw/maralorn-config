{ pkgs, ... }:
{
  programs.eww = {
    enable = true;
    package = pkgs.callPackage ./_eww-package.nix { withWayland = true; };
    configDir = ./eww-config;
  };
}
