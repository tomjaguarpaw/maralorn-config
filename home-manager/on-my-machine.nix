{ pkgs, config, lib,  ... }:
{
  home = {
    username = "maralorn";
    homeDirectory = "/home/maralorn";
    packages = builtins.attrValues (import ../common/pkgs.nix).my-home-pkgs;
  };
}
