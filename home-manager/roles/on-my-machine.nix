{
  pkgs,
  config,
  lib,
  ...
}: {
  home = {
    username = "maralorn";
    homeDirectory = "/home/maralorn";
    packages = builtins.attrValues pkgs.my-home-pkgs;
  };
}
