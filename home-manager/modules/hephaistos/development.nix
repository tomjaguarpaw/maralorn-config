{ pkgs, ... }:
{
  home.packages = [
    pkgs.remmina
    pkgs.mdbtools
  ];
}
