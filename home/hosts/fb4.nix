{ pkgs, ... }:
{
imports = [
    ../snippets/everywhere.nix
    ../snippets/graphical.nix
  ];
  home.packages = with pkgs; [
    xautolock
    syncthing
  ];
}
