{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs) gamemode mangohud steam-tui;
    inherit (pkgs.steamPackages) steamcmd;
  };
}
