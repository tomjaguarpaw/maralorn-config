{ pkgs, ... }:
{
  home.packages = [
    pkgs.river
    pkgs.wlopm
    pkgs.wlr-randr
    pkgs.swaybg
    pkgs.kanshi
    pkgs.jaq # Für eww
    pkgs.river-luatile
    pkgs.river-tag-overlay
  ];
  xdg.configFile = {
    "river/init".source = ./river-init.sh;
    "river-luatile/layout.lua".source = ./river-layout.lua;
  };
}
