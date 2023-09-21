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
  xdg.configFile."river/init".source = ./river-init.sh;
  xdg.configFile."river-luatile/layout.lua".source = ./river-layout.lua;
  wayland.windowManager.sway.enable = true;
  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        exec = [
          "eww open bar"
          "eww open bg"
        ];
        outputs = [ { criteria = "eDP-1"; } ];
      };
      docked = {
        exec = [
          "eww open bar"
          "eww open bg"
        ];
        outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          { criteria = "DP-6"; }
        ];
      };
    };
  };
}
