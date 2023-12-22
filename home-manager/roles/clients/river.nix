{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs)
      river
      wlopm
      wlr-randr
      swaybg
      kanshi
      jaq # Für eww
      river-luatile
      river-tag-overlay
      light
      ;
    tv-on = pkgs.writeShellScriptBin "tv-on" ''
      wlr-randr --output HDMI-A-1 --mode 1920x1080
    '';
    tv-only = pkgs.writeShellScriptBin "tv-only" ''
      wlr-randr --output HDMI-A-1 --mode 1920x1080
      wlr-randr --output DP-3 --off
    '';
    tv-off = pkgs.writeShellScriptBin "tv-off" ''
      wlr-randr --output HDMI-A-1 --off
      wlr-randr --output DP-3 --mode 5120x1440
    '';
  };
  xdg.configFile = {
    "river/init".source = ./river-init.sh;
    "river-luatile/layout.lua".source = ./river-layout.lua;
  };
  systemd.user.targets.river-session.Unit.BindsTo = "graphical-session.target";
}
