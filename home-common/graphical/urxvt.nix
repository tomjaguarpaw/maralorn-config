{ pkgs, lib, config, ... }:
let
  colors = config.common.colors;
in {
  home = {
    packages = with pkgs; [
      rxvt_unicode-with-plugins
    ];
  };
  xresources.properties = {
    "*transparent"  = true;
    "*tintColor"    = colors.background;
    "*scrollBar"    = false;
    "*urgentOnBell" = true;
    "*background"   = colors.background;
    "*foreground"   = colors.foreground;
    "*color0"  = colors.black;
    "*color8"  = colors.brightBlack;
    "*color1"  = colors.red;
    "*color9"  = colors.brightRed;
    "*color2"  = colors.green;
    "*color10" = colors.brightGreen;
    "*color3"  = colors.yellow;
    "*color11" = colors.brightYellow;
    "*color4"  = colors.blue;
    "*color12" = colors.brightBlue;
    "*color5"  = colors.magenta;
    "*color13" = colors.brightMagenta;
    "*color6"  = colors.cyan;
    "*color14" = colors.brightCyan;
    "*color7"  = colors.white;
    "*color15" = colors.brightWhite;
    "*boldFont" = "";
  };
}
