{ pkgs, ... }:
let
  my-lib = import ../../../lib;
  inherit (my-lib) colors;
in {
  home.sessionVariables.TERMINAL = "${pkgs.kitty}/bin/kitty";
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+plus" = "change_font_size all +1.0";
      "ctrl+minus" = "change_font_size all -1.0";
    };
    settings = {
      linux_display_server = "wayland"; # Causes ugly decorations
      hide_window_decorations = true;
      strip_trailing_spaces = "always";

      font_size = "10.5";

      foreground = colors.foreground;
      background = colors.background;
      background_opacity = "0.9";

      selection_foreground = colors.background;
      selection_background = colors.foreground;

      # black
      color0 = colors.black;
      color8 = colors.brightBlack;

      # red
      color1 = colors.red;
      color9 = colors.brightRed;

      # green
      color2 = colors.green;
      color10 = colors.brightGreen;

      # yellow
      color3 = colors.yellow;
      color11 = colors.brightYellow;

      # blue
      color4 = colors.blue;
      color12 = colors.brightBlue;

      # magenta
      color5 = colors.magenta;
      color13 = colors.brightMagenta;

      # cyan
      color6 = colors.cyan;
      color14 = colors.brightCyan;

      color7 = colors.white;
      color15 = colors.brightWhite;
    };
  };
}
