{ pkgs, ... }:
let
  my-lib = import ../../lib;
  theme = my-lib.themes.default;
in
{
  home.sessionVariables.TERMINAL = "${pkgs.kitty}/bin/kitty";
  home.packages = [
    (pkgs.runCommandLocal "fake-gnome-terminal" { } ''
      mkdir -p $out/bin
      ln -s ${pkgs.kitty}/bin/kitty $out/bin/gnome-terminal
    '')
  ];
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

      foreground = theme.primary.foreground;
      background = theme.primary.background;

      selection_foreground = theme.primary.background;
      selection_background = theme.primary.foreground;
      background_opacity = "0.9";
      background_tint = "0.9";

      # black
      color0 = theme.normal.white;
      color8 = theme.bright.white;

      # red
      color1 = theme.normal.red;
      color9 = theme.bright.red;

      # green
      color2 = theme.normal.green;
      color10 = theme.bright.green;

      # yellow
      color3 = theme.normal.yellow;
      color11 = theme.bright.yellow;

      # blue
      color4 = theme.normal.blue;
      color12 = theme.bright.blue;

      # magenta
      color5 = theme.normal.magenta;
      color13 = theme.bright.magenta;

      # cyan
      color6 = theme.normal.cyan;
      color14 = theme.bright.cyan;

      color7 = theme.normal.black;
      color15 = theme.bright.black;
    };
  };
}
