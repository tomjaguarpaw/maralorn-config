{
  pkgs,
  config,
  ...
}: {
  home.sessionVariables.TERMINAL = "${pkgs.kitty}/bin/kitty";
  home.packages = [
    (pkgs.recursiveLinkFarm "fake-gnome-terminal" {
      "bin/gnome-terminal" = config.home.sessionVariables.TERMINAL;
    })
  ];
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+plus" = "change_font_size all +1.0";
      "ctrl+minus" = "change_font_size all -1.0";
    };
    theme = "Catppuccin-Latte";
    settings = {
      enable_audio_bell = false;
      visual_bell_duration = "0.1";
      linux_display_server = "wayland"; # Causes ugly decorations
      hide_window_decorations = true;
      window_margin_width = 0;
      strip_trailing_spaces = "always";

      font_size = "8";

      cursor = "#1e66f5";
    };
  };
}
