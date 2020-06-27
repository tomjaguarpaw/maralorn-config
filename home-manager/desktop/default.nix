{ pkgs, lib, config, ... }:
let inherit (import ../../lib) colors;
in {
  imports =
    [ ./sleep-nag.nix ./kitty.nix ./wallpaper.nix ./gnome.nix ./firefox.nix ];
  m-0.colors = colors;
  home = {
    packages = builtins.attrValues pkgs.desktop-pkgs;
    file.".zprofile".text =
      ". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh";
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
  };
}
