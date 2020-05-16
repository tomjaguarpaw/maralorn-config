{ pkgs, lib, config, ... }:
let
  inherit (import ../../pkgs) desktop-pkgs my-ssh-add;
  inherit (import ../../lib) colors;
in {
  imports =
    [ ./rofi.nix ./ssh-agent.nix ./sleep-nag.nix ./kitty.nix ./wallpaper.nix ./gnome.nix ];
  m-0 = {
    terminal = "${desktop-pkgs.terminal}/bin/terminal";
    colors = colors;
  };
  home = {
    packages = builtins.attrValues desktop-pkgs;
    file.".zprofile".text = ''
      . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    '';
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
