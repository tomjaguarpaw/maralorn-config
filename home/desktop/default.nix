{ pkgs, lib, config, ... }:
let
  inherit (import ../../pkgs) desktop-pkgs;
  inherit (import ../../lib) colors;
in {
  imports = [ ./sleep-nag.nix ./kitty.nix ./wallpaper.nix ./gnome.nix ];
  m-0.colors = colors;
  home = {
    packages = builtins.attrValues desktop-pkgs;
    file = {
      ".zprofile".text = ''
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      '';
      ".gnupg/gpg-agent.conf".text =
        "pinentry-program ${pkgs.pinentry.gnome3}/bin/pinentry";
    };
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
