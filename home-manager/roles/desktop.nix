{ pkgs, lib, config, ... }:
let inherit (import ../../lib) colors;
in
{
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
      name = "Arc";
      package = pkgs.arc-theme;
    };
  };
}
