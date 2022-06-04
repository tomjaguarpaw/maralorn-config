{pkgs, ...}: let
  inherit (import ../../lib) colors;
in {
  m-0.colors = colors;
  home = {
    packages = builtins.attrValues pkgs.desktop-pkgs;
    file.".zprofile".text = ". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh";
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Flat-Remix-Blue-Light";
      package = pkgs.flat-remix-icon-theme;
    };
    theme = {
      name = "Flat-Remix-GTK-Blue-Light-Solid";
      package = pkgs.flat-remix-gtk;
    };
    gtk3.bookmarks = [
      "ftp://fluffy.lo.m-0.eu"
    ];
  };
}
