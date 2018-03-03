with import <nixpkgs> {};
{ pkgs, ... }:
let
   desktopItem = makeDesktopItem {
    name = "Tasktree";
    exec = "tasktree";
    icon = "tasktree";
    comment = "A taskwarrior UI";
    desktopName = "Tasktree";
    genericName = "Tasktree";
    categories = "Office;";
  };
  tasktree = with pkgs; with rustPlatform; buildRustPackage rec {
    name = "tasktree";
    version = "abb312f";
    src = fetchFromGitHub {
      rev = version;
      owner = "maralorn";
      repo = "tasktree";
      sha256 = "139xjvi7b62k3075b4md9hdkb1xafhhiyz2yhbb96d73j1gkqs77";
    };
    depsSha256 = "1iw9n1bj7h1v6nz2m3y6045qjavvim3hk5cli3y8x2zakmx88mia";
    cargoSha256 = "1iw9n1bj7h1v6nz2m3y6045qjavvim3hk5cli3y8x2zakmx88mia";

    propagatedBuildInputs = [ gnome3.gtk atk cairo gdk_pixbuf glib pango ];
    postInstall = ''
      function installIcon () {
          mkdir -p $out/share/icons/hicolor/$1/apps/
          cp icons/$1.png $out/share/icons/hicolor/$1/apps/tasktree.png
      }
      installIcon "16x16"
      installIcon "32x32"
      installIcon "64x64"

      mkdir -p $out/share/applications
      ln -s ${desktopItem}/share/applications/* $out/share/applications/
    '';
  };
in {
  home = {
    packages = with pkgs; [
      vimPlugins.vimtex
      redshift
      python27Packages.syncthing-gtk
      rxvt_unicode
      tasktree
      tilda
    ];
    keyboard = {
      layout = "de";
      variant = "neo";
    };
  };
  gtk = {
    enable = true;
    iconeTheme = {
      name = "Arc";
      package = "arc-icon-theme";
    };
    theme = {
      name = "Arc";
      packages = "arc-theme";
    };
  };
  service = {
    compton = {
      enable = true;
      fade = true;
      shadow = true;
    };
    dunst = {
      enable = true;
    };
    gnome-keyring = {
      enable = true;
      components = [
        "secrets"
        "ssh"
      ];
    };
    gpg-agent = {
      enable = true;
    };
    network-manager-applet.enable = true;
    redshift = {
      enable = true;
      temperature.day = 6500;
      latitude = 49.86667;
      longitude = 8.65;
    };
    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.i3lock}/bin/i3lock";
    };
  };
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      gaps = {
        inner = 3;
        outer = 6;
        smartBorders = "on";
      };
    };
  };
}
