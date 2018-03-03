{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {

  imports = [
    ../../snippets/everywhere.nix
    ../../snippets/my-systems.nix
    ../../snippets/graphical.nix
    ../../snippets/latex.nix
  ];

  programs = {
    firefox = {
      enable = true;
      package = unstable.firefox;
    };
  };

  services = {
    udiskie = {
      enable = true;
      notify = true;
      tray = "auto";
    };
  };

  home.packages = with pkgs; [
    # web
    chromium

    # tools & office
    gimp
    imagemagick
    libreoffice-fresh
    pandoc
    xournal
    musescore
    handbrake
    octave

    # dev
    rustup
    gnome3.glade

    # look & feel
    libertine
    nerdfonts

    # media
    ncmpcpp
    pavucontrol
    deluge
    mpd
    gmpc
    calibre
    gnome-mpv
  ];
}
