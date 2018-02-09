{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  imports = [
    ../snippets/everywhere.nix
    ../snippets/my-systems.nix
    ../snippets/graphical.nix
  ];
  home.packages = with pkgs; [
    # config
    gnome3.gnome_session
    gnome3.gnome-dictionary

    # web
    unstable.firefox
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

    # look & feel
    arc-theme
    arc-icon-theme
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
