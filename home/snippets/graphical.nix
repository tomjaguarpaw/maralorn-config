{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  tasktree = with pkgs; with rustPlatform; buildRustPackage rec {
    name = "tasktree";
    version = "0.1.0";
    src = ~/data/aktuell/it/code/tasktree;
    depsSha256 = "1p06yqrwc3nlf9jsd50ic76qvkg9hnfb5bgnqwzrna941dk4v4sj";
    cargoSha256 = "1p06yqrwc3nlf9jsd50ic76qvkg9hnfb5bgnqwzrna941dk4v4sj";

    buildInputs = [ gnome3.gtk atk cairo gdk_pixbuf glib pango ];
  };
in {
  home.packages = with pkgs; [
    vimPlugins.vimtex
    redshift
    python27Packages.syncthing-gtk
    rxvt_unicode
    tasktree
    gnome3.gnome_terminal
  ];
}
