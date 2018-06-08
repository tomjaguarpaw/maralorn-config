{ lib, pkgs, config, ...}:
with lib;
let
  rustPkgs = import <rust126> {};
in {

options.m-0.rustdev.enable = mkEnableOption "Rust Dev";

config = mkIf config.m-0.rustdev.enable {
  home.packages = with pkgs; [
#    gnome3.glade
    rustPkgs.rustracer
    rustPkgs.cargo
    rustPkgs.rustc
    gcc
    binutils-unwrapped
    rustfmt
    carnix
  ];
};

}
