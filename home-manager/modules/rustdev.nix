{ lib, pkgs, config, ...}:
with lib;
let
  unstablePkgs = import <unstable> {};
in {

options.m-0.rustdev.enable = mkEnableOption "Rust Dev";

config = mkIf config.m-0.rustdev.enable {
  home.packages = with pkgs; [
    gnome3.glade
    unstablePkgs.rustracer
    unstablePkgs.cargo
    unstablePkgs.rustc
    gcc
    binutils-unwrapped
    unstablePkgs.carnix
  ];
};

}
