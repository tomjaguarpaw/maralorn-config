{ lib, pkgs, config, ...}:
with lib;
{

options.m-0.rustdev.enable = mkEnableOption "Rust Dev";

config = mkIf config.m-0.rustdev.enable {
  home.packages = with pkgs; [
    gnome3.glade
    rustracer
    cargo
    gcc
    binutils-unwrapped
    rustfmt
    carnix
  ];
};

}
