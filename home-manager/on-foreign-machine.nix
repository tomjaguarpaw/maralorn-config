{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript;
  sources = import ../nix/sources.nix;
in
{
  home = {
    sessionVariables = {
      NIX_PATH = "$HOME/.nix-path";
    };
    file = {
      home-manager-source = {
        target = ".nix-path/home-manager";
        source = sources.home-manager;
      };
      nixpkgsr-source = {
        target = ".nix-path/nixpkgs";
        source = sources.nixpkgs;
      };
      nixos = {
        target = ".nix-path/nixos";
        source = sources.nixpkgs;
      };
      unstable = {
        target = ".nix-path/unstable";
        source = sources.unstable;
      };
    };
  };
}
