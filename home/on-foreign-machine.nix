{ pkgs, config, lib, ... }:
let inherit (import ../lib) writeHaskellScript sources;
in {
  home = {
    packages = builtins.attrValues (import ../pkgs).foreign-home-pkgs;
    sessionVariables = { NIX_PATH = "$HOME/.nix-path"; };
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
