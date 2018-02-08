{ pkgs, ... }:
let
  unstable = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  imports = [
    ../snippets/everywhere.nix
    ../snippets/my-systems.nix
  ];
}
