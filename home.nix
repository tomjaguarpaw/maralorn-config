let
  inherit (import (import ./nix/sources.nix).nixpkgs { }) lib;
  modes = import home/modes.nix;
in lib.listToAttrs (lib.flatten (lib.mapAttrsToList (host: configs:
  lib.mapAttrsToList (mode: config: {
    name = "${host}-${mode}";
    value = config;
  }) configs) modes))
