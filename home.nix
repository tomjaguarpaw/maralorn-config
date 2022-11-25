let
  inherit (import (import ./nix/sources.nix).nixos-unstable {}) lib;
  modes = import home-manager/machines.nix;
in
  lib.listToAttrs (lib.flatten (lib.mapAttrsToList
    (
      host:
        lib.mapAttrsToList
        (mode: config: {
          name = "${host}-${mode}";
          value = config;
        })
    )
    modes))
