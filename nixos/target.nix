{ withSecrets ? false }:
let
  sources = import ../nix/sources.nix;
  inherit (import sources.nixpkgs { }) lib pkgs;
  machines = lib.attrNames (builtins.readDir ./machines);
  getConfig = hostname:
    args:
    import (./machines + "/${hostname}/configuration.nix") (args // {
      bla = "fünf";
    });
in lib.listToAttrs (map (hostname: {
  name = hostname;
  value =
    (import <nixpkgs/nixos> { configuration = getConfig hostname; }).system;
}) machines)
