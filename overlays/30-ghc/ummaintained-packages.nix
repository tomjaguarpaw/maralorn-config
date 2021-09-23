let
  pkgs = import (import ../../nix/sources.nix).nixos-unstable { };
  inherit (pkgs) lib;
  myPkgs = (import ./packages.nix).makeHaskellPackages (pkgs.haskellPackages);
in
lib.attrNames (lib.filterAttrs (_: pkg: [ ] == pkg.meta.maintainers or [ ]) myPkgs)

