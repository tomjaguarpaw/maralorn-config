{pkgs ? import (import nix/sources.nix).nixpkgs {}}: let
  inherit (pkgs) haskellPackages;
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [(import ./. {inherit pkgs;})];
    buildInputs = builtins.attrValues {
      inherit (haskellPackages) hlint cabal-install notmuch hsemail;
      inherit (pkgs) coreutils zlib;
    };
  }
