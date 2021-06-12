{ pkgs ? import (import nix/sources.nix).nixpkgs {} }:
pkgs.haskellPackages.callCabal2nix "logfeed" ./. {}
