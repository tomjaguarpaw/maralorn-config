{pkgs ? import (import ../../nix/sources.nix).nixos-unstable {}}:
pkgs.haskellPackages.callCabal2nix "logfeed" ./. {}
