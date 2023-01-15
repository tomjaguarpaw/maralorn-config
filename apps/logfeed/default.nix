{ pkgs ? import (import nix/sources.nix).nixpkgs {} }:
with pkgs; with haskell.lib; with haskellPackages;
callCabal2nix "logfeed" ./. { purebred-email = doJailbreak (unmarkBroken (dontCheck purebred-email)); }
