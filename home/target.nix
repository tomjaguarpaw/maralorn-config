let
  inherit (import (import ../nix/sources.nix).nixpkgs { }) lib pkgs;
  modes = import ./modes.nix;
  home-manager = import <home-manager/home-manager/home-manager.nix>;
  buildHomeManager = attr:
    (home-manager {
      confPath = ../home.nix;
      confAttr = attr;
    }).activationPackage;
in lib.mapAttrs (host: configs:
  pkgs.runCommand "${host}-modes" { } ''
    mkdir $out
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList
      (mode: config: "ln -s ${buildHomeManager "${host}-${mode}"} $out/${mode}")
      configs)}
  '') modes
