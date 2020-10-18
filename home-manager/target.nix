let
  sources = import ../nix/sources.nix;
  inherit (import sources.nixpkgs { }) lib pkgs;
  modes = import ./machines.nix;
  home-manager = import "${sources.home-manager}/home-manager/home-manager.nix";
  buildHomeManager = attr:
    (home-manager {
      confPath = ../home.nix;
      confAttr = attr;
    }).activationPackage;
  buildModesForHost = host: modes:
    pkgs.runCommandLocal "${host}-modes" { } ''
      mkdir $out
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (mode: config:
        "ln -s ${buildHomeManager "${host}-${mode}"} $out/${mode}") modes)}'';
in lib.mapAttrs buildModesForHost modes
