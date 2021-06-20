let
  sources = import ../nix/sources.nix;
  inherit (import sources.nixos-unstable { }) lib pkgs;
  modes = import ./machines.nix;
  home-manager = channel: import "${sources.${channel}}/home-manager/home-manager.nix";
  buildHomeManager = host: mode:
    (home-manager (import ../channels.nix).${host}.home-manager-channel {
      confPath = ../home.nix;
      confAttr = "${host}-${mode}";
    }).activationPackage;
  buildModesForHost = host: modes:
    pkgs.runCommandLocal "${host}-modes" { } ''
      mkdir $out
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (mode: config:
        "ln -s ${buildHomeManager host mode} $out/${mode}") modes)}'';
in
lib.mapAttrs buildModesForHost modes
