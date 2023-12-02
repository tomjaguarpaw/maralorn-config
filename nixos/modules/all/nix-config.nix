{
  pkgs,
  config,
  lib,
  ...
}:
{
  nix = {
    nixPath = ["nixpkgs=flake:pkgs"];

    registry.pkgs = {
      from = {
        type = "indirect";
        id = "pkgs";
      };
      flake = pkgs.flake-inputs.nixos-unstable;
    };
    settings.trusted-users = ["maralorn"];
    optimise = {
      dates = [];
      automatic = true;
    };
    distributedBuilds = lib.mkDefault true;
    gc.options = "-d";
  };

  environment.etc."nix/machines".source = toString (
    pkgs.runCommand "nix-machines" {} ''
      cp $(${lib.getExe pkgs.builders-configurator} ${config.networking.hostName} --without-connection) $out
    ''
  );
}
