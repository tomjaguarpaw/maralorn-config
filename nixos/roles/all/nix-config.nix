{
  pkgs,
  config,
  lib,
  ...
}:
{
  nix = {
    package = pkgs.unstable.nixVersions.unstable;
    nixPath = [ "nixpkgs=flake:pkgs" ];

    registry = {
      pkgs = {
        from = {
          type = "indirect";
          id = "pkgs";
        };
        flake = pkgs.flake-inputs.nixos-unstable;
      };
      stable-pkgs = {
        from = {
          type = "indirect";
          id = "stable-pkgs";
        };
        flake = pkgs.flake-inputs.nixos-stable;
      };
    };
    settings.trusted-users = [ "maralorn" ];
    optimise = {
      dates = [ ];
      automatic = true;
    };
    distributedBuilds = lib.mkDefault true;
    gc.options = "-d";
  };

  programs.ssh.knownHosts = {
    "build1.darmstadt.ccc.de".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE/oyJPRwW3bJoWKtXSrVOiqMaKq+9yd03+N2PuCbMKv";
    "build2.darmstadt.ccc.de".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOZ7/eZLTfUD7Ejjio+8ivNFb8iyK1CD5Pq8uCDojT+z";
    "build3.darmstadt.ccc.de".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM2D/SwJf46XCoim06lOyO42JqJiTeM8UMkT4bYluJJr";
    "build4.darmstadt.ccc.de".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDu9ZsbUYaCzzZv4vn22KrKi/R9pCfOEe4aYWyLd96C1";
  };

  environment.etc."nix/machines".source = toString (
    pkgs.runCommand "nix-machines" { } ''
      cp $(${lib.getExe pkgs.builders-configurator} ${config.networking.hostName} --without-connection) $out
    ''
  );
}
