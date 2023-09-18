flake-inputs:
{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports =
    [
      (import ../../roles/home-manager.nix flake-inputs)
      flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
      ../../roles
      ../../roles/fonts.nix
      ../../roles/metal.nix
    ]
    ++ flake-inputs.self.nixFromDirs [
      ../../modules/hephaistos
      ../../modules/clients
      ../../modules/not-home
      ../../modules/all
      ../../modules/impermanent
    ];

  networking = {
    hostName = "hephaistos";
    networkmanager.enable = true;
  };

  system.stateVersion = "23.05";
}
