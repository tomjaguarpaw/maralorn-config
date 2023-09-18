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
      flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad
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

  networking.hostName = "hephaistos";

  system.stateVersion = "23.05";
}
