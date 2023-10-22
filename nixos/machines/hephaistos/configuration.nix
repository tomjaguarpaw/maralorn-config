flake-inputs:
{ prelude, ... }:
with prelude; {
  imports =
    [
      flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad
      ../../roles
      ../../roles/fonts.nix
    ]
    ++ nixFromDirs [
      ../../modules/hephaistos
      ../../modules/clients
      ../../modules/laptops
      ../../modules/not-home
      ../../modules/all
      ../../modules/impermanent
      ../../modules/beefs
      ../../modules/metal
      ../../modules/new-sync
    ];

  networking.hostName = "hephaistos";

  system.stateVersion = "23.05";
}
