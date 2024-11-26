flake-inputs:
{ mylib, ... }:
{
  imports =
    [ flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad ]
    ++ mylib.nixFromDirs [
      ../../roles/hephaistos
      ../../roles/clients
      ../../roles/laptops
      ../../roles/not-home
      ../../roles/all
      ../../roles/impermanent
      ../../roles/beefs
      ../../roles/metal
      ../../roles/new-sync
    ];

  networking = {
    hostName = "hephaistos";
    hostId = "f593e579";
  };

  system.stateVersion = "23.05";
}
