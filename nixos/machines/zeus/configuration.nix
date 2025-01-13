flake-inputs:
{
  config,
  pkgs,
  lib,
  mylib,
  ...
}:
let
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in
{
  imports =
    [
      (flake-inputs.secrets.lib.vpn "zeus")
      "${flake-inputs.nixos-hardware}/common/gpu/amd/sea-islands"
    ]
    ++ mylib.nixFromDirs [
      ./../../roles/all
      ./../../roles/clients
      ./../../roles/zeus
      ./../../roles/impermanent
      ./../../roles/beefs
      ./../../roles/metal
    ];

  services = {
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      configDir = "/disk/persist/syncthing";
      cert = config.age.secrets."syncthing/zeus/cert.pem".path;
      key = config.age.secrets."syncthing/zeus/key.pem".path;
      settings = syncthing.declarativeWith [
        "hera"
        "pegasus"
        "hephaistos"
        "athene"
      ] "/disk/persist/home/maralorn/media";
    };
  };
  system.stateVersion = "21.05";
}
