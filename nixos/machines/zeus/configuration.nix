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
      ./../../roles/servers
    ];

  systemd.tmpfiles.rules = [ "Z /home/maralorn - maralorn users - -" ];

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
        "apollo"
        "pegasus"
        "hephaistos"
        "athene"
      ] "/disk/persist/home/maralorn/media";
    };
    #minecraft-server = {
    #  enable = true;
    #  openFirewall = true;
    #  eula = true;
    #  dataDir = "/disk/persist/minecraft";
    #};
  };
  system.stateVersion = "21.05";
}
