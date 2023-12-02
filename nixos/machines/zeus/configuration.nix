flake-inputs:
{
  config,
  pkgs,
  lib,
  mylib,
  ...
}:
let
  inherit (import ../../../common/common.nix {inherit pkgs;}) syncthing;
in
{
  imports =
    [
      (flake-inputs.secrets.lib.vpn "zeus")
      "${flake-inputs.nixos-hardware}/common/gpu/amd/sea-islands"
      ./hardware-configuration.nix
      ../../roles
      ../../roles/fonts.nix
    ]
    ++ mylib.nixFromDirs [
      ./../../modules/all
      ./../../modules/clients
      ./../../modules/zeus
      ./../../modules/impermanent
      ./../../modules/beefs
      ./../../modules/metal
      ./../../modules/servers
    ];

  fileSystems =
    let
      btrfsOptions = {
        neededForBoot = true;
        options = [
          "compress=zstd"
          "autodefrag"
          "noatime"
        ];
      };
    in
    {
      "/disk" = btrfsOptions;
      "/nix" = btrfsOptions;
    };

  systemd.tmpfiles.rules = [
    "z / 755 - - - -"
    "Z /home/maralorn - maralorn users - -"
  ];

  services = {
    snapper = {
      configs.persist = {
        SUBVOLUME = "/disk/persist";
        TIMELINE_MIN_AGE = "3600";
        TIMELINE_LIMIT_WEEKLY = "4";
        TIMELINE_LIMIT_MONTHLY = "1";
        TIMELINE_LIMIT_YEARLY = "0";
        TIMELINE_CREATE = true;
        TIMELINE_CLEANUP = true;
      };
      cleanupInterval = "15m";
      snapshotInterval = "*:00/3:00";
    };
    syncthing =
      {
        enable = true;
        group = "users";
        user = "maralorn";
        openDefaultPorts = true;
        configDir = "/disk/persist/syncthing";
        cert = config.age.secrets."syncthing/zeus/cert.pem".path;
        key = config.age.secrets."syncthing/zeus/key.pem".path;
      }
      // syncthing.declarativeWith
        [
          "hera"
          "apollo"
          "pegasus"
          "hephaistos"
          "athene"
        ]
        "/disk/persist/home/maralorn/media";
    #minecraft-server = {
    #  enable = true;
    #  openFirewall = true;
    #  eula = true;
    #  dataDir = "/disk/persist/minecraft";
    #};
  };
  system.stateVersion = "21.05";
}
