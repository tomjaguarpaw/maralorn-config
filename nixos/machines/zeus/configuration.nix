flake-inputs:
{
  config,
  pkgs,
  lib,
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
      ./hardware-configuration.nix
      ../../roles
      ../../roles/fonts.nix
      ../../roles/metal.nix
      ../../roles/standalone
    ]
    ++ flake-inputs.self.nixFromDirs [
      ../../modules/zeus
      ../../modules/clients
      ../../modules/all
    ];

  age.identityPaths = [ "/disk/persist/etc/ssh/ssh_host_ed25519_key" ];

  nix.distributedBuilds = false;

  fileSystems =
    let
      btrfsOptions = {
        options = [
          "compress=zstd"
          "autodefrag"
          "noatime"
        ];
      };
    in
    {
      "/disk" = {
        neededForBoot = true;
      } // btrfsOptions;
      "/boot" = btrfsOptions;
      "/nix" = btrfsOptions;
    };

  environment.etc = {
    nixos.source = "/disk/persist/maralorn/git/config";
    machine-id.source = "/disk/persist/machine-id";
  };

  systemd.tmpfiles.rules = [
    "d /disk/persist/root 700 root root - -"
    "z / 755 - - - -"
    "d /disk/persist/home/maralorn 700 maralorn users - -"
    "d /disk/volatile/home/maralorn 700 maralorn users - -"
    "Z /home/maralorn - maralorn users - -"
  ];

  environment.persistence."/disk/persist" = {
    directories = [
      "/etc/ssh"
      "/var/lib/nixos" # Nixos has state to track userids
      "/var/lib/bluetooth" # Bluetooth pairing date
      "/var/lib/tailscale" # VPN login state
      "/root/.ssh"
    ];
    users.maralorn.directories = [
      ".cache/rbw" # Save user login
      "Games"
    ];
  };
  environment.persistence."/disk/volatile" = {
    users.maralorn.directories = [
      ".factorio" # Factorio save games and login
      ".config/heroic" # Login data
      ".local/state/wireplumber" # For volume levels
    ];
  };

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
        ]
        "/disk/persist/maralorn/media";
    #minecraft-server = {
    #  enable = true;
    #  openFirewall = true;
    #  eula = true;
    #  dataDir = "/disk/persist/minecraft";
    #};
  };
  system.stateVersion = "21.05";
}
