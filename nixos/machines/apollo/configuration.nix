flake-inputs: {
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (import ../../../common/common.nix {inherit pkgs;}) syncthing;
in {
  imports = [
    (flake-inputs.secrets.lib.vpn "apollo")
    flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/boot-key.nix
    ../../roles/standalone
    ../../roles/metal.nix
    ../../roles/display-server.nix
    (import ../../roles/monitoring/folder-size-exporter.nix {
      folders = [
        "/"
        "/home"
        "/home/maralorn"
        "/home/maralorn/media"
        "/home/maralorn/git"
      ];
    })
  ];

  environment.systemPackages = [
    pkgs.networkmanagerapplet # For when the gnome dialog sucks in asking for a wifi password.
  ];

  networking = {
    hostName = "apollo";
    domain = "m-0.eu";
    networkmanager.enable = true;
  };

  systemd = {
    network.wait-online.enable = false;
    services = {
      NetworkManager-wait-online.enable = false;
      throttled.path = [pkgs.kmod];
    };
  };

  services = {
    fprintd.enable = true;
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
    snapper = {
      configs.home = {
        SUBVOLUME = "/home";
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
        cert = config.age.secrets."syncthing/apollo/cert.pem".path;
        key = config.age.secrets."syncthing/apollo/key.pem".path;
      }
      // syncthing.declarativeWith ["hera" "zeus" "pegasus"] "/home/maralorn/media";
  };
  system.stateVersion = "19.09";
}
