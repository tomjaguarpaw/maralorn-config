flake-inputs:
{
  lib,
  mylib,
  config,
  pkgs,
  ...
}:
let
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in
{
  imports =
    [
      (flake-inputs.secrets.lib.vpn "apollo")
      flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
      ./hardware-configuration.nix
      (import ../../roles/folder-size-exporter.nix {
        folders = [
          "/"
          "/home"
          "/home/maralorn"
          "/home/maralorn/media"
          "/home/maralorn/git"
        ];
      })
    ]
    ++ mylib.nixFromDirs [
      ../../roles/clients
      ../../roles/laptops
      ../../roles/not-home
      ../../roles/all
      ../../roles/apollo
      ../../roles/metal
    ];

  networking.hostName = "apollo";

  systemd.services.throttled.path = [ pkgs.kmod ];

  services = {
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
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      cert = config.age.secrets."syncthing/apollo/cert.pem".path;
      key = config.age.secrets."syncthing/apollo/key.pem".path;
      settings = syncthing.declarativeWith [
        "hera"
        "zeus"
        "pegasus"
        "hephaistos"
        "athene"
      ] "/home/maralorn/media";
    };
  };
  system.stateVersion = "19.09";
}
