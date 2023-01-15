{
  lib,
  config,
  pkgs,
  ...
}: let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts prefix;
  inherit ((import ../../../nix/sources.nix)) nixos-hardware;
  inherit (import ../../../common/common.nix {inherit pkgs;}) syncthing;
  vpn = (import ../../../private.nix).privateValue (_: _: {}) "vpn";
in {
  imports = [
    "${nixos-hardware}/lenovo/thinkpad/t480s"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/earlyoom.nix
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
    (vpn "apollo")
  ];
  systemd.services.lenovo_fix.path = [pkgs.kmod];

  environment.systemPackages = [
    pkgs.networkmanagerapplet # For when the gnome dialog sucks in asking for a wifi password.
  ];

  networking = {
    hostName = "apollo";
    domain = "m-0.eu";
    networkmanager.enable = true;
    firewall = {
      allowedTCPPorts = [
        4713 # pulseaudio
      ];
      allowedUDPPorts = [
        4713 # pulseaudio
      ];
    };
    wireguard.interfaces = {
      m0wire = {
        allowedIPsAsRoutes = false;
        ips = ["${hosts.apollo-wg}/112" "${hosts.vpn.apollo}/64"];
        privateKeyFile = pkgs.privatePath "wireguard/apollo-private";
        peers = [
          {
            publicKey = wireguard.pub.hera;
            allowedIPs = ["::/0"];
            # endpoint =
            #  "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
            endpoint = "[${hosts.hera-v4}]:${builtins.toString wireguard.port}";
            presharedKeyFile = pkgs.privatePath "wireguard/psk";
            persistentKeepalive = 25;
          }
        ];
        postSetup = ["${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire"];
      };
    };
  };

  services = {
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
    snapper = {
      configs.home = {
        subvolume = "/home";
        extraConfig = ''
          TIMELINE_MIN_AGE="3600"
          TIMELINE_LIMIT_WEEKLY="4"
          TIMELINE_LIMIT_MONTHLY="1"
          TIMELINE_LIMIT_YEARLY="0"
          TIMELINE_CREATE="yes"
          TIMELINE_CLEANUP="yes"
        '';
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
        cert = pkgs.privatePath "syncthing/apollo/cert.pem";
        key = pkgs.privatePath "syncthing/apollo/key.pem";
      }
      // syncthing.declarativeWith ["hera" "zeus" "pegasus"] "/home/maralorn/media";
  };
  systemd.services.NetworkManager-wait-online.enable = false;
  system.stateVersion = "19.09";
}
