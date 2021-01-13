{ lib, config, pkgs, ... }:

let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts prefix;
  nixos-hardware = (import ../../../nix/sources.nix).nixos-hardware;
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
  vpn = (import ../../../private.nix).privateValue ({ ... }: { }) "vpn";
in {

  imports = [
    "${nixos-hardware}/lenovo/thinkpad/t480s"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/boot-key.nix
    ../../roles/standalone
    vpn
  ];

  networking = {
    hostName = "apollo";
    domain = "m-0.eu";
    firewall.allowedTCPPorts = [
      8123 # Weiterleitung von stream.maralorn.de
    ];
    wireguard.interfaces = {
      m0wire = {
        allowedIPsAsRoutes = false;
        ips = [ "${hosts.apollo-wg}/112" ];
        privateKeyFile = pkgs.privatePath "wireguard/apollo-private";
        peers = [{
          publicKey = wireguard.pub.hera;
          allowedIPs = [ "::/0" ];
          # endpoint =
          #  "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
          endpoint = "[${hosts.hera-v4}]:${builtins.toString wireguard.port}";
          presharedKeyFile = pkgs.privatePath "wireguard/psk";
          persistentKeepalive = 25;
        }];
        postSetup =
          [ "${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire" ];
      };
    };
  };

  programs = {
    adb.enable = true;
    sway.enable = true;
    ssh = {
      extraConfig = ''
        Host fb04*.mathematik.tu-darmstadt.de
          ProxyJump brandy@gwres1.mathematik.tu-darmstadt.de
      '';
      startAgent = true;
    };
    seahorse.enable = lib.mkForce false;
    dconf.enable = true;
  };

  services = {
    fwupd.enable = true;
    upower.enable = true;
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.hplip ];
    };
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
    unbound = {
      enable = true;
      extraConfig = ''
        server:
          domain-insecure: dn42.

        forward-zone:
          name: "dn42."
          forward-addr: 172.23.0.53
      '';
    };
    fstrim.enable = true;
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
    prometheus.exporters.node = {
      firewallFilter = "-i m0wire -p tcp -m tcp --dport 9100";
      openFirewall = true;
    };
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      package = pkgs.syncthingNext;
      declarative = syncthing.declarativeWith [ "hera" ] "/home/maralorn/media"
        // {
          cert = pkgs.privatePath "syncthing/apollo/cert.pem";
          key = pkgs.privatePath "syncthing/apollo/key.pem";
        };
    };
    xserver = {
      enable = true;
      displayManager = {
        autoLogin = {
          enable = true;
          user = "maralorn";
        };
        gdm.enable = true;
      };
      desktopManager.gnome3.enable = true;
    };
    gnome3 = {
      evolution-data-server.enable = lib.mkForce false;
      gnome-keyring.enable = lib.mkForce false;
      at-spi2-core.enable = lib.mkForce false;
      tracker.enable = false;
      tracker-miners.enable = false;
      gnome-online-miners.enable = lib.mkForce false;
    };
  };

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 204800;
  networking = { networkmanager.enable = true; };
  console.keyMap = "neo";

  sound.enable = true;
  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true; # for gw2
    };
    pulseaudio = {
      enable = true;
      support32Bit = true;
      tcp = {
        enable = true;
        anonymousClients.allowedIpRanges = [ "127.0.0.1" "::1" ];
      };
    };
  };

  virtualisation.docker.enable = true;

  system.stateVersion = "19.09";
}
