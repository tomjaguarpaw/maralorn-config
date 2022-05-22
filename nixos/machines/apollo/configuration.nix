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
    (import ../../roles/monitoring/folder-size-exporter.nix {
      folders = ["/nix"];
      subfolders = ["/home/maralorn/git"];
    })
    (vpn "apollo")
  ];
  systemd.services.lenovo_fix.path = [pkgs.kmod];

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

  programs = {
    adb.enable = true;
    sway.enable = true;
    seahorse.enable = lib.mkForce false;
    dconf.enable = true;
  };

  security.rtkit.enable = true;
  services = {
    #teamviewer.enable = true;
    pipewire = {
      enable = lib.mkForce false;
      #alsa = {
      #enable = true;
      #support32Bit = true;
      #};
      #pulse.enable = true;
      #media-session.enable = true;
    };
    fwupd.enable = true;
    upower.enable = true;
    printing = {
      enable = true;
      drivers = [pkgs.gutenprint pkgs.hplip];
    };
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
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
      firewallFilter = "-i m0wire -p tcp -m tcp -m multiport --dports 9100,9558";
      openFirewall = true;
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
      // syncthing.declarativeWith ["hera" "zeus"] "/home/maralorn/media";
    xserver = {
      enable = true;
      displayManager = {
        autoLogin = {
          enable = true;
          user = "maralorn";
        };
        gdm.enable = true;
      };
      desktopManager.gnome.enable = true;
    };
    gnome = {
      evolution-data-server.enable = lib.mkForce false;
      gnome-keyring.enable = lib.mkForce false;
      at-spi2-core.enable = lib.mkForce false;
      tracker.enable = false;
      tracker-miners.enable = false;
      gnome-online-miners.enable = lib.mkForce false;
    };
  };

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 204800;
  console.keyMap = "neo";

  sound.enable = true;
  #hardware = {
  #opengl = {
  #enable = true;
  #driSupport32Bit = true; # for gw2
  #};
  #pulseaudio = {
  #enable = true;
  #support32Bit = true;
  #tcp = {
  #enable = true;
  #anonymousClients.allowedIpRanges = [ "127.0.0.1" "::1" "192.168.178.0/24" ];
  #};
  #};
  #};

  # virtualisation.docker.enable = true;

  system.stateVersion = "19.09";
}
