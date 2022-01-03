{ config, pkgs, lib, ... }:

let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts prefix;
  localAddress = "fdc0:1::2";
in
{

  imports = [
    ./hardware-configuration.nix
    ../../roles
    ../../roles/admin.nix
    ../../roles/fonts.nix
    ../../roles/earlyoom.nix
    ../../roles/standalone
    ../../roles/home-assistant-local
  ];

  fileSystems =
    let
      btrfsOptions = { options = [ "compress=zstd" "autodefrag" "noatime" ]; };
    in
    {
      "/disk" = { neededForBoot = true; } // btrfsOptions;
      "/nix" = btrfsOptions;
    };

  environment.etc = {
    nixos.source = "/disk/persist/maralorn/git/config";
    machine-id.source = "/disk/persist/machine-id";
  };

  systemd.services."activate-home-manager" = {
    path = [ pkgs.nix pkgs.dbus ];
    script = ''
      if [[ -e /home/maralorn/.mode ]]; then
        MODE="$(cat /home/maralorn/.mode)"
      else
        MODE="default"
      fi
      /disk/volatile/maralorn/modes/$MODE/activate
    '';
    serviceConfig = {
      Type = "oneshot";
      User = "maralorn";
    };
    wantedBy = [ "multi-user.target" ];
    # Try to avoid race conditions, when the user get’s logged in before activation was completed.
    before = [ "display-manager.service" ];
  };

  systemd.tmpfiles.rules = [
    "d /disk/persist/root 700 root root - -"
    "d /disk/persist/root/.ssh 700 root root - -"
    "d /disk/persist/etc/ssh 755 root root - -"
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /home/maralorn/.config 700 maralorn users - -"
    "z / 755 - - - -"
    "Z /home/maralorn - maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /disk/persist/cups - - - - -"
    "d /tmp/scans/scans 777 ftp ftp - -"
    "L+ /var/lib/cups - - - - /disk/persist/cups"
    "L+ /root/.ssh - - - - /disk/persist/root/.ssh"
    "L+ /etc/ssh - - - - /disk/persist/etc/ssh"
  ];

  boot = {
    loader = {
      grub = {
        device = "nodev";
        efiSupport = true;
        efiInstallAsRemovable = true;
      };
    };
    initrd = {
      luks.devices."crypted-nixos" = {
        # device defined in hardware-configuration.nix
        allowDiscards = true;
        keyFile = "/diskkey.bin";
      };
      secrets = {
        "diskkey.bin" = "/disk/persist/diskkey.bin"; # Key can live on crypted disk, is copied to initrd on install
      };
    };
  };

  networking = {
    hostName = "fluffy";
    domain = "lo.m-0.eu";
    firewall = {
      allowedUDPPorts = [ 631 ];
      allowedTCPPorts = [ 21 80 631 ];
      allowedTCPPortRanges = [{ from = 51000; to = 51999; }];
    };
    interfaces.enp1s0 = {
      ipv6.addresses = [{ address = localAddress; prefixLength = 64; }];
      useDHCP = true;
    };
    wireguard.interfaces = {
      m0wire = {
        ips = [ "${hosts.vpn.fluffy}/64" ];
        privateKeyFile = "/disk/persist/wireguard-private-key";
        peers = [
          {
            publicKey = wireguard.pub.hera;
            allowedIPs = [ "${hosts.vpn.prefix}::/64" ];
            endpoint = "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
            presharedKeyFile = pkgs.privatePath "wireguard/psk";
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };

  programs = {
    ssh = {
      startAgent = true;
    };
  };

  security.rtkit.enable = true;
  hardware.printers = {
    ensureDefaultPrinter = "Klio";
    ensurePrinters = [
      {
        name = "Klio";
        location = "Wohnzimmer";
        description = "Klio (Brother MFC-L3750CDW)";
        deviceUri = "ipp://klio.lo.m-0.eu/ipp";
        model = "everywhere";
      }
    ];
  };
  services = {
    fwupd.enable = true;
    printing = {
      enable = true;
      allowFrom = [ "all" ];
      listenAddresses = [ "[${localAddress}]:631" ];
      extraConf = "ServerAlias *";
      defaultShared = true;
    };
    vsftpd = {
      extraConfig = ''
        pasv_enable=Yes
        pasv_min_port=51000
        pasv_max_port=51999
      '';
      enable = true;
      anonymousUploadEnable = true;
      anonymousUser = true;
      anonymousUserHome = "/tmp/scans";
      anonymousUserNoPassword = true;
      anonymousUmask = "000";
      writeEnable = true;
    };
    fstrim.enable = true;
    snapper = {
      configs.persist = {
        subvolume = "/disk/persist";
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
  };

  #boot.kernel.sysctl."fs.inotify.max_user_watches" = 204800;
  console.keyMap = "neo";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
