flake-inputs: {
  config,
  pkgs,
  lib,
  ...
}: let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts;
  localAddress = "fdc0:1::2";
in {
  imports = [
    (import ../../roles/home-assistant flake-inputs)
    (import ../../roles/home-manager.nix flake-inputs)
    ../../roles
    ../../roles/admin.nix
    ../../roles/earlyoom.nix
    ../../roles/fonts.nix
    ../../roles/metal.nix
    ../../roles/standalone
    ./hardware-configuration.nix
  ];

  age.identityPaths = ["/disk/persist/etc/ssh/ssh_host_ed25519_key"];

  fileSystems = let
    btrfsOptions = {options = ["compress=zstd" "autodefrag" "noatime"];};
  in {
    "/disk" = {neededForBoot = true;} // btrfsOptions;
    "/nix" = btrfsOptions;
  };

  environment.etc = {
    nixos.source = "/disk/persist/maralorn/git/config";
    machine-id.source = "/disk/persist/machine-id";
  };

  systemd.services = {
    ensure-printers.serviceConfig.SuccessExitStatus = "0 1";
  };

  systemd.tmpfiles.rules = [
    "d /backup 700 borg borg - -"
    "d /disk/persist/root 700 root root - -"
    "d /disk/persist/root/.ssh 700 root root - -"
    "d /disk/persist/etc/ssh 755 root root - -"
    "d /disk/persist/var/lib/nixos 755 root root - -"
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /home/maralorn/.config 700 maralorn users - -"
    "z / 755 - - - -"
    "Z /home/maralorn - maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /tmp/scans/scans 777 ftp ftp - -"
    "L+ /root/.ssh - - - - /disk/persist/root/.ssh"
    "L+ /etc/ssh - - - - /disk/persist/etc/ssh"
    "L+ /var/lib/nixos - - - - /disk/persist/var/lib/nixos"
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
      allowedUDPPorts = [631];
      allowedTCPPorts = [21 80 631];
      allowedTCPPortRanges = [
        {
          from = 51000;
          to = 51999;
        }
      ];
    };
    interfaces.enp1s0 = {
      ipv6.addresses = [
        {
          address = localAddress;
          prefixLength = 64;
        }
      ];
      useDHCP = true;
    };
    wireguard.interfaces = {
      m0wire = {
        ips = ["${hosts.vpn.fluffy}/64"];
        privateKeyFile = "/disk/persist/wireguard-private-key";
        peers = [
          {
            publicKey = wireguard.pub.hera;
            allowedIPs = ["${hosts.vpn.prefix}::/64"];
            endpoint = "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
            presharedKeyFile = config.age.secrets."wireguard/psk".path;
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
    borgbackup.repos.hera = {
      path = "/backup/hera-borg-repo";
      authorizedKeys = pkgs.privateValue ["dummy-key"] "backup-ssh-keys";
    };
    printing = {
      enable = true;
      allowFrom = ["all"];
      listenAddresses = ["[${localAddress}]:631"];
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
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
