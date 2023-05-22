flake-inputs:
{ config, pkgs, lib, ... }:
let inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in {
  imports = [
    (flake-inputs.secrets.lib.vpn "zeus")
    "${flake-inputs.nixos-hardware}/common/gpu/amd/sea-islands"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/fonts.nix
    ../../roles/metal.nix
    ../../roles/display-server.nix
    ../../roles/standalone
  ];

  age.identityPaths = [ "/disk/persist/etc/ssh/ssh_host_ed25519_key" ];

  nix.distributedBuilds = false;

  fileSystems = let
    btrfsOptions = { options = [ "compress=zstd" "autodefrag" "noatime" ]; };
  in {
    "/disk" = { neededForBoot = true; } // btrfsOptions;
    "/boot" = btrfsOptions;
    "/nix" = btrfsOptions;
    "/home/maralorn/.config/pulse" = {
      mountPoint = "/home/maralorn/.config/pulse";
      device = "/disk/persist/maralorn/.config/pulse";
      options = [ "bind" ];
    };
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
        MODE="klausur"
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
    "z / 755 - - - -"
    "d /disk/persist/home/maralorn 700 maralorn users - -"
    "d /disk/volatile/home/maralorn 700 maralorn users - -"
    "Z /home/maralorn - maralorn users - -"
    # "d /disk/persist/minecraft 700 minecraft minecraft - -"
    #"d /var/lib/misc 755 - - - -"
  ];

  environment.persistence."/disk/persist" = {
    directories = [
      "/etc/ssh"
      "/var/lib/nixos"
      "/var/lib/bluetooth"
      "/var/lib/tailscale"
      "/root/.ssh"
    ];
    users.maralorn.directories = [ ".cache/rbw" ".factorio" ];
  };

  boot = {
    loader = {
      efi = { efiSysMountPoint = "/boot/efi"; };
      grub = {
        # Enabled by default
        device = "nodev"; # Don‘t write masterboot under efi
        efiInstallAsRemovable =
          true; # Make loader discoverable by filename on efidisk without needing to write efivars to system
        efiSupport = true;
        enableCryptodisk = true;
        backgroundColor = "#000000";
      };
    };
    kernelParams = [ "amdgpu.cik_support=1" ];
    initrd = {
      luks.devices."crypted-nixos" = {
        # device defined in hardware-configuration.nix
        allowDiscards = true;
        keyFile = "/diskkey.bin";
      };
      kernelModules = [
        "amdgpu" # For earlier and better framebuffer
      ];
      secrets = {
        "diskkey.bin" =
          "/disk/persist/diskkey.bin"; # Key can live on crypted disk, is copied to initrd on install
      };
    };
  };

  networking = {
    hostName = "zeus";
    domain = "lo.m-0.eu";
    networkmanager.enable = false;
    interfaces.enp34s0 = {
      useDHCP = true;
      ipv6.addresses = [{
        address = "fdc0:1::4";
        prefixLength = 64;
      }];
    };
    firewall.allowedTCPPorts = [
      6600 # mpd
    ];
    firewall.allowedUDPPorts = [
      34197 # factorio
    ];
    #wireguard.interfaces = {
    #  m0wire = {
    #    allowedIPsAsRoutes = false;
    #    ips = ["${hosts.zeus-wg}/112" "${hosts.vpn.zeus}/64"];
    #    privateKeyFile = config.age.secrets."wireguard/zeus-private".path;
    #    peers = [
    #      {
    #        publicKey = wireguard.pub.hera;
    #        allowedIPs = ["::/0"];
    #        endpoint = "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
    #        presharedKeyFile = config.age.secrets."wireguard/psk".path;
    #        persistentKeepalive = 25;
    #      }
    #    ];
    #    postSetup = ["${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire"];
    #  };
    #};
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
    #prometheus.exporters.node = {
    # firewallFilter = "-i m0wire -p tcp -m tcp -m multiport --dports 9100,9558";
    # openFirewall = lib.mkForce false;
    #};
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      configDir = "/disk/persist/syncthing";
      cert = config.age.secrets."syncthing/zeus/cert.pem".path;
      key = config.age.secrets."syncthing/zeus/key.pem".path;
    } // syncthing.declarativeWith [ "hera" "apollo" "pegasus" ]
      "/disk/persist/maralorn/media";
    #minecraft-server = {
    #  enable = true;
    #  openFirewall = true;
    #  eula = true;
    #  dataDir = "/disk/persist/minecraft";
    #};
  };
  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true; # for gw2
    };
    pulseaudio = {
      support32Bit = true;
      tcp = {
        enable = true;
        anonymousClients.allowedIpRanges = [ "127.0.0.1" "::1" ];
      };
    };
  };
  system.stateVersion = "21.05";
}
