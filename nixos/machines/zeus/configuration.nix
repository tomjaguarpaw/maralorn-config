{ config, pkgs, lib, ... }:

let
  wireguard = import ../../../common/wireguard.nix;
  inherit (config.m-0) hosts prefix;
  nixos-hardware = (import ../../../nix/sources.nix).nixos-hardware;
  nixos-unstable = (import ../../../nix/sources.nix).nixos-unstable;
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
  vpn = (import ../../../private.nix).privateValue (_: _: { }) "vpn";
in
{

  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    "${nixos-unstable}/nixos/modules/services/misc/home-assistant.nix"
    "${nixos-hardware}/common/gpu/amd/sea-islands"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/admin.nix
    ../../roles/fonts.nix
    ../../roles/earlyoom.nix
    #../../roles/boot-key.nix
    ../../roles/standalone
    (vpn "zeus")
  ];

  fileSystems =
    let
      btrfsOptions = { options = [ "compress=zstd" "autodefrag" "noatime" ]; };
    in
    {
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
        MODE="orga"
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
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /disk/persist/maralorn/.config/pulse 700 maralorn users - -"
    "d /home/maralorn/.config 700 maralorn users - -"
    "Z /home/maralorn - maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /disk/persist/var/lib/bluetooth - - - - -"
    "d /disk/persist/var/lib/waydroid 777 root root - -"
    "d /disk/persist/minecraft 700 minecraft minecraft - -"
    "L+ /var/lib/bluetooth - - - - /disk/persist/var/lib/bluetooth"
    "L+ /var/lib/waydroid - - - - /disk/persist/var/lib/waydroid"
    "L+ /root/.ssh - - - - /disk/persist/root/.ssh"
  ];

  boot = {
    loader = {
      efi = {
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        # Enabled by default
        device = "nodev"; # Don‘t write masterboot under efi
        efiInstallAsRemovable = true; # Make loader discoverable by filename on efidisk without needing to write efivars to system
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
        "binder_linux"
      ];
      secrets = {
        "diskkey.bin" = "/disk/persist/diskkey.bin"; # Key can live on crypted disk, is copied to initrd on install
      };
    };
  };

  networking = {
    hostName = "zeus";
    domain = "lo.m-0.eu";
    networkmanager.enable = false;
    interfaces.enp34s0 = {
      useDHCP = true;
      ipv6.addresses = [{ address = "fdc0:1::4"; prefixLength = 64; }];
    };
    wireguard.interfaces = {
      m0wire = {
        allowedIPsAsRoutes = false;
        ips = [ "${hosts.zeus-wg}/112" ];
        privateKeyFile = "/disk/persist/wireguard-private-key";
        peers = [
          {
            publicKey = wireguard.pub.hera;
            allowedIPs = [ "::/0" ];
            endpoint = "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
            presharedKeyFile = pkgs.privatePath "wireguard/psk";
            persistentKeepalive = 25;
          }
        ];
        postSetup =
          [ "${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire" ];
      };
    };
  };

  programs = {
    #adb.enable = true;
    sway.enable = true;
    ssh = {
      #  extraConfig = ''
      #    Host fb04*.mathematik.tu-darmstadt.de
      #      ProxyJump brandy@gwres1.mathematik.tu-darmstadt.de
      #  '';
      startAgent = true;
    };
    seahorse.enable = lib.mkForce false;
    dconf.enable = true;
  };

  security.rtkit.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_xanmod;
  virtualisation.waydroid.enable = true;
  services = {
    pipewire.enable = lib.mkForce false;
    fwupd.enable = true;
    #upower.enable = true;
    unbound.enable = true;
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
    syncthing = {
      enable = true;
      group = "users";
      user = "maralorn";
      openDefaultPorts = true;
      configDir = "/disk/persist/syncthing";
    } // syncthing.declarativeWith [ "hera" "apollo" ] "/disk/persist/maralorn/media";
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
    minecraft-server = {
      enable = true;
      openFirewall = true;
      eula = true;
      dataDir = "/disk/persist/minecraft";
    };
  };

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 204800;
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

  system.stateVersion = "21.05";
}
