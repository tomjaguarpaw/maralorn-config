{ config, pkgs, lib, ... }:

let
  #wireguard = import ../../../common/wireguard.nix;
  #inherit (config.m-0) hosts prefix;
  #nixos-hardware = (import ../../../nix/sources.nix).nixos-hardware;
  #inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
  #vpn = (import ../../../private.nix).privateValue ({ ... }: { }) "vpn";
in
{

  imports = [
    #"${nixos-hardware}/lenovo/thinkpad/t480s"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/admin.nix
    ../../roles/fonts.nix
    ../../roles/earlyoom.nix
    #../../roles/boot-key.nix
    ../../roles/standalone
    #vpn
  ];

  fileSystems =
    let
      btrfsOptions = { options = [ "compress=zstd" "autodefrag" "noatime" ]; };
    in
    {
      "/disk" = { neededForBoot = true; } // btrfsOptions;
      "/boot" = btrfsOptions;
      "/nix" = btrfsOptions;
    };

  environment.etc."nixos".source = "/disk/persist/maralorn/git/config";

  systemd.services."activate-home-manager" = {
    path = [ pkgs.nix pkgs.dbus ];
    serviceConfig = {
      Type = "oneshot";
      User = "maralorn";
      ExecStart = "/disk/volatile/maralorn/modes/default/activate";
    };
    wantedBy = [ "multi-user.target" ];
  };

  systemd.tmpfiles.rules = [
    "d /disk/persist/root 700 root root - -"
    "d /disk/persist/root/.ssh 700 root root - -"
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /disk/persist/var/lib/bluetooth - - - - -"
    "L+ /var/lib/bluetooth - - - - /disk/persist/var/lib/bluetooth"
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
      kernelModules = [ "amdgpu" ]; # For earlier and better framebuffer
      secrets = {
        "diskkey.bin" = "/disk/persist/diskkey.bin"; # Key can live on crypted disk, is copied to initrd on install
      };
    };
  };

  networking = {
    hostName = "zeus";
    domain = "m-0.eu";
    interfaces.enp34s0.useDHCP = true;
    #wireguard.interfaces = {
    #  m0wire = {
    #    allowedIPsAsRoutes = false;
    #    ips = [ "${hosts.apollo-wg}/112" ];
    #    privateKeyFile = pkgs.privatePath "wireguard/apollo-private";
    #    peers = [
    #      {
    #        publicKey = wireguard.pub.hera;
    #        allowedIPs = [ "::/0" ];
    #        # endpoint =
    #        #  "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
    #        endpoint = "[${hosts.hera-v4}]:${builtins.toString wireguard.port}";
    #        presharedKeyFile = pkgs.privatePath "wireguard/psk";
    #        persistentKeepalive = 25;
    #      }
    #    ];
    #    postSetup =
    #      [ "${pkgs.iproute}/bin/ip route add ${prefix}::/96 dev m0wire" ];
    #  };
    #};
  };

  programs = {
    #adb.enable = true;
    sway.enable = true;
    #ssh = {
    #  extraConfig = ''
    #    Host fb04*.mathematik.tu-darmstadt.de
    #      ProxyJump brandy@gwres1.mathematik.tu-darmstadt.de
    #  '';
    #  startAgent = true;
    #};
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
    #fwupd.enable = true;
    #upower.enable = true;
    #printing = {
    #  enable = true;
    #  drivers = [ pkgs.gutenprint pkgs.hplip ];
    #};
    #unbound.enable = true;
    #fstrim.enable = true;
    #snapper = {
    #  configs.home = {
    #    subvolume = "/home";
    #    extraConfig = ''
    #      TIMELINE_MIN_AGE="3600"
    #      TIMELINE_LIMIT_WEEKLY="4"
    #      TIMELINE_LIMIT_MONTHLY="1"
    #      TIMELINE_LIMIT_YEARLY="0"
    #      TIMELINE_CREATE="yes"
    #      TIMELINE_CLEANUP="yes"
    #    '';
    #  };
    #  cleanupInterval = "15m";
    #  snapshotInterval = "*:00/3:00";
    #};
    #prometheus.exporters.node = {
    #  firewallFilter = "-i m0wire -p tcp -m tcp --dport 9100";
    #  openFirewall = true;
    #};
    #syncthing = {
    #  enable = true;
    #  group = "users";
    #  user = "maralorn";
    #  openDefaultPorts = true;
    #  declarative = syncthing.declarativeWith [ "hera" ] "/home/maralorn/media"
    #    // {
    #    cert = pkgs.privatePath "syncthing/apollo/cert.pem";
    #    key = pkgs.privatePath "syncthing/apollo/key.pem";
    #  };
    #};
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
