{ config, pkgs, lib, ... }:

let
  #wireguard = import ../../../common/wireguard.nix;
  #inherit (config.m-0) hosts prefix;
  #nixos-unstable = (import ../../../nix/sources.nix).nixos-unstable;
  #inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
in
{

  #disabledModules = [
  #  "services/misc/home-assistant.nix"
  #];

  imports = [
    #"${nixos-unstable}/nixos/modules/services/misc/home-assistant.nix"
    ./hardware-configuration.nix
    ../../roles
    ../../roles/admin.nix
    ../../roles/fonts.nix
    ../../roles/earlyoom.nix
    ../../roles/standalone
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
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /home/maralorn/.config 700 maralorn users - -"
    "z / 755 - - - -"
    "Z /home/maralorn - maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /disk/persist/var/lib/hass - - - - -"
    "d /tmp/scans/scans 777 ftp ftp - -"
    "L+ /var/lib/waydroid - - - - /disk/persist/var/lib/waydroid"
    "L+ /root/.ssh - - - - /disk/persist/root/.ssh"
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
    interfaces.enp1s0 = {
      ipv6.addresses = [{ address = "fdc0:1::2"; prefixLength = 64; }];
      useDHCP = true;
    };
    #wireguard.interfaces = {
    #  m0wire = {
    #    allowedIPsAsRoutes = false;
    #    ips = [ "${hosts.zeus-wg}/112" ];
    #    privateKeyFile = "/disk/persist/wireguard-private-key";
    #    peers = [
    #      {
    #        publicKey = wireguard.pub.hera;
    #        allowedIPs = [ "::/0" ];
    #        endpoint = "[${hosts.hera-wg-host}]:${builtins.toString wireguard.port}";
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
    ssh = {
      startAgent = true;
    };
  };

  security.rtkit.enable = true;
  hardware.printers.ensurePrinters = [
    {
      name = "Klio";
      location = "Wohnzimmer";
      description = "Brother MFC-L3750CDW";
      deviceUri = "ipp://klio.lo.m-0.eu/ipp";
      model = "everywhere";
    }
  ];
  services = {
    home-assistant = {
      enable = true;
      configDir = "/disk/persist/var/lib/hass";
      config = {
        met = { };
        default_config = { };
        zha = { };
        ipp = { };
        brother = { };
      };
    };
    fwupd.enable = true;
    #upower.enable = true;
    printing.enable = true;
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
    #prometheus.exporters.node = {
    #  firewallFilter = "-i m0wire -p tcp -m tcp -m multiport --dports 9100,9558";
    #  openFirewall = true;
    #};
    #syncthing = {
    #  enable = true;
    #  group = "users";
    #  user = "maralorn";
    #  openDefaultPorts = true;
    #  configDir = "/disk/persist/syncthing";
    #} // syncthing.declarativeWith [ "hera" "apollo" ] "/disk/persist/maralorn/media";
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
