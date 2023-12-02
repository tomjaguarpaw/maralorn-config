_:
{pkgs, mylib, ...}:
let
  localAddress = "fdc0:1::2";
in
{
  imports =
    [
      ../../roles
      ../../roles/fonts.nix
      ../../roles/home-assistant
    ]
    ++ mylib.nixFromDirs [
      ../../modules/athene
      ../../modules/all
      ../../modules/impermanent
      ../../modules/servers
      ../../modules/metal
      ../../modules/new-sync
    ];

  systemd.services.ensure-printers.serviceConfig.SuccessExitStatus = "0 1";

  systemd.tmpfiles.rules = [
    "d /backup 700 borg borg - -"
    "d /disk/persist/maralorn 700 maralorn users - -"
    "d /home/maralorn/.config 700 maralorn users - -"
    "z / 755 - - - -"
    "Z /home/maralorn - maralorn users - -"
    "d /disk/volatile/maralorn 700 maralorn users - -"
    "d /tmp/scans/scans 777 ftp ftp - -"
  ];

  networking = {
    hostName = "athene";
    domain = "lo.m-0.eu";
    firewall = {
      allowedUDPPorts = [
        631 # cups
      ];
      allowedTCPPorts = [
        21 # ftp for scanner
        80 # http
        443 # https
        631 # cups
        4713 # pulseaudio input
        6600 # mpd
      ];
      allowedTCPPortRanges =
        [
          # also ftp
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
  };
  programs.ssh.startAgent = true;
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
      enable = false; # Printer is currently disfunctional anyway
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
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
