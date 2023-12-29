flake-inputs:
{
  config,
  pkgs,
  lib,
  mylib,
  ...
}:
let
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
  backupJobs = pkgs.privateValue { } "borgbackup";
  backupJobNames = map (name: "borgbackup-job-${name}") (lib.attrNames backupJobs);
in
{
  imports =
    [
      (flake-inputs.secrets.lib.vpn "hera")
      ./mail.nix
      ../../roles
      ../../roles/blog.nix
      #../../roles/coturn.nix
      ../../roles/email2matrix.nix
      ../../roles/foundryvtt.nix
      ../../roles/git.nix
      ../../roles/go-neb.nix
      ../../roles/goatcounter.nix
      ../../roles/headscale.nix
      ../../roles/mailman.nix
      ../../roles/matrix-synapse
      ../../roles/miniflux.nix
      ../../roles/monitoring
      ../../roles/nixpkgs-bot.nix
      ../../roles/unbound.nix
      ../../roles/update-postgres.nix
      ./cloud.nix
      ./hardware-configuration.nix
      ./network.nix
      ./web.nix
      (import ../../roles/monitoring/folder-size-exporter.nix {
        folders = [
          "/"
          "/home"
          "/home/maralorn"
          "/media"
          "/var"
          "/var/lib"
          "/var/lib/containers"
          "/var/lib/nextcloud"
          "/var/lib/nextcloud/data"
          "/var/log"
          "/var/vmail"
        ];
      })
    ]
    ++ mylib.nixFromDirs [
      ../../modules/hera
      ../../modules/not-home
      ../../modules/servers
      ../../modules/all
    ];

  m-0.monitoring = [
    {
      name = "hera";
      host = "hera:9100";
    }
  ];

  systemd.services =
    {
      pg_backup = {
        script =
          lib.concatMapStringsSep "\n"
            (
              name: "${config.services.postgresql.package}/bin/pg_dump ${name} > /var/lib/db-backup-dumps/${name}"
            )
            config.services.postgresql.ensureDatabases;
        serviceConfig = {
          User = "postgres";
          Type = "oneshot";
        };
      };
      #bump-config = {
      #  script = ''
      #    ${pkgs.laminar}/bin/laminarc queue bump-config
      #  '';
      #  serviceConfig = {
      #    Type = "oneshot";
      #  };
      #  startAt = "Sat 04:00";
      #};
      night-routines = {
        script =
          let
            start = "${pkgs.systemd}/bin/systemctl start";
          in
          # ${start} mysql-backup -- only needed for firefox-sync
          ''
            set -x
            set +e
            ${start} pg_backup
            ${lib.concatMapStringsSep "\n" (name: "${start} ${name}") backupJobNames}
            ${pkgs.coreutils}/bin/rm -rf /var/lib/db-backup-dumps/*
            ${start} synapse-cleanup
            ${start} nix-gc
            ${start} nix-optimise
          '';
        serviceConfig = {
          Type = "oneshot";
        };
        startAt = "03:00";
      };
    }
    // lib.listToAttrs (
      map
        (name: {
          inherit name;
          value = {
            serviceConfig.Type = "oneshot";
          };
        })
        backupJobNames
    );
  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
    };
    borgbackup.jobs = backupJobs;
    taskserver = {
      enable = true;
      fqdn = "${config.m-0.virtualHosts.taskserver}";
      listenHost = "::";
      organisations."maralorn.de".users = [ "maralorn" ];
    };
    syncthing = {
      enable = true;
      group = "nginx";
      user = "maralorn";
      openDefaultPorts = true;
      cert = config.age.secrets."syncthing/hera/cert.pem".path;
      key = config.age.secrets."syncthing/hera/key.pem".path;
      settings =
        syncthing.declarativeWith
          [
            "apollo"
            "zeus"
            "pegasus"
            "hephaistos"
            "athene"
          ]
          "/media";
    };
  };
  systemd.tmpfiles.rules = [ "Z /media 0770 maralorn nginx - -" ];

  users.users = {
    choreutes = {
      description = "choreutes";
      isNormalUser = true;
      uid = 1001;
      extraGroups = [
        "wheel"
        "systemd-journal"
      ];
      hashedPasswordFile = config.age.secrets.pam-login-password-choreutes.path;
    };
    ved-backup = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDSldCn4LJcIos8PVI7PJZSM5aQ8FoDPUzMTwSHm6NUl root@bach"
      ];
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
