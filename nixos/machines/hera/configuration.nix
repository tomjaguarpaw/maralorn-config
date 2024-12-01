_:
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
      (import ../../roles/folder-size-exporter.nix {
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
      ../../roles/hera
      ../../roles/not-home
      ../../roles/servers
      ../../roles/all
    ];

  systemd.services =
    {
      #pg_backup = {
      #  script = lib.concatMapStringsSep "\n" (
      #    name: "${config.services.postgresql.package}/bin/pg_dump ${name} > /var/lib/db-backup-dumps/${name}"
      #  ) config.services.postgresql.ensureDatabases;
      #  serviceConfig = {
      #    User = "postgres";
      #    Type = "oneshot";
      #  };
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
            #${start} pg_backup
            ${lib.concatMapStringsSep "\n" (name: "${start} ${name}") backupJobNames}
            #${pkgs.coreutils}/bin/rm -rf /var/lib/db-backup-dumps/*
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
      map (name: {
        inherit name;
        value = {
          serviceConfig.Type = "oneshot";
        };
      }) backupJobNames
    );
  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
    };
    borgbackup.jobs = backupJobs;
    syncthing = {
      enable = true;
      group = "nginx";
      user = "maralorn";
      openDefaultPorts = true;
      cert = config.age.secrets."syncthing/hera/cert.pem".path;
      key = config.age.secrets."syncthing/hera/key.pem".path;
      settings = syncthing.declarativeWith [
        "apollo"
        "zeus"
        "pegasus"
        "hephaistos"
        "athene"
      ] "/media";
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

  system.stateVersion = "18.03";
}
