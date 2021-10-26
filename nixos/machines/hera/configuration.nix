{ config, pkgs, lib, ... }:
let
  inherit (config.m-0.private) me;
  inherit (import ../../../common/common.nix { inherit pkgs; }) syncthing;
  backupJobs = pkgs.privateValue { } "borgbackup";
  backupJobNames = map (name: "borgbackup-job-${name}") (lib.attrNames backupJobs);
in
{

  imports = [
    ./hardware-configuration.nix
    ../../roles
    ../../roles/standalone
    ../../roles/server
    ../../roles/git.nix
    ../../roles/element.nix
    ../../roles/mathechor.de.nix
    ../../roles/monitoring
    ../../roles/blog.nix
    ../../roles/email2matrix.nix
    ../../roles/matrix-synapse
    ../../roles/matrix-signal
    ../../roles/coturn.nix
    ../../roles/go-neb.nix
    ../../roles/laminar
    ../../roles/kassandra-server.nix
    ../../roles/foundryvtt.nix
    ../../roles/mailman.nix
    ../../roles/home-assistant
    ../../roles/firefox-sync.nix
    ./web.nix
    ./mail.nix
    ./boot.nix
    ./cloud.nix
    ./network.nix
  ];
  m-0.monitoring = [
    {
      name = "hera";
      host = "hera-intern:9100";
    }
  ];

  programs = {
    ssh = {
      extraConfig = ''
        Host fb04*.mathematik.tu-darmstadt.de
          ProxyJump brandy@gwres1.mathematik.tu-darmstadt.de
      '';
      startAgent = true;
    };
    java.enable = true;
  };
  nixpkgs.config.android_sdk.accept_license = true;
  systemd.services = {
    pg_backup =
      {
        script = lib.concatMapStringsSep "\n"
          (name: "${config.services.postgresql.package}/bin/pg_dump ${name} > /var/lib/db-backup-dumps/${name}")
          config.services.postgresql.ensureDatabases;
        serviceConfig = {
          User = "postgres";
          Type = "oneshot";
        };
      };
    night-routines = {
      script =
        let
          start = "${pkgs.systemd}/bin/systemctl start";
          container = "${pkgs.nixos-container}/bin/nixos-container run";
        in
        ''
          set -x
          set +e
          ${start} pg_backup
          ${container} chor-cloud -- ${start} nextcloud-pg-backup
          ${lib.concatMapStringsSep "\n" (name: "${start} ${name}") backupJobNames}
          ${pkgs.coreutils}/bin/rm -rf /var/lib/db-backup-dumps/*
          ${start} nix-optimise
          if [[ "$(date '+%A')" == "Monday" ]]; then
            ${start} nix-gc
          fi
          ${start} synapse-cleanup
          ${pkgs.laminar}/bin/laminarc queue bump-config
        '';
      serviceConfig = {
        Type = "oneshot";
      };
      startAt = "03:00";
    };
  } // lib.listToAttrs (map (name: { name = name; value = { serviceConfig.Type = "oneshot"; }; }) backupJobNames);
  services = {
    borgbackup.jobs = backupJobs;
    taskserver = {
      enable = true;
      fqdn = "hera.m-0.eu";
      listenHost = "::";
      organisations."maralorn.de".users = [ "maralorn" ];
    };
    syncthing = {
      enable = true;
      group = "nginx";
      user = "maralorn";
      openDefaultPorts = true;
      declarative = syncthing.declarativeWith [ "apollo" "zeus" ] "/media" // {
        cert = pkgs.privatePath "syncthing/hera/cert.pem";
        key = pkgs.privatePath "syncthing/hera/key.pem";
      };
    };
  };
  boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };
  systemd.tmpfiles.rules = [ "Z /media 0770 maralorn nginx - -" ];
  nix.sshServe = {
    protocol = "ssh-ng";
    enable = true;
    keys = pkgs.privateValue [ ] "root-ssh-keys";
  };

  users.users.choreutes = {
    description = "choreutes";
    isNormalUser = true;
    uid = 1001;
    extraGroups = [ "wheel" "systemd-journal" ];
    passwordFile = pkgs.privatePath "pam-login-password-choreutes";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
