{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.
# dropbearkey -t rsa -f /etc/nixos/hosts/<hostname>/secret/boot_rsa

let
  inherit (config.m-0.private) me;
  inherit (import ../../common/common.nix { inherit pkgs; }) syncthing;
in {

  imports = [
    ./hardware-configuration.nix
    ../../system
    ../../system/test-timer.nix
    ../../system/standalone
    ../../system/server
    ../../system/git.nix
    ../../system/riot.nix
    ../../system/mathechor.de.nix
    ../../system/monitoring
    ../../system/blog.nix
    ../../system/email2matrix.nix
    ../../system/matrix-synapse.nix
    ../../system/coturn.nix
    ../../system/serve-store.nix
    ../../system/go-neb.nix
    ./web.nix
    ./mail.nix
    ./boot.nix
    ./cloud.nix
    ./network.nix
    ./secret
  ];
  m-0.monitoring = [{
    name = "hera";
    host = "hera-intern:9100";
  }];

  systemd.package = pkgs.systemd-next;
  programs = {
    ssh.extraConfig = ''
      Host fb04*.mathematik.tu-darmstadt.de
        ProxyJump brandy@gwres1.mathematik.tu-darmstadt.de
    '';
  };
  systemd.services."pg_backup" = {
    script = let name = "matrix-synapse";
    in ''
      ${pkgs.postgresql}/bin/pg_dump matrix-synapse > /var/lib/db-backup-dumps/tmp/${name}
      ${pkgs.coreutils}/bin/mv /var/lib/db-backup-dumps/tmp/${name} /var/lib/db-backup-dumps/cur/${name}
    '';
    serviceConfig = {
      User = "matrix-synapse";
      Type = "oneshot";
    };
    startAt = "23:00";
  };
  services = {
    borgbackup.jobs = let
      passphrases = (import secret/secrets.nix).borgbackup;
      defaultBackup = {
        doInit = false;
        compression = "zstd,5";
        exclude = [ "/var/lib/containers/*/var/lib/nextcloud/data/appdata_*" ];
        paths = [
          "/media"
          "/var/lib/containers/mail/var/vmail"
          "/var/lib/containers/chor-cloud/var/lib/nextcloud/data"
          "/var/lib/containers/cloud/var/lib/nextcloud/data"
          "/var/lib/matrix-synapse"
          "/var/lib/db-backup-dumps/cur"
          "/var/lib/gitolite"
          "/var/lib/taskserver"
        ];
      };
    in {
      fb04217 = defaultBackup // {
        encryption = {
          mode = "keyfile-blake2";
          passphrase = passphrases.fb04217;
        };
        extraArgs = "--remote-path=bin/borg";
        repo =
          "brandy@fb04217.mathematik.tu-darmstadt.de:/media/maralorn-backup/hera-borg-repo";
      };
      cysec = defaultBackup // {
        encryption = {
          mode = "keyfile-blake2";
          passphrase = passphrases.cysec;
        };
        repo = "maralorn@borg.cysec.de:/srv/cube/maralorn/hera-borg-repo";
      };
    };
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
      declarative = syncthing.declarativeWith [ "apollo" ] "/media" // {
        cert = "/etc/nixos/hosts/hera/secret/syncthing/cert.pem";
        key = "/etc/nixos/hosts/hera/secret/syncthing/key.pem";
      };
    };
  };
  boot.kernel.sysctl = { "fs.inotify.max_user_watches" = 204800; };
  systemd.tmpfiles.rules = [ "Z /media 0750 maralorn nginx - -" ];

  users.users.choreutes = {
    linger = true;
    description = "choreutes";
    isNormalUser = true;
    uid = 1001;
    extraGroups = [ "wheel" "systemd-journal" ];
    passwordFile = "/etc/nixos/hosts/hera/secret/pw-choreutes";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
