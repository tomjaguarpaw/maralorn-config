{ pkgs, config, ... }:
let
  adminCreds = pkgs.privateValue {
    adminpass = "";
    dbpass = "";
    adminuser = "";
  } "nextcloud-admin";
  nextcloudServices = hostname: {
    nextcloud-pg-backup = {
      script =
        let
          name = "nextcloud-psql-${hostname}";
        in
        ''
          ${config.services.postgresql.package}/bin/pg_dump nextcloud > /var/lib/db-backup-dumps/${name}
        '';
      serviceConfig = {
        User = "nextcloud";
        Type = "oneshot";
      };
    };
    nextcloud-setup = {
      requires = [
        "postgresql.service"
        "redis.service"
      ];
      after = [
        "postgresql.service"
        "redis.service"
      ];
    };
  };
  nextcloudConf = hostname: {
    enable = true;
    hostName = hostname;
    package = pkgs.nextcloud29;
    maxUploadSize = "10g";
    caching = {
      redis = true;
      apcu = false;
      memcached = false;
    };
    settings.default_phone_region = "DE";
    config = {
      dbtype = "pgsql";
      adminuser = "maralorn";
      adminpassFile = builtins.toFile "nextcloud-adminpass" adminCreds.adminpass;
    };
    autoUpdateApps = {
      enable = true;
      startAt = "20:30";
    };
  };
  mainHostName = "cloud.maralorn.de";
in
{
  systemd.services = nextcloudServices mainHostName;
  services = {
    redis.servers."".enable = true;
    nextcloud = nextcloudConf mainHostName;
    postgresql.ensureDatabases = [ "nextcloud" ];
  };
  users.users.nextcloud.extraGroups = [ "nginx" ];
}
