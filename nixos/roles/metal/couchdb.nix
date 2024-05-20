{ lib, config, ... }:
{
  services.couchdb = {
    enable = true;
    adminUser = "admin";
    adminPass = "admin";
    bindAddress = "::";
    extraConfig = lib.generators.toINI { } {
      chttpd_auth.timeout = 604800; # one week
      log.writer = "journald"; # to prevent redundant timestamps
    };
  };
  environment.persistence.snapshoted.directories = lib.mkIf config.has-persistence [
    "/var/lib/couchdb"
  ];
}
