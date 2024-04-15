{ lib, options, ... }:
{
  services.couchdb = {
    enable = true;
    adminUser = "admin";
    adminPass = "admin";
    extraConfig = lib.generators.toINI { } {
      chttpd_auth.timeout = 604800; # one week
      log.writer = "journald"; # to prevent redundant timestamps
    };
  };
  environment = lib.optionalAttrs (options.environment ? persistence) {
    persistence.snapshoted.directories = [ "/var/lib/couchdb" ];
  };
}
