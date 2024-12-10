{ lib, config, ... }:
{
  services.couchdb = {
    enable = true;
    adminUser = "admin";
    adminPass = "admin";
    bindAddress = "::";
    extraConfig = lib.generators.toINI { } {
      chttpd_auth = {
        timeout = 604800; # one week
        max_http_request_size = "80000000";
      };
      couchdb.max_document_size = "80000000";
      log.writer = "journald"; # to prevent redundant timestamps
    };
  };
  environment = lib.mkIf config.has-persistence {
    persistence.snapshoted.directories = [ "/var/lib/couchdb" ];
  };
}
