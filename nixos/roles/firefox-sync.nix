{
  pkgs,
  lib,
  config,
  ...
}: {
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };

  services.mysqlBackup = {
    enable = true;
    databases = ["firefox_syncserver"];
    calendar = "";
    singleTransaction = true;
  };

  services = {
    firefox-syncserver = {
      enable = true;
      secrets = config.age.secrets.firefox-syncserver-secrets.path;
      logLevel = "trace";
      database = {
        name = "firefox_syncserver";
        createLocally = true;
      };
      singleNode = {
        enable = true;
        hostname = "firefox-sync.maralorn.de";
        capacity = 1;
        enableNginx = true;
        enableTLS = true;
      };
    };
    nginx = {
      enable = true;
      virtualHosts."firefox-sync.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
      };
    };
  };
}
