{pkgs, config, ...}:
{
  services = {
    mysql = {
      enable = true;
      package = pkgs.mariadb;
    };

    mysqlBackup = {
      enable = true;
      databases = ["firefox_syncserver"];
      calendar = "";
      singleTransaction = true;
    };

    firefox-syncserver = {
      enable = true;
      secrets = config.age.secrets.firefox-syncserver-secrets.path;
      database = {
        name = "firefox_syncserver";
        createLocally = true;
      };
      singleNode = {
        enable = true;
        hostname = config.m-0.virtualHosts.firefox-sync;
        capacity = 1;
        enableTLS = true;
        enableNginx = true;
      };
    };
  };
}
