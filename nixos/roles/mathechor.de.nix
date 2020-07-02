{ config, pkgs, lib, ... }:
let inherit (config.m-0.private) mathechor-pw me;
in {
  services = {
    nginx = {
      enable = true;
      virtualHosts."mathechor.de" = {
        serverAliases = [ "www.mathechor.de" ];
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/www/mathechor/public";
            index = "index.html";
            extraConfig =
              "location ~* .(otf)$ {add_header Access-Control-Allow-Origin *;}";
          };
        };
      };
      virtualHosts."intern.mathechor.de" = {
        forceSSL = true;
        enableACME = true;
        basicAuth.mathechor = mathechor-pw;
        locations = {
          "/" = {
            root = "/var/www/mathechor/intern";
            index = "index.html";
          };
          "/mathechor.ics" = {
            proxyPass =
              "https://cloud.mathechor.de/remote.php/dav/public-calendars/nebsfFTzQKGSSsDc?export";
            extraConfig = ''
              proxy_ssl_name cloud.mathechor.de;
              proxy_ssl_server_name on;
            '';
          };
        };
      };
    };
  };

}
