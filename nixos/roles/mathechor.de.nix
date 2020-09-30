{ config, pkgs, lib, ... }:
{
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
        basicAuthFile = pkgs.privatePath "basic-auth/mathechor.de";
        locations = {
          "/" = {
            root = "/var/www/mathechor/intern";
            index = "index.html";
          };
          "/mathechor.ics" = {
            proxyPass = pkgs.privateValue "" "mathechor-ics";
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
