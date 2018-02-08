{
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."mathechor.de" = {
        serverAliases = ["www.mathechor.de"];
        forceSSL = true;
        enableACME = true;
        locations = {
          "~* Makefile".extraConfig = "deny all;";
          "/" = {
            root = "/var/www/mathechor/public";
            index = "index.html";
            extraConfig = "location ~* \.(otf)$ {add_header Access-Control-Allow-Origin *;}";
          };
        };
      };
      virtualHosts."intern.mathechor.de" = {
        forceSSL = true;
        enableACME = true;
        # See /etc/nixos/local/ für basic_auth pw.
        locations = {
          "~* Makefile".extraConfig = "deny all;";
          "/" = {
            root = "/var/www/mathechor/intern";
            index = "index.html";
          };
          "/mathechor.ics" = {
            proxyPass ="http://127.0.0.1:5232/maralorn/23e21619-29c6-17eb-043f-8ab5af00b46b/";
            extraConfig = ''
              proxy_set_header     X-Remote-User maralorn;
            '';
          };
        };
      };
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/www/blog/output";
            index = "index.html";
          };
        };
      };
      virtualHosts."charon.olymp.space" = {
        forceSSL = true;
        enableACME = true;
        default = true;
        locations = {
          "/ved.ics" = {
            proxyPass ="http://127.0.0.1:5232/maralorn/5a155c2c-1d87-e50d-874c-63f8858d1302/";
            extraConfig = ''
              proxy_set_header     X-Remote-User maralorn;
            '';
          };
        };
      };
    };
  };
}
