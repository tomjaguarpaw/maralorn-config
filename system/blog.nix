{ config, pkgs, lib, ... }: {
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/www/blog";
            index = "index.html";
          };
        };
      };
    };
  };

}
