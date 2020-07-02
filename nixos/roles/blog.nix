{ config, pkgs, lib, ... }: {
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
