{ config, pkgs, lib, ... }: {
  services = {
    nginx = {
      enable = true;
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/cache/gc-links/blog";
            index = "index.html";
          };
        };
      };
    };
  };

}
