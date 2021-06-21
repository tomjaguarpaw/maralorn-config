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
            tryFiles = "$uri $uri/index.html $uri.html";
            extraConfig = ''
              error_page   404  =  /not-found.html;
            '';
          };
        };
      };
    };
  };

}
