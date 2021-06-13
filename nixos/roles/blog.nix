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
            tryFiles = "$uri $uri.html =404";
          };
        };
        extraConfig = ''
          satisfy any;
          allow ${config.m-0.prefix}::/64;
          deny all;
          error_page   404  =  /not-found.html;
        '';
      };
    };
  };

}
