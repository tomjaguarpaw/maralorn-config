{
  services = {
    nginx = {
      enable = true;
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        root = "/var/cache/gc-links/blog";
        locations = {
          "/static" = {
            extraConfig = ''
              rewrite ^(.*)\.[0-9a-f]+\.(css)$ $1.$2 last;
              expires 100y;
              add_header Pragma public;
              add_header Cache-Control "public";
            '';
          };
          "/" = {
            tryFiles = "$uri $uri.html $uri/index.html =404";
          };
        };
        extraConfig = ''
          error_page 404 /not-found.html;
        '';
      };
    };
  };
}
