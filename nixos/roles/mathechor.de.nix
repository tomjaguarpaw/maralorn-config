{ pkgs, ... }: {
  services = {
    nginx = {
      enable = true;
      virtualHosts."mathechor.de" = {
        serverAliases = [ "www.mathechor.de" ];
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/cache/gc-links/mathechor.de/public";
            index = "index.html";
            extraConfig =
              "location ~* .(otf)$ {add_header Access-Control-Allow-Origin *;}";
          };
        };
      };
      virtualHosts."intern.mathechor.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "/var/cache/gc-links/mathechor.de/intern";
            index = "index.html";
            extraConfig = ''
              if ($query_string ~ "pw=([A-Za-z]*)") {
                 add_header Set-Cookie "password=$1; path=/; Max-Age=${
                   toString (365 * 24 * 60 * 60)
                 }; Secure";
                 return 303 /;
              }
              if ($http_cookie !~ "password=${
                pkgs.privateValue "" "mathechor.de-pw"
              }") {
                 return 303 /logout;
              }
            '';
          };
          "/login".extraConfig = ''
            more_set_headers 'Content-Type: text/html';
            return 200 '<!DOCTYPE html><html><head><meta charset="UTF-8"></head><body><form style="text-align: center; margin: 250px auto; width: 500px;" action="/" method="get"><label for="pw">Passwort: </label><input type="password" name="pw"><input type="submit" value="login"></form></html></body>';
          '';
          "/logout".extraConfig = ''
            add_header Set-Cookie 'password=""; Max-Age=0';
            return 303 /login;
          '';
          "/mathechor.ics" = {
            proxyPass = pkgs.privateValue "" "mathechor-ics";
            extraConfig = ''
              proxy_ssl_name cloud.mathechor.de;
              proxy_ssl_server_name on;
              set $args export;
            '';
          };
        };
      };
    };
  };
}
