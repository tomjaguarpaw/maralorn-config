{ config, pkgs, lib, ... }:
let
  locations."/".extraConfig = "return 301 https://blog.maralorn.de$request_uri;";
in
{
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  m-0.monitoring = [{
    name = "hera-nginx";
    host = "hera-intern:9113";
  }];
  security.acme.certs."hera.m-0.eu".keyType = "rsa4096";
  services = {
    nginx = {
      enable = lib.mkForce pkgs.withSecrets;
      virtualHosts = {
        "stream.maralorn.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/".proxyPass =
            "http://[${config.m-0.hosts.apollo-wg}]:8123";
        };
        "tasks.maralorn.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://[::1]:8000";
            proxyWebsockets = true;
          };
        };
        "hera.m-0.eu" = {
          enableACME = true;
          forceSSL = true;
          inherit locations;
        };
        "maralorn.de" = {
          enableACME = true;
          forceSSL = true;
          inherit locations;
        };
        "fdroid.maralorn.de" = {
          forceSSL = true;
          enableACME = true;
          locations = {
            "/" = {
              root = "/var/www/fdroid/repo";
            };
          };
        };
      };
    };
  };

}
