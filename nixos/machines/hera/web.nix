{ config, pkgs, ... }:
let
  locations = {
    "/" = {
      extraConfig = ''
        return 200 "Hello there. I hope you are having a very nice day! If you don't know what to find here, you probably don't care about this domain.";
      '';
    };
  };
in {
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  m-0.monitoring = [{
    name = "hera-nginx";
    host = "hera-intern:9113";
  }];
  security.acme.certs."hera.m-0.eu".keyType = "rsa4096";
  services = {
    nginx = {
      enable = true;
      virtualHosts."tasks.maralorn.de" = {
        basicAuthFile = pkgs.privatePath "basic-auth/kassandra";
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            proxyPass = "http://[::1]:8000";
            proxyWebsockets = true;
          };
        };
      };
      virtualHosts."hera.m-0.eu" = {
        enableACME = true;
        forceSSL = true;
        inherit locations;
      };
      virtualHosts."maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        inherit locations;
      };
    };
  };

}
