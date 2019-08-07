{ config, ... }:
let
  locations = {
    "/" = {
      extraConfig = ''
        return 200 "Hello there. I hope you are having a very nice day! If you don't know what to find here, you probably don't care about this domain.";
      '';
    };
  };
in {
  m-0.monitoring = [{
    name = "hera-nginx";
    host = "hera-intern:9113";
  }];
  services = {
    nginx = {
      enable = true;
      virtualHosts."hera.m-0.eu" = {
        enableACME = true;
        forceSSL = true;
        inherit locations;
      };
      virtualHosts."maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations = locations // {
          "/.well-known/matrix/server" = {
            extraConfig = ''
              default_type application/json;
              return 200 "{\"m.server\": \"matrix.maralorn.de:443\"}";
            '';
          };
        };
      };
    };
  };

}
