{ pkgs, ... }:
{
  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.overrideAttrs (oldAttrs: {
      doInstallCheck = false;
      patches = (oldAttrs.patches or [ ]) ++ [ ./warnwetter.patch ];
    });
    config = {
      default_config = { };
      weather = [{
        platform = "warnwetter";
        name = "DWD Darmstadt";
        station_id = "L886";
      }];
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "::1" ];
      };
    };
    lovelaceConfig = {
      views = [{
        cards = [
          {
            type = "weather-forecast";
            entity = "weather.dwd_darmstadt";
          }
        ];
      }];
    };
  };

  services.nginx = {
    virtualHosts."home.maralorn.de" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://[::1]:8123";
        proxyWebsockets = true;
      };
    };
  };

}
