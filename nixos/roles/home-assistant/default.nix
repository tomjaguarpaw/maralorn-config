{ pkgs, ... }:
{
  services.home-assistant = {
    enable = true;
    package = pkgs.home-assistant.overrideAttrs (
      oldAttrs: {
        doInstallCheck = false;
        patches = (oldAttrs.patches or [ ]) ++ [ ./warnwetter.patch ];
      }
    );
    config = {
      default_config = { };
      sensor = [
        {
          platform = "rmvtransport";
          next_departure = [
            {
              station = "3024634";
            }
          ];
        }
      ];
      weather = [
        {
          platform = "warnwetter";
          name = "DWD Darmstadt";
          station_id = "L886";
        }
      ];
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "::1" ];
      };
      prometheus = {
        namespace = "hass";
      };
    };
    lovelaceConfig = {
      views = [
        {
          cards = [
            {
              type = "weather-forecast";
              entity = "weather.dwd_darmstadt";
            }
            {
              type = "picture";
              image = "https://www.dwd.de/DWD/wetter/radar/radfilm_hes_akt.gif";
            }
            {
              type = "history-graph";
              entities = [
                "sensor.pegasus_battery_level"
                "sensor.pegasus_battery_state"
                "sensor.kalliope_battery_level"
                "sensor.kalliope_battery_state"
              ];
            }
          ];
        }
      ];
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
