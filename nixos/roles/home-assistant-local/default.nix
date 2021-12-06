{ pkgs, ... }:
let
  inherit (import ../../../nix/sources.nix) nixos-unstable;
in
{

  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    "${nixos-unstable}/nixos/modules/services/misc/home-assistant.nix"
    ./hexa-cards.nix
  ];
  systemd.tmpfiles.rules = [
    "d /disk/persist/var/lib/hass - - - - -"
  ];

  services = {
    home-assistant = {
      enable = true;
      package = pkgs.home-assistant.overrideAttrs (
        oldAttrs: {
          doInstallCheck = false;
          patches = (oldAttrs.patches or [ ]) ++ [ ./warnwetter.patch ];
        }
      );
      configDir = "/disk/persist/var/lib/hass";
      config = {
        homeassistant = {
          name = "Kiesstraße 10, 1. OG, links";
          latitude = "49.8766";
          longitude = "8.6524";
          elevation = "150";
          unit_system = "metric";
          currency = "EUR";
          time_zone = "Europe/Berlin";
          external_url = "http://home.wg.m-0.eu";
          internal_url = "http://home.lo.m-0.eu";
        };
        automation = { };
        history = { };
        image = { };
        sun = { };
        logbook = { };
        config = { };
        mobile_app = { };
        recorder = { };
        ssdp = { };
        system_health = { };
        zha = { };
        ipp = { };
        brother = { };
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
        title = "Kiesstraße 10, 1. OG, links";
        views = [
          {
            title = "Übersicht";
            cards = [
              {
                type = "weather-forecast";
                entity = "weather.dwd_darmstadt";
              }
              {
                type = "custom:sun-card";
              }
              {
                type = "picture";
                image = "https://www.dwd.de/DWD/wetter/radar/radfilm_hes_akt.gif";
              }
              #{
              #  type = "history-graph";
              #  entities = [
              #    "sensor.pegasus_battery_level"
              #    "sensor.pegasus_battery_state"
              #    "sensor.kalliope_battery_level"
              #    "sensor.kalliope_battery_state"
              #  ];
              #}
              {
                type = "custom:rmv-card";
                entity = "sensor.darmstadt_schulstrasse";
              }
            ];
          }
        ];
      };
    };
    nginx = {
      enable = true;
      virtualHosts = {
        "home.lo.m-0.eu" = {
          extraConfig = "proxy_buffering off;";
          locations."/" = {
            proxyPass = "http://[::1]:8123";
            proxyWebsockets = true;
          };
          locations."/custom/" = {
            alias = "/run/hass/";
          };
        };
        "fluffy.lo.m-0.eu" = {
          default = true;
          locations."/".extraConfig = "return 301 http://home.lo.m-0.eu$request_uri;";
        };
      };
    };
  };
}
