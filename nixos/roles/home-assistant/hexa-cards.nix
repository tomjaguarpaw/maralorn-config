{ pkgs, ... }:
let
  nur = pkgs.flake-inputs'.hexa-nur-packages.packages;

  mkLovelaceModule = name: {
    url = "custom/${name}.js?${nur.hassLovelaceModules."${name}".version}";
    type = "module";
  };
in
{
  systemd.tmpfiles.rules = [
    # Lovelace Cards
    "d /run/hass 0700 nginx nginx"
    "L+ /run/hass/mini-graph-card.js - - - - ${nur.hassLovelaceModules.mini-graph-card}/mini-graph-card-bundle.js"
    #"L+ /run/hass/mini-media-player.js - - - - ${nur.hassLovelaceModules.mini-media-player}/mini-media-player-bundle.js"
    #"L+ /run/hass/multiple-entity-row.js - - - - ${nur.hassLovelaceModules.multiple-entity-row}/multiple-entity-row.js"
    "L+ /run/hass/sun-card.js - - - - ${nur.hassLovelaceModules.sun-card}/sun-card.js"
    #"L+ /run/hass/slider-button-card.js - - - - ${nur.hassLovelaceModules.slider-button-card}/slider-button-card.js"
    "L+ /run/hass/rmv-card.js - - - - ${nur.hassLovelaceModules.rmv-card}/rmv-card.js"
    #"L+ /run/hass/weather-card-chart.js - - - - ${nur.hassLovelaceModules.weather-card-chart}/weather-card-chart.js"
  ];

  services.home-assistant.config.lovelace = {
    resources = [
      (mkLovelaceModule "mini-graph-card")
      #(mkLovelaceModule "mini-media-player")
      #(mkLovelaceModule "multiple-entity-row")
      (mkLovelaceModule "rmv-card")
      #(mkLovelaceModule "weather-card-chart")
      (mkLovelaceModule "sun-card")
      #(mkLovelaceModule "slider-button-card")
    ];
  };
}
