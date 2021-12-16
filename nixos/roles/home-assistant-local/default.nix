{ pkgs, ... }:
let
  inherit (import ../../../nix/sources.nix) nixos-unstable;
  homeAssistantDir = "/disk/persist/home-assistant";
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
    "d ${homeAssistantDir} - - - - -"
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
      configDir = homeAssistantDir;
      config = {
        group = {
          wohnzimmer_lights = {
            name = "Lichter Wohnzimmer";
            entities = [ "switch.blaue_lichterkette" "switch.lichterkette_schrank" "switch.lichterkette_fernseher" ];
          };
          schlafzimmer_lights = {
            name = "Lichter Schlafzimmer";
            entities = [ "switch.lichterkette_schlafzimmer" "switch.weihnachtsstern_schlafzimmer" ];
          };
        };
        homeassistant = pkgs.privateValue { } "homeassistant-home";
        logger = {
          default = "info";
          logs = {
            "homeassistant.core" = "debug";
            "homeassistant.components.zha" = "debug";
            zigpy = "debug";
            zigpy_znp = "debug";
            zhaquirks = "debug";
          };
        };
        automation = [
          {
            alias = "Entfeuchtersteuerung Schlafzimmer";
            trigger = [
              { platform = "state"; entity_id = "sensor.schlafzimmer_humidity"; }
              { platform = "state"; entity_id = "binary_sensor.schlafzimmerfenster"; }
            ];
            action = [{
              choose = [
                {
                  conditions = [{
                    condition = "or";
                    conditions = [
                      { condition = "numeric_state"; entity_id = "sensor.schlafzimmer_humidity"; below = 64; }
                      { condition = "state"; entity_id = "binary_sensor.schlafzimmerfenster"; state = "on"; }
                    ];
                  }];
                  sequence = { service = "switch.turn_off"; target.entity_id = "switch.luftentfeuchter"; };
                }
                {
                  conditions = [{ condition = "numeric_state"; entity_id = "sensor.schlafzimmer_humidity"; above = 66; }];
                  sequence = { service = "switch.turn_on"; target.entity_id = "switch.luftentfeuchter"; };
                }
              ];
            }];
          }
          {
            alias = "Lüftungssteuerung Bad";
            trigger = [{ platform = "state"; entity_id = "sensor.bad_humidity"; }];
            action = [{
              choose = [{
                conditions = [{ condition = "numeric_state"; entity_id = "sensor.bad_humidity"; above = 66; }];
                sequence = { service = "switch.turn_on"; target.entity_id = "switch.lueftung_bad"; };
              }
                {
                  conditions = [{ condition = "numeric_state"; entity_id = "sensor.bad_humidity"; below = 64; }];
                  sequence = { service = "switch.turn_off"; target.entity_id = "switch.lueftung_bad"; };
                }];
            }];
          }
          {
            alias = "Thermostatsteuerung Schlafzimmer";
            trigger = [
              { platform = "state"; entity_id = "input_number.target_temperature_schlafzimmer"; }
              { platform = "state"; entity_id = "sensor.schlafzimmer_temperature"; }
              { platform = "state"; entity_id = "binary_sensor.schlafzimmerfenster"; }
              { platform = "state"; entity_id = "climate.schlafzimmer"; }
            ];
            action = [{
              choose = [{
                conditions = [
                  { condition = "numeric_state"; entity_id = "sensor.schlafzimmer_temperature"; below = "input_number.target_temperature_schlafzimmer"; }
                  { condition = "state"; entity_id = "binary_sensor.schlafzimmerfenster"; state = "off"; }
                ];
                sequence = {
                  service = "climate.set_temperature";
                  target.area_id = "schlafzimmer";
                  data = { temperature = 30; hvac_mode = "heat"; };
                };
              }];
              default = {
                service = "climate.turn_off";
                target.area_id = "schlafzimmer";
              };
            }];
          }
          {
            alias = "Thermostatsteuerung Küche";
            trigger = [
              { platform = "state"; entity_id = "input_number.target_temperature_kueche"; }
              { platform = "state"; entity_id = "sensor.kueche_temperature"; }
              { platform = "state"; entity_id = "binary_sensor.kuechenfenster"; }
              { platform = "state"; entity_id = "climate.kueche"; }
            ];
            action = [{
              choose = [{
                conditions = [
                  { condition = "numeric_state"; entity_id = "sensor.kueche_temperature"; below = "input_number.target_temperature_kueche"; }
                  { condition = "state"; entity_id = "binary_sensor.kuechenfenster"; state = "off"; }
                ];
                sequence = {
                  service = "climate.set_temperature";
                  target.area_id = "kuche";
                  data = { temperature = 30; hvac_mode = "heat"; };
                };
              }];
              default = {
                service = "climate.turn_off";
                target.area_id = "kuche";
              };
            }];
          }
          {
            alias = "Thermostatsteuerung Wohnzimmer";
            trigger = [
              { platform = "state"; entity_id = "input_number.target_temperature_wohnzimmer"; }
              { platform = "state"; entity_id = "binary_sensor.wohnzimmerfenster"; }
              { platform = "state"; entity_id = "climate.wohnzimmer"; }
            ];
            action = [{
              choose = [{
                conditions = [{ condition = "state"; entity_id = "binary_sensor.wohnzimmerfenster"; state = "off"; }];
                sequence = {
                  service = "climate.set_temperature";
                  target.area_id = "wohnzimmer";
                  data = { temperature = "{{ states('input_number.target_temperature_wohnzimmer') | int }}"; hvac_mode = "heat"; };
                };
              }];
              default = {
                service = "climate.turn_off";
                target.area_id = "wohnzimmer";
              };
            }];
          }
          {
            alias = "Küchentemperatur";
            trigger = [{ platform = "state"; entity_id = "input_select.scene_kueche"; }];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_kueche";
              data.value = ''
                {% if is_state('input_select.scene_kueche', 'empty') %}
                  18
                {% else %}
                  21
                {% endif %}
              '';
            }];
          }
          {
            alias = "Wohnzimmertemperatur";
            trigger = [{ platform = "state"; entity_id = "input_select.scene_wohnzimmer"; }];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_wohnzimmer";
              data.value = ''
                {% if is_state('input_select.scene_wohnzimmer', 'empty') %}
                  18
                {% else %}
                  24
                {% endif %}
              '';
            }];
          }
          {
            alias = "Schlafzimmertemperatur";
            trigger = [{ platform = "state"; entity_id = "input_select.scene_schlafzimmer"; }];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_schlafzimmer";
              data.value = ''
                {% if is_state('input_select.scene_schlafzimmer', 'empty') %}
                  18
                {% else %}
                  21
                {% endif %}
              '';
            }];
          }
          {
            alias = "Wohnzimmerlichter";
            trigger = [{ platform = "state"; entity_id = "input_select.scene_wohnzimmer"; } { platform = "state"; entity_id = "sun.sun"; }];
            action = [{
              service = ''
                {% if is_state('input_select.scene_wohnzimmer', 'force-active') or (is_state('input_select.scene_wohnzimmer', 'active') and state_attr('sun.sun', 'elevation') < 6) %}
                homeassistant.turn_on
                {% else %}
                homeassistant.turn_off
                {% endif %}'';
              target.entity_id = "group.wohnzimmer_lights";
            }];
          }
          {
            alias = "Schlafzimmerlichter";
            trigger = [{ platform = "state"; entity_id = "input_select.scene_schlafzimmer"; } { platform = "state"; entity_id = "sun.sun"; }];
            action = [{
              service = ''
                {% if is_state('input_select.scene_schlafzimmer', 'force-active') or (is_state('input_select.scene_schlafzimmer', 'active') and state_attr('sun.sun', 'elevation') < 6) %}
                homeassistant.turn_on
                {% else %}
                homeassistant.turn_off
                {% endif %}'';
              target.entity_id = "group.schlafzimmer_lights";
            }];
          }
          {
            alias = "Schlafzimmer vorheizen";
            trigger = [{ platform = "time"; at = "19:00:00"; } { platform = "time"; at = "04:00:00"; }];
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "empty"; }];
            action = [{ service = "input_select.select_option"; data.option = "heat"; entity_id = "input_select.scene_schlafzimmer"; }];
          }
          {
            alias = "Schlafzimmer nachts kühl";
            trigger = [{ platform = "time"; at = "01:00:00"; }];
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "heat"; }];
            action = [{ service = "input_select.select_option"; data.option = "empty"; entity_id = "input_select.scene_schlafzimmer"; }];
          }
          {
            alias = "Morgens Licht an";
            trigger = [{ platform = "time"; at = "08:00:00"; }];
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "heat"; }];
            action = [{ service = "input_select.select_option"; data.option = "active"; entity_id = "input_select.scene_schlafzimmer"; }];
          }
          # Warnung für offene Fenster oder Türen
          # Warnungen für niedrige Akkustände
          # Warnungen für hohe Luftfeuchtigkeit
        ];
        history = { };
        image = { };
        sun = { };
        logbook = { };
        config = { };
        mobile_app = { };
        recorder = { };
        ssdp = { };
        template = [
          { sensor = [{ state = "{% if is_state('switch.luftentfeuchter', 'on') %}1{% else %}0{% endif %}"; name = "Luftentfeuchter"; }]; }
          { sensor = [{ state = "{% if is_state('binary_sensor.schlafzimmerfenster', 'on') %}1{% else %}0{% endif %}"; name = "Schlafzimmerfenster"; }]; }
          { sensor = [{ state = "{% if is_state('climate.schlafzimmer', 'heat') %}1{% else %}0{% endif %}"; name = "Schlafzimmerheizung"; }]; }
        ];
        input_number = {
          target_temperature_schlafzimmer = {
            name = "Zieltemperatur Schlafzimmer";
            unit_of_measurement = "°C";
            min = "17";
            max = "25";
            step = "0.25";
          };
          target_temperature_wohnzimmer = {
            name = "Zieltemperatur Wohnzimmer";
            unit_of_measurement = "°C";
            min = "17";
            max = "25";
            step = "0.25";
          };
          target_temperature_kueche = {
            name = "Zieltemperatur Küche";
            unit_of_measurement = "°C";
            min = "17";
            max = "25";
            step = "0.25";
          };
        };
        input_select = {
          scene_schlafzimmer = {
            name = "Szene Schlafzimmer";
            options = [ "empty" "heat" "active" "force-active" ];
          };
          scene_wohnzimmer = {
            name = "Szene Wohnzimmer";
            options = [ "empty" "heat" "active" "force-active" ];
          };
          scene_kueche = {
            name = "Szene Kueche";
            options = [ "empty" "heat" "active" "force-active" ];
          };
        };
        system_health = { };
        zha = {
          device_config = {
            "00:12:4b:00:24:c0:ff:52-1".type = "switch"; # Lüftung Bad
            "00:12:4b:00:24:c1:00:45-1".type = "switch"; # Blaue Lichterkette
            "00:12:4b:00:24:c1:00:1b-1".type = "switch"; # Luftentfeuchter Schlafzimmer
            "00:12:4b:00:24:c1:00:05-1".type = "switch"; # Lichterkette Fernseher
            "00:12:4b:00:24:c0:ff:16-1".type = "switch"; # Lichterkette Schrank
            "00:12:4b:00:24:c0:ff:a8-1".type = "switch"; # Lichterkette Schlafzimmer
            "00:12:4b:00:24:c0:ff:ad-1".type = "switch"; # Weihnachtsstern Schlafzimmer
          };
        };
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
      lovelaceConfig =
        let
          badges = [
            { type = "state-label"; entity = "input_select.scene_wohnzimmer"; name = "Wohnzimmer"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_wohnzimmer"; }; }
            { type = "state-label"; entity = "input_select.scene_kueche"; name = "Küche"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_kueche"; }; }
            { type = "state-label"; entity = "input_select.scene_schlafzimmer"; name = "Schlafzimmer"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_schlafzimmer"; }; }
            {
              type = "entity-filter";
              entities = [ "binary_sensor.wohnzimmerfenster" "binary_sensor.schlafzimmerfenster" "binary_sensor.kuechenfenster" "binary_sensor.wohnungstuer" ];
              state_filter = [ "on" ];
            }
          ];
        in
        {
          views = [
            {
              icon = "mdi:city";
              inherit badges;
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
                  type = "custom:sun-card";
                }
                {
                  type = "custom:rmv-card";
                  entity = "sensor.darmstadt_schulstrasse";
                }
              ];
            }
            {
              icon = "mdi:sofa";
              inherit badges;
              cards = [
                {
                  type = "entities";
                  icon = "mdi:sofa";
                  title = "Wohnzimmer";
                  show_header_toggle = false;
                  entities = [
                    {
                      type = "custom:multiple-entity-row";
                      entity = "climate.wohnzimmer";
                      name = "Temperatur";
                      attribute = "current_temperature";
                      unit = false;
                      entities = [
                        { entity = "climate.wohnzimmer"; name = "Heizung"; }
                        { entity = "input_number.target_temperature_wohnzimmer"; name = "Zieltemp."; unit = false; }
                      ];
                    }
                    {
                      type = "custom:multiple-entity-row";
                      entity = "group.wohnzimmer_lights";
                      toggle = true;
                      state_color = true;
                      name = "Lichter";
                      entities = [
                        { entity = "switch.lichterkette_schrank"; state_color = true; toggle = true; name = "Ballons"; }
                        { entity = "switch.lichterkette_fernseher"; state_color = true; toggle = true; name = "Blumen"; }
                        { entity = "switch.blaue_lichterkette"; state_color = true; toggle = true; name = "Blaue Kette"; }
                      ];
                    }
                    { entity = "binary_sensor.wohnzimmerfenster"; secondary_info = "last-changed"; name = "Balkontür"; }
                  ];
                }
                {
                  type = "history-graph";
                  entities = [
                    "input_number.target_temperature_wohnzimmer"
                    "climate.wohnzimmer"
                  ];
                }
              ];
            }
            {
              icon = "mdi:countertop";
              inherit badges;
              cards = [
                {
                  type = "entities";
                  title = "Küche";
                  icon = "mdi:countertop";
                  entities = [
                    {
                      type = "custom:multiple-entity-row";
                      entity = "sensor.kueche_temperature";
                      name = "Temperatur";
                      unit = false;
                      entities = [{ entity = "climate.kueche"; name = "Heizung"; }
                        { entity = "climate.kueche"; attribute = "temperature"; name = "Zieltemp. an Heizung"; }
                        { entity = "climate.kueche"; attribute = "current_temperature"; name = "Temp. an Heizung"; unit = false; }
                        { entity = "input_number.target_temperature_kueche"; name = "Zieltemp."; unit = false; }];
                    }
                    {
                      type = "custom:multiple-entity-row";
                      entity = "binary_sensor.kuechenfenster";
                      secondary_info = "last-changed";
                      name = "Fenster";
                      entities = [{ entity = "sensor.bad_humidity"; name = "Luftfeuchtigkeit"; }];
                    }
                  ];
                }
                {
                  type = "history-graph";
                  entities = [
                    "sensor.kueche_temperature"
                    "input_number.target_temperature_kueche"
                    "sensor.kueche_humidity"
                    "climate.kueche"
                  ];
                }
              ];
            }
            {
              icon = "mdi:bed-king";
              inherit badges;
              cards = [
                {
                  type = "vertical-stack";
                  cards = [
                    {
                      type = "custom:mini-graph-card";
                      entities = [
                        { entity = "sensor.schlafzimmer_humidity"; name = "Luftfeuchtigkeit"; show_fill = false; state_adaptive_color = true; }
                        { entity = "sensor.luftentfeuchter"; name = "Entfeuchter"; color = "#0000ff"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
                        { entity = "sensor.schlafzimmerfenster"; name = "Fenster"; color = "#ff0000"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
                      ];
                      color_thresholds = [{ value = 0; color = "#009933"; } { value = 64; color = "#ffbf00"; } { value = 66; color = "#ff0000"; }];
                      color_thresholds_transition = "hard";
                      show = {
                        labels = true;
                        labels_secondary = "hover";
                      };
                      lower_bound_secondary = 0;
                      upper_bound_secondary = 1;
                      hour24 = true;
                      decimals = 1;
                      points_per_hour = 6;
                      hours_to_show = 6;
                      update_interval = 30;
                      line_width = 3;
                      state_map = [
                        { value = 0; label = "Zu/Aus"; }
                        { value = 1; label = "An/Auf"; }
                      ];
                    }
                    {
                      type = "custom:mini-graph-card";
                      entities = [
                        { entity = "sensor.schlafzimmer_temperature"; name = "Temperatur"; show_fill = false; }
                        { entity = "input_number.target_temperature_schlafzimmer"; name = "Zieltemperatur"; show_fill = false; }
                        { entity = "sensor.schlafzimmerheizung"; name = "Heizung"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
                      ];
                      show = {
                        labels = true;
                        labels_secondary = "hover";
                      };
                      lower_bound_secondary = 0;
                      upper_bound_secondary = 1;
                      hours_to_show = 6;
                      update_interval = 30;
                      line_width = 3;
                      hour24 = true;
                      decimals = 1;
                      points_per_hour = 6;
                      state_map = [
                        { value = 0; label = "Aus"; }
                        { value = 1; label = "An"; }
                      ];
                    }
                  ];
                }
                {
                  type = "grid";
                  cards = [
                    { type = "custom:slider-button-card"; entity = "switch.weihnachtsstern_schlafzimmer"; }
                    { type = "custom:slider-button-card"; entity = "switch.luftentfeuchter"; }
                    { type = "custom:slider-button-card"; entity = "climate.schlafzimmer"; }
                  ];

                }
              ];
            }
            {
              icon = "mdi:shower-head";
              inherit badges;
              cards = [
                {
                  type = "entities";
                  entities = [{
                    type = "custom:multiple-entity-row";
                    entity = "switch.lueftung_bad";
                    toggle = true;
                    state_color = true;
                    name = "Lüftung";
                    entities = [
                      { entity = "sensor.bad_humidity"; name = "Luftfeuchtigkeit"; }
                      { entity = "sensor.bad_temperature"; name = "Temperatur"; }
                    ];
                  }];
                }
                {
                  type = "history-graph";
                  entities = [
                    "sensor.bad_temperature"
                    "sensor.bad_humidity"
                    "switch.lueftung_bad"
                  ];
                }
              ];
            }
            {
              icon = "mdi:door-closed";
              inherit badges;
              cards = [
                {
                  type = "entities";
                  title = "Flur";
                  entities = [{ entity = "binary_sensor.wohnungstuer"; secondary_info = "last-changed"; }];
                }
                { type = "history-graph"; entities = [ "binary_sensor.wohnungstuer" ]; }
              ];
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
