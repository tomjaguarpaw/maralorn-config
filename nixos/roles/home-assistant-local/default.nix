{ pkgs, lib, ... }:
let
  haLib = import ./lib.nix lib;
  inherit (haLib) modules util cards conditions triggers;
  modes =
    let
      empty = {
        icon = util.mkIcon "account-off";
        title = "Leer";
      };
      heat = {
        icon = util.mkIcon "radiator";
        title = "Heizen";
      };
      active = {
        icon = util.mkIcon "account";
        title = "Aktiv";
      };
      force-active = {
        icon = util.mkIcon "lightbulb-on";
        title = "Alles An";
      };
      vacation = {
        icon = util.mkIcon "airplane";
        title = "Urlaub";
      };
    in
    {
      flat = {
        title = "Wohnung";
        name = "flat";
        options = { inherit active vacation; };
      };
    };
  inherit (import ../../../nix/sources.nix) nixos-unstable;
  homeAssistantDir = "/disk/persist/home-assistant";
in
{

  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    (modules.mkModeSwitcher modes.flat { })
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
        frontend.themes.ourdefault = {
          primary-color = "#858EFF";
        };
        automation = [
          {
            alias = "Set theme at startup'";
            trigger = { platform = "homeassistant"; event = "start"; };
            action = { service = "frontend.set_theme"; data.name = "ourdefault"; };
          }
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
            trigger = with triggers; [
              (stateTrigger "input_number.target_temperature_schlafzimmer")
              (stateTrigger "sensor.schlafzimmer_temperature")
              (stateTrigger "binary_sensor.schlafzimmerfenster")
              (stateTrigger "climate.schlafzimmer")
              (modeSwitchTrigger modes.flat)
            ];
            action = [{
              choose = [{
                conditions = [
                  { condition = "numeric_state"; entity_id = "sensor.schlafzimmer_temperature"; below = "input_number.target_temperature_schlafzimmer"; }
                  { condition = "state"; entity_id = "binary_sensor.schlafzimmerfenster"; state = "off"; }
                  (conditions.modeIs modes.flat "active")
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
            trigger = with triggers; [
              { platform = "state"; entity_id = "input_number.target_temperature_kueche"; }
              { platform = "state"; entity_id = "sensor.kueche_temperature"; }
              { platform = "state"; entity_id = "binary_sensor.kuechenfenster"; }
              { platform = "state"; entity_id = "climate.kueche"; }
              (modeSwitchTrigger modes.flat)
            ];
            action = [{
              choose = [{
                conditions = [
                  { condition = "numeric_state"; entity_id = "sensor.kueche_temperature"; below = "input_number.target_temperature_kueche"; }
                  { condition = "state"; entity_id = "binary_sensor.kuechenfenster"; state = "off"; }
                  (conditions.modeIs modes.flat "active")
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
            trigger = with triggers; [
              { platform = "state"; entity_id = "input_number.target_temperature_wohnzimmer"; }
              { platform = "state"; entity_id = "binary_sensor.wohnzimmerfenster"; }
              { platform = "state"; entity_id = "climate.wohnzimmer"; }
              (modeSwitchTrigger modes.flat)
            ];
            action = [{
              choose = [{
                conditions = [{ condition = "state"; entity_id = "binary_sensor.wohnzimmerfenster"; state = "off"; }
                  (conditions.modeIs modes.flat "active")];
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
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "empty"; } (conditions.modeIs modes.flat "active")];
            action = [{ service = "input_select.select_option"; data.option = "heat"; entity_id = "input_select.scene_schlafzimmer"; }];
          }
          {
            alias = "Schlafzimmer nachts kühl";
            trigger = [{ platform = "time"; at = "01:00:00"; }];
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "heat"; } (conditions.modeIs modes.flat "active")];
            action = [{ service = "input_select.select_option"; data.option = "empty"; entity_id = "input_select.scene_schlafzimmer"; }];
          }
          {
            alias = "Morgens Licht an";
            trigger = [{ platform = "time"; at = "08:00:00"; }];
            condition = [{ condition = "state"; entity_id = "input_select.scene_schlafzimmer"; state = "heat"; } (conditions.modeIs modes.flat "active")];
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
          { sensor = [{ state = "{% if is_state('switch.lueftung_bad', 'on') %}1{% else %}0{% endif %}"; name = "Lüftung"; }]; }
          { sensor = [{ state = "{% if is_state('binary_sensor.schlafzimmerfenster', 'on') %}1{% else %}0{% endif %}"; name = "Schlafzimmerfenster"; }]; }
          { sensor = [{ state = "{% if is_state('binary_sensor.wohnzimmerfenster', 'on') %}1{% else %}0{% endif %}"; name = "Balkontür"; }]; }
          { sensor = [{ state = "{% if is_state('binary_sensor.kuechenfenster', 'on') %}1{% else %}0{% endif %}"; name = "Küchenfenster"; }]; }
          { sensor = [{ state = "{% if is_state('binary_sensor.wohnungstuer', 'on') %}1{% else %}0{% endif %}"; name = "Wohnungstür"; }]; }
          { sensor = [{ state = "{% if is_state('climate.schlafzimmer', 'heat') %}1{% else %}0{% endif %}"; name = "Schlafzimmerheizung"; }]; }
          { sensor = [{ state = "{% if is_state('climate.wohnzimmer', 'heat') %}1{% else %}0{% endif %}"; name = "Wohnzimmerheizung"; }]; }
          { sensor = [{ state = "{% if is_state('climate.kueche', 'heat') %}1{% else %}0{% endif %}"; name = "Küchenheizung"; }]; }
          { sensor = [{ state = "{{ state_attr('climate.wohnzimmer', 'current_temperature') }}"; name = "Temperatur Wohnzimmer"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_schlafzimmer', 'empty') %}1{% else %}0{% endif %}"; name = "schlafzimmer_empty"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_schlafzimmer', 'heat') %}1{% else %}0{% endif %}"; name = "schlafzimmer_heat"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_schlafzimmer', 'active') %}1{% else %}0{% endif %}"; name = "schlafzimmer_active"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_schlafzimmer', 'force-active') %}1{% else %}0{% endif %}"; name = "schlafzimmer_force_active"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_wohnzimmer', 'empty') %}1{% else %}0{% endif %}"; name = "wohnzimmer_empty"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_wohnzimmer', 'heat') %}1{% else %}0{% endif %}"; name = "wohnzimmer_heat"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_wohnzimmer', 'active') %}1{% else %}0{% endif %}"; name = "wohnzimmer_active"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_wohnzimmer', 'force-active') %}1{% else %}0{% endif %}"; name = "wohnzimmer_force_active"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_kueche', 'empty') %}1{% else %}0{% endif %}"; name = "kueche_empty"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_kueche', 'heat') %}1{% else %}0{% endif %}"; name = "kueche_heat"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_kueche', 'active') %}1{% else %}0{% endif %}"; name = "kueche_active"; }]; }
          { binary_sensor = [{ state = "{% if is_state('input_select.scene_kueche', 'force-active') %}1{% else %}0{% endif %}"; name = "kueche_force_active"; }]; }
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
          alertbadges = [
            {
              type = "entity-filter";
              entities = [ "binary_sensor.wohnzimmerfenster" "binary_sensor.schlafzimmerfenster" "binary_sensor.kuechenfenster" "binary_sensor.wohnungstuer" ];
              state_filter = [ "on" ];
            }
          ];
          badges = [
            { type = "state-label"; entity = "input_select.scene_wohnzimmer"; name = "Wohnzimmer"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_wohnzimmer"; }; }
            { type = "state-label"; entity = "input_select.scene_kueche"; name = "Küche"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_kueche"; }; }
            { type = "state-label"; entity = "input_select.scene_schlafzimmer"; name = "Schlafzimmer"; tap_action = { action = "call-service"; service = "input_select.select_next"; service_data.entity_id = "input_select.scene_schlafzimmer"; }; }
          ];
          envstack =
            {
              type = "vertical-stack";
              cards = [
                {
                  type = "custom:sun-card";
                }
                {
                  type = "weather-forecast";
                  entity = "weather.dwd_darmstadt";
                }
                {
                  type = "picture";
                  image = "https://www.dwd.de/DWD/wetter/radar/radfilm_hes_akt.gif";
                }
                {
                  type = "custom:rmv-card";
                  entity = "sensor.darmstadt_schulstrasse";
                }
              ];
            };
          wohnzimmerstack =
            {
              type = "vertical-stack";
              cards = [
                {
                  type = "glance";
                  title = "Wohnzimmer";
                  columns = 4;
                  show_state = false;
                  entities = [
                    { entity = "binary_sensor.wohnzimmer_empty"; name = "Leer"; icon = "mdi:account-off"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_wohnzimmer"; option = "empty"; }; }; }
                    { entity = "binary_sensor.wohnzimmer_heat"; name = "Heizen"; icon = "mdi:radiator"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_wohnzimmer"; option = "heat"; }; }; }
                    { entity = "binary_sensor.wohnzimmer_active"; name = "Aktiv"; icon = "mdi:account"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_wohnzimmer"; option = "active"; }; }; }
                    { entity = "binary_sensor.wohnzimmer_force_active"; name = "Alles An"; icon = "mdi:lightbulb-on"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_wohnzimmer"; option = "force-active"; }; }; }
                  ];
                }
                {
                  type = "custom:mini-graph-card";
                  entities = [
                    { entity = "sensor.temperatur_wohnzimmer"; name = "Temperatur"; show_fill = false; }
                    { entity = "input_number.target_temperature_wohnzimmer"; name = "Zieltemperatur"; show_fill = false; }
                    { entity = "sensor.wohnzimmerheizung"; name = "Heizung"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
                  ];
                  show = {
                    labels = true;
                    labels_secondary = "hover";
                  };
                  lower_bound_secondary = 0;
                  upper_bound_secondary = 1;
                  hours_to_show = 24;
                  update_interval = 30;
                  line_width = 2;
                  hour24 = true;
                  decimals = 1;
                  points_per_hour = 3;
                  state_map = [
                    { value = 0; label = "Aus"; }
                    { value = 1; label = "An"; }
                  ];
                }
                {
                  type = "logbook";
                  entities = [ "input_select.scene_wohnzimmer" "binary_sensor.wohnzimmerfenster" "switch.lichterkette_fernseher" "switch.lichterkette_schrank" "switch.blaue_lichterkette" ];
                }
              ];
            };
          kuechenstack = {
            type = "vertical-stack";
            cards = [
              {
                type = "glance";
                title = "Küche";
                columns = 4;
                show_state = false;
                entities = [
                  { entity = "binary_sensor.kueche_empty"; name = "Leer"; icon = "mdi:account-off"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_kueche"; option = "empty"; }; }; }
                  { entity = "binary_sensor.kueche_heat"; name = "Heizen"; icon = "mdi:radiator"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_kueche"; option = "heat"; }; }; }
                  { entity = "binary_sensor.kueche_active"; name = "Aktiv"; icon = "mdi:account"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_kueche"; option = "active"; }; }; }
                  { entity = "binary_sensor.kueche_force_active"; name = "Alles An"; icon = "mdi:lightbulb-on"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_kueche"; option = "force-active"; }; }; }
                ];
              }
              {
                type = "custom:mini-graph-card";
                entities = [
                  { entity = "sensor.kueche_humidity"; name = "Luftfeuchtigkeit"; show_fill = false; state_adaptive_color = true; }
                  { entity = "sensor.kuchenfenster"; name = "Fenster"; color = "#ff0000"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
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
                points_per_hour = 3;
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                state_map = [
                  { value = 0; label = "Zu"; }
                  { value = 1; label = "Auf"; }
                ];
              }
              {
                type = "custom:mini-graph-card";
                entities = [
                  { entity = "sensor.kueche_temperature"; name = "Temperatur"; show_fill = false; }
                  { entity = "input_number.target_temperature_kueche"; name = "Zieltemperatur"; show_fill = false; }
                  { entity = "sensor.kuchenheizung"; name = "Heizung"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
                ];
                show = {
                  labels = true;
                  labels_secondary = "hover";
                };
                lower_bound_secondary = 0;
                upper_bound_secondary = 1;
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                hour24 = true;
                decimals = 1;
                points_per_hour = 3;
                state_map = [
                  { value = 0; label = "Aus"; }
                  { value = 1; label = "An"; }
                ];
              }
              {
                type = "logbook";
                entities = [ "input_select.scene_kueche" "climate.kueche" "binary_sensor.kuechenfenster" ];
              }
            ];
          };
          schlafzimmerstack =
            {
              type = "vertical-stack";
              cards = [
                {
                  type = "glance";
                  title = "Schlafzimmer";
                  columns = 4;
                  show_state = false;
                  entities = [
                    { entity = "binary_sensor.schlafzimmer_empty"; name = "Leer"; icon = "mdi:account-off"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_schlafzimmer"; option = "empty"; }; }; }
                    { entity = "binary_sensor.schlafzimmer_heat"; name = "Heizen"; icon = "mdi:radiator"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_schlafzimmer"; option = "heat"; }; }; }
                    { entity = "binary_sensor.schlafzimmer_active"; name = "Aktiv"; icon = "mdi:account"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_schlafzimmer"; option = "active"; }; }; }
                    { entity = "binary_sensor.schlafzimmer_force_active"; name = "Alles An"; icon = "mdi:lightbulb-on"; tap_action = { action = "call-service"; service = "input_select.select_option"; service_data = { entity_id = "input_select.scene_schlafzimmer"; option = "force-active"; }; }; }
                  ];
                }
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
                  points_per_hour = 3;
                  hours_to_show = 24;
                  update_interval = 30;
                  line_width = 2;
                  state_map = [
                    { value = 0; label = "Aus/Zu"; }
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
                  hours_to_show = 24;
                  update_interval = 30;
                  line_width = 2;
                  hour24 = true;
                  decimals = 1;
                  points_per_hour = 3;
                  state_map = [
                    { value = 0; label = "Aus"; }
                    { value = 1; label = "An"; }
                  ];
                }
                {
                  type = "logbook";
                  entities = [ "input_select.scene_schlafzimmer" "switch.weihnachtsstern_schlafzimmer" "switch.luftentfeuchter" "climate.schlafzimmer" "binary_sensor.schlafzimmerfenster" ];
                }
              ];
            };
          badstack =
            {
              type = "vertical-stack";
              cards = [
                {
                  type = "glance";
                  title = "Bad";
                  columns = 4;
                  show_state = false;
                  entities = [ ];
                }
                {
                  type = "custom:mini-graph-card";
                  entities = [
                    { entity = "sensor.bad_humidity"; name = "Luftfeuchtigkeit"; show_fill = false; state_adaptive_color = true; }
                    { entity = "sensor.luftung"; name = "Lüftung"; color = "#0000ff"; y_axis = "secondary"; show_fill = true; show_points = false; show_line = false; smoothing = false; }
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
                  points_per_hour = 3;
                  hours_to_show = 24;
                  update_interval = 30;
                  line_width = 2;
                  state_map = [
                    { value = 0; label = "Aus"; }
                    { value = 1; label = "An"; }
                  ];
                }
                {
                  type = "custom:mini-graph-card";
                  entities = [
                    { entity = "sensor.bad_temperature"; name = "Temperatur"; show_fill = false; }
                  ];
                  show = {
                    labels = true;
                    labels_secondary = "hover";
                  };
                  lower_bound_secondary = 0;
                  upper_bound_secondary = 1;
                  hours_to_show = 24;
                  update_interval = 30;
                  line_width = 2;
                  hour24 = true;
                  decimals = 1;
                  points_per_hour = 3;
                  state_map = [
                    { value = 0; label = "Aus"; }
                    { value = 1; label = "An"; }
                  ];
                }
                {
                  type = "logbook";
                  entities = [ "switch.lueftung_bad" ];
                }
              ];
            };
          flurstack = {
            type = "vertical-stack";
            cards = [
              (cards.modeSwitcher modes.flat)
              {
                type = "custom:mini-graph-card";
                entities = [
                  { entity = "sensor.wohnungstur"; name = "Wohnungstür"; color = "#ff0000"; show_fill = true; aggregate_func = "max"; smoothing = false; }
                ];
                show = {
                  labels = true;
                };
                lower_bound = 0;
                upper_bound = 1;
                hour24 = true;
                decimals = 1;
                points_per_hour = 3;
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                state_map = [
                  { value = 0; label = "Zu"; }
                  { value = 1; label = "Auf"; }
                ];
              }
              {
                type = "logbook";
                entities = [ "binary_sensor.wohnungstuer" ];
              }
            ];
          };
        in
        {
          views = [
            { icon = "mdi:city"; inherit badges; cards = [ envstack ]; }
            { icon = "mdi:floor-plan"; inherit alertbadges; cards = [ wohnzimmerstack kuechenstack schlafzimmerstack badstack ]; }
            { icon = "mdi:sofa"; inherit badges; cards = [ wohnzimmerstack ]; }
            { icon = "mdi:countertop"; inherit badges; cards = [ kuechenstack ]; }
            { icon = "mdi:bed-king"; inherit badges; cards = [ schlafzimmerstack ]; }
            { icon = "mdi:shower-head"; inherit badges; cards = [ badstack ]; }
            { icon = "mdi:door-closed"; inherit badges; cards = [ flurstack ]; }
          ];
        };
    };
    nginx = {
      enable = true;
      virtualHosts = {
        "home.lo.m-0.eu" = {
          serverAliases = [ "home.vpn.m-0.eu" ];
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
