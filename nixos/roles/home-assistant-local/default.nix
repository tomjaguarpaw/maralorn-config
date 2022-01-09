{ pkgs, lib, ... }:
let
  haLib = import ./lib.nix lib;
  inherit (haLib) modules util cards conditions triggers jinja actions tap_actions;
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
      force_active = {
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
        title = "Wohnungsmodus";
        name = "flat";
        options = { inherit active vacation; };
      };
      wohnzimmer = {
        title = "Wohnzimmermodus";
        name = "wohnzimmer";
        options = { inherit empty heat active force_active; };
      };
      kueche = {
        title = "Küchenmodus";
        name = "kueche";
        options = { inherit empty active; };
      };
      schlafzimmer = {
        title = "Schlafzimmermodus";
        name = "schlafzimmer";
        options = { inherit empty heat active force_active; };
      };
    };
  fenster = map (name: "binary_sensor.${name}")
    [
      "kuechenfenster"
      "wohnzimmerfenster"
      "schlafzimmerfenster"
      "wohnungstuer"
    ];
  batteries = map (name: "sensor.${name}") [
    "wohnzimmerfenster_battery"
    "thermostat_kueche_battery"
    "thermostat_schlafzimmer_battery"
    "thermostat_wohnzimmer_battery"
    "klimasensor_bad_battery"
    "klimasensor_kueche_battery"
    "klimasensor_schlafzimmer_battery"
    "kuechenfenster_battery"
    "pegasus_battery_level"
    "schlafzimmerfenster_battery"
    "wohnungstuer_battery"
  ];
  inherit (import ../../../nix/sources.nix) nixos-unstable;
  homeAssistantDir = "/disk/persist/home-assistant";
in
{

  disabledModules = [
    "services/misc/home-assistant.nix"
  ];

  imports = [
    (modules.mkModeSwitcher modes.wohnzimmer { })
    (modules.mkModeSwitcher modes.kueche { })
    (modules.mkModeSwitcher modes.schlafzimmer { })
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
        shopping_list = { };
        matrix = {
          homeserver = "https://matrix.maralorn.de";
          username = "@marabot:maralorn.de";
          password = pkgs.privateValue "" "matrix/marabot-pw";
        };
        notify = [{ platform = "matrix"; default_room = "#fluffy:maralorn.de"; }];
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
                      { condition = "numeric_state"; entity_id = "sensor.schlafzimmer_humidity"; below = 65; }
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
              (stateTrigger "input_number.target_temperature_kueche")
              (stateTrigger "sensor.kueche_temperature")
              (stateTrigger "binary_sensor.kuechenfenster")
              (stateTrigger "climate.kueche")
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
              (stateTrigger "input_number.target_temperature_wohnzimmer")
              (stateTrigger "binary_sensor.wohnzimmerfenster")
              (stateTrigger "climate.wohnzimmer")
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
            trigger = [ (triggers.modeSwitchTrigger modes.kueche) ];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_kueche";
              data.value = jinja.if' (jinja.isState (util.modeSelectEntity modes.kueche) "empty") "18" "20.5";
            }];
          }
          {
            alias = "Wohnzimmertemperatur";
            trigger = [ (triggers.modeSwitchTrigger modes.wohnzimmer) ];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_wohnzimmer";
              data.value = jinja.if' (jinja.isState (util.modeSelectEntity modes.wohnzimmer) "empty") "18" "24";
            }];
          }
          {
            alias = "Schlafzimmertemperatur";
            trigger = [ (triggers.modeSwitchTrigger modes.schlafzimmer) ];
            action = [{
              service = "input_number.set_value";
              target.entity_id = "input_number.target_temperature_schlafzimmer";
              data.value = jinja.if' (jinja.isState (util.modeSelectEntity modes.schlafzimmer) "empty") "18" "20.5";
            }];
          }
          {
            alias = "Wohnzimmerlichter";
            trigger = with triggers; [
              (modeSwitchTrigger modes.wohnzimmer)
              (stateTrigger "sun.sun")
            ];
            action = [{
              service = jinja.if'
                (jinja.or
                  (jinja.isState (util.modeSelectEntity modes.wohnzimmer) "force_active")
                  (jinja.and
                    (jinja.isState (util.modeSelectEntity modes.wohnzimmer) "active")
                    "state_attr('sun.sun', 'elevation') < 6"))
                "homeassistant.turn_on"
                "homeassistant.turn_off";
              target.entity_id = "group.wohnzimmer_lights";
            }];
          }
          {
            alias = "Schlafzimmerlichter";
            trigger = with triggers; [
              (modeSwitchTrigger modes.schlafzimmer)
              (stateTrigger "sun.sun")
            ];
            action = [{
              service = jinja.if'
                (jinja.or
                  (jinja.isState (util.modeSelectEntity modes.schlafzimmer) "force_active")
                  (jinja.and
                    (jinja.isState (util.modeSelectEntity modes.schlafzimmer) "active")
                    "state_attr('sun.sun', 'elevation') < 6"))
                "homeassistant.turn_on"
                "homeassistant.turn_off";
              target.entity_id = "group.schlafzimmer_lights";
            }];
          }
          {
            alias = "Schlafzimmer vorheizen";
            trigger = [{ platform = "time"; at = "19:00:00"; } { platform = "time"; at = "04:00:00"; }];
            condition = [
              (conditions.modeIs modes.schlafzimmer "empty")
              (conditions.modeIs modes.flat "active")
            ];
            action = [ (actions.setMode modes.schlafzimmer "heat") ];
          }
          {
            alias = "Schlafzimmer nachts kühl";
            trigger = [{ platform = "time"; at = "01:00:00"; }];
            condition = [
              (conditions.modeIs modes.schlafzimmer "heat")
              (conditions.modeIs modes.flat "active")
            ];
            action = [ (actions.setMode modes.schlafzimmer "empty") ];
          }
          {
            alias = "Morgens Licht an";
            trigger = [{ platform = "time"; at = "07:00:00"; }];
            condition = [
              (conditions.modeIs modes.schlafzimmer "heat")
              (conditions.modeIs modes.flat "active")
            ];
            action = [ (actions.setMode modes.schlafzimmer "force_active") ];
          }
          {
            alias = "Warnung bei niedrigem Akkustand";
            trigger = map
              (limit: {
                platform = "numeric_state";
                below = toString limit;
                entity_id = batteries;
              }) [ 25 20 15 10 5 4 3 2 1 ];
            action = [ (actions.notify "{{ trigger.to_state.name }} ist {{ trigger.to_state.value }}%.") ];
          }
          # Warnungen für hohe Luftfeuchtigkeit
        ] ++ (map
          (minutes:
            {
              alias = "Warnung bei lange offenem Fenster";
              trigger = map (name: triggers.stateTrigger name // { to = "on"; for = "00:${toString minutes}:00"; }) fenster;
              action = [ (actions.notify "{{ trigger.to_state.name }} ist seit mehr als ${toString minutes} Minuten offen.") ];
            }) [ 10 20 30 40 50 60 ]);
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
              entities = fenster;
              state_filter = [ "on" ];
            }
            {
              type = "entity-filter";
              entities = batteries;
              state_filter = [{ value = 25; operator = "<"; }];
            }
          ];
          badges =
            let
              badge = mode:
                {
                  type = "state-label";
                  entity = util.modeSelectEntity mode;
                  name = mode.title;
                  tap_action = tap_actions.cycleMode mode;
                };
            in
            [
              (badge modes.wohnzimmer)
              (badge modes.kueche)
              (badge modes.schlafzimmer)
            ] ++ alertbadges;
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
                (cards.modeSwitcher modes.wohnzimmer)
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
                  entities = [ (util.modeSelectEntity modes.wohnzimmer) "binary_sensor.wohnzimmerfenster" "switch.lichterkette_fernseher" "switch.lichterkette_schrank" "switch.blaue_lichterkette" ];
                }
              ];
            };
          kuechenstack = {
            type = "vertical-stack";
            cards = [
              (cards.modeSwitcher modes.kueche)
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
                entities = [ (util.modeSelectEntity modes.kueche) "climate.kueche" "binary_sensor.kuechenfenster" ];
              }
            ];
          };
          schlafzimmerstack =
            {
              type = "vertical-stack";
              cards = [
                (cards.modeSwitcher modes.schlafzimmer)
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
                  entities = [ (util.modeSelectEntity modes.schlafzimmer) "switch.weihnachtsstern_schlafzimmer" "switch.luftentfeuchter" "climate.schlafzimmer" "binary_sensor.schlafzimmerfenster" ];
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
            {
              icon = "mdi:switch";
              badges = alertbadges;
              cards = [
                (cards.modeSwitcher modes.wohnzimmer)
                (cards.modeSwitcher modes.kueche)
                (cards.modeSwitcher modes.schlafzimmer)
                (cards.modeSwitcher modes.flat)
              ];
            }
            { icon = "mdi:sofa"; inherit badges; cards = [ wohnzimmerstack ]; }
            { icon = "mdi:countertop"; inherit badges; cards = [ kuechenstack ]; }
            { icon = "mdi:bed-king"; inherit badges; cards = [ schlafzimmerstack ]; }
            { icon = "mdi:shower-head"; inherit badges; cards = [ badstack ]; }
            { icon = "mdi:door-closed"; inherit badges; cards = [ flurstack ]; }
            { icon = "mdi:city"; inherit badges; cards = [ envstack ]; }
            { icon = "mdi:floor-plan"; badges = alertbadges; cards = [ wohnzimmerstack kuechenstack schlafzimmerstack badstack ]; }
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
