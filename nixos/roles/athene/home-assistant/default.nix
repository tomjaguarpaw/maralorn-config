{
  pkgs,
  lib,
  config,
  ...
}:
let
  haLib = import ./_lib.nix lib;
  colors = rec {
    okay = "#009933";
    warn = "#ffbf00";
    alert = "#ff0000";
    dehumidifier = humidity;
    wall = "#898989";
    heating = "#ff9021";
    temperature = "#7100ff";
    outside_temperature = "#d4b2ff";
    humidity = "#00bfff";
    outside_humidity = "#b2ebff";
    window = "#99046c";
    primary = "#858EFF";
  };
  esp = {
    schlafzimmer = "670dcb"; # Getauscht, war eigentlich in der Küche
    wohnzimmer = "670a54";
    bad = "670dbe";
    kueche = "671af9"; # Getauscht, war eigentlich im Schlafzimmer.
  };
  sensor = {
    schlafzimmer = "${esp.schlafzimmer}_bme280";
    wohnzimmer = "${esp.wohnzimmer}_scd41";
    bad = "${esp.bad}_bme280";
    kueche = "${esp.kueche}_bme280";
  };
  humidity_threshold = {
    schlafzimmer = {
      upper = 81;
      lower = 79;
    };
    bad = {
      upper = 65;
      lower = 62;
    };
  };
  inherit (haLib) triggers;
  #fenster = map (name: "binary_sensor.${name}") [
  #  "kuechenfenster"
  #  "wohnzimmerfenster"
  #  "schlafzimmerfenster"
  #];
  homeAssistantDir = "/disk/persist/home-assistant";
in
{
  systemd.tmpfiles.rules = [ "d ${homeAssistantDir} - - - - -" ];

  services = {
    avahi.enable = true;
    home-assistant = {
      enable = true;
      configDir = homeAssistantDir;
      extraComponents = [
        "esphome"
        "openweathermap"
        "ipp"
        "brother"
        "dwd_weather_warnings"
      ];
      config = {
        prometheus.requires_auth = false;
        default_config = { };
        shopping_list = { };
        group = {
          wohnzimmer_lights = {
            name = "Lichter Wohnzimmer";
            entities = [
              "switch.blaue_lichterkette"
              "switch.lichterkette_schrank"
              "switch.lichterkette_kartenwand"
            ];
          };
          schlafzimmer_lights = {
            name = "Lichter Schlafzimmer";
            entities = [
              "switch.lichterkette_schlafzimmer"
              "switch.weihnachtsstern_schlafzimmer"
            ];
          };
        };
        homeassistant = pkgs.privateValue { } "homeassistant-home";
        frontend.themes.ourdefault.modes.dark.primary-color = colors.primary;
        timer.block_heating_schlafzimmer = { };
        automation = [
          {
            alias = "Set theme at startup'";
            trigger = {
              platform = "homeassistant";
              event = "start";
            };
            action = {
              service = "frontend.set_theme";
              data.name = "ourdefault";
            };
          }
          {
            alias = "Entfeuchtersteuerung Schlafzimmer";
            trigger = [
              (triggers.stateTrigger "sensor.wall_humidity_schlafzimmer")
              (triggers.stateTrigger "binary_sensor.schlafzimmerfenster")
            ];
            action = [
              {
                choose = [
                  {
                    conditions = [
                      {
                        condition = "or";
                        conditions = [
                          {
                            condition = "numeric_state";
                            entity_id = "sensor.wall_humidity_schlafzimmer";
                            below = humidity_threshold.schlafzimmer.lower;
                          }
                          {
                            condition = "state";
                            entity_id = "binary_sensor.schlafzimmerfenster";
                            state = [ "on" ];
                          }
                        ];
                      }
                    ];
                    sequence = {
                      service = "switch.turn_off";
                      target.entity_id = "switch.luftentfeuchter";
                    };
                  }
                  {
                    conditions = [
                      {
                        condition = "numeric_state";
                        entity_id = "sensor.wall_humidity_schlafzimmer";
                        above = humidity_threshold.schlafzimmer.upper;
                      }
                    ];
                    sequence = {
                      service = "switch.turn_on";
                      target.entity_id = "switch.luftentfeuchter";
                    };
                  }
                ];
              }
            ];
          }
          {
            alias = "Lüftungssteuerung Bad";
            trigger = [ (triggers.stateTrigger "sensor.${sensor.bad}_humidity") ];
            action = [
              {
                choose = [
                  {
                    conditions = "{{ states('sensor.${sensor.bad}_dew_point')|float > states('sensor.openweathermap_darmstadt_hourly_dew_point')|float(0) + 2 and states('sensor.${sensor.bad}_humidity')|float > ${toString humidity_threshold.bad.upper} }}";
                    sequence = {
                      service = "switch.turn_on";
                      target.entity_id = "switch.lueftung_bad";
                    };
                  }
                  {
                    conditions = "{{ states('sensor.${sensor.bad}_dew_point')|float < states('sensor.openweathermap_darmstadt_hourly_dew_point')|float(0) + 1 or states('sensor.${sensor.bad}_humidity')|float < ${toString humidity_threshold.bad.lower} }}";
                    sequence = {
                      service = "switch.turn_off";
                      target.entity_id = "switch.lueftung_bad";
                    };
                  }
                ];
              }
            ];
          }
          #{
          #  alias = "Backup Lüftungssteuerung Bad";
          #  trigger = [
          #    (triggers.stateTrigger "switch.lueftung_bad"
          #      // {
          #        to = "on";
          #        for = "03:00:00";
          #      })
          #  ];
          #  action = [
          #    {
          #      service = "switch.turn_off";
          #      target.entity_id = "switch.lueftung_bad";
          #    }
          #  ];
          #}
          {
            alias = "Schlafzimmerfenstertimer";
            trigger = [ (triggers.stateTrigger "binary_sensor.schlafzimmerfenster") ];
            action = {
              service = "timer.start";
              data = {
                entity_id = "timer.block_heating_schlafzimmer";
                duration = "00:10:00";
              };
            };
          }
          {
            alias = "Heizungsblock Schlafzimmer";
            trigger =
              let
                inherit (triggers) stateTrigger;
              in
              [
                (stateTrigger "binary_sensor.schlafzimmerfenster")
                (stateTrigger "timer.block_heating_schlafzimmer")
              ];
            action = [
              {
                choose = [
                  {
                    conditions = [
                      {
                        condition = "state";
                        entity_id = "timer.block_heating_schlafzimmer";
                        state = [ "idle" ];
                      }
                      {
                        condition = "state";
                        entity_id = "binary_sensor.schlafzimmerfenster";
                        state = [
                          "off"
                          "unavailable"
                        ];
                      }
                    ];
                    sequence = {
                      service = "climate.turn_on";
                      target.area_id = "schlafzimmer";
                    };
                  }
                ];
                default = {
                  service = "climate.turn_off";
                  target.area_id = "schlafzimmer";
                };
              }
            ];
          }
          {
            alias = "Thermostatkorrektur Schlafzimmer";
            trigger =
              let
                inherit (triggers) stateTrigger;
              in
              [
                (stateTrigger "sensor.${sensor.schlafzimmer}_temperature")
                (stateTrigger "climate.schlafzimmer" // { attribute = "current_temperature"; })
              ];
            actions = [
              {
                action = "number.set_value";
                target.entity_id = "number.heizkorper_schlafzimmer_number_temperature_offset";
                data.value = "{{ (states('number.heizkorper_schlafzimmer_number_temperature_offset') | float + states('sensor.${sensor.schlafzimmer}_temperature') | float - state_attr('climate.schlafzimmer','current_temperature') | float) | round(0) }}";

              }
            ];
          }
          #{
          #  alias = "Warnung bei hohem CO2";
          #  trigger =
          #    map
          #      (limit: {
          #        platform = "numeric_state";
          #        above = toString limit;
          #        entity_id = [
          #          "sensor.${sensor.schlafzimmer}_co2"
          #          "sensor.${sensor.wohnzimmer}_co2"
          #        ];
          #      })
          #      [
          #        1500
          #        2000
          #        2500
          #        3000
          #      ];
          #  action = [ (actions.notify "{{ trigger.to_state.name }} ist {{ trigger.to_state.state }} ppm.") ];
          #}
          #{
          #  alias = "Aufwachen";
          #  trigger = [
          #    {
          #      platform = "time";
          #      at = "07:00:00";
          #    }
          #  ];
          #  action = [
          #    {
          #      service = "switch.turn_on";
          #      target.entity_id = "group.schlafzimmer_lights";
          #    }
          #  ];
          #}
          #{
          #  alias = "Wach";
          #  trigger = [
          #    {
          #      platform = "time";
          #      at = "08:00:00";
          #    }
          #  ];
          #  action = [
          #    {
          #      service = "switch.turn_off";
          #      target.entity_id = "group.schlafzimmer_lights";
          #    }
          #  ];
          #}
        ]
        #++ (map
        #  (minutes: {
        #    alias = "Warnung bei ${minutes} Minuten offenem Fenster oder offener Tür";
        #    trigger = map (
        #      name:
        #      triggers.stateTrigger name
        #      // {
        #        to = "on";
        #        for = "00:${minutes}:00";
        #      }
        #    ) fenster;
        #    condition = {
        #      condition = "numeric_state";
        #      entity_id = "sensor.openweathermap_darmstadt_hourly_temperature";
        #      attribute = "temperature";
        #      below = 16;
        #    };
        #    action = [
        #      (actions.notify "{{ trigger.to_state.name }} ist seit mehr als ${minutes} Minuten offen.")
        #    ];
        #  })
        #  (
        #    map toString [
        #      10
        #      20
        #      30
        #      40
        #      50
        #      60
        #    ]
        #  )
        #);
        ;
        recorder = { };
        template = [
          {
            sensor = [
              {
                state = "{% if is_state('switch.luftentfeuchter', 'on') %}1{% else %}0{% endif %}";
                name = "Luftentfeuchter";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('switch.lueftung_bad', 'on') %}1{% else %}0{% endif %}";
                name = "Lüftung";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('binary_sensor.schlafzimmerfenster', 'on') %}1{% else %}0{% endif %}";
                name = "Schlafzimmerfenster";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('binary_sensor.wohnzimmerfenster', 'on') %}1{% else %}0{% endif %}";
                name = "Balkontür";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('binary_sensor.kuechenfenster', 'on') %}1{% else %}0{% endif %}";
                name = "Küchenfenster";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('binary_sensor.wohnungstuer', 'on') %}1{% else %}0{% endif %}";
                name = "Wohnungstür";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('climate.schlafzimmer', 'heat') %}1{% else %}0{% endif %}";
                name = "Schlafzimmerheizung";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('climate.wohnzimmer', 'heat') %}1{% else %}0{% endif %}";
                name = "Wohnzimmerheizung";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{% if is_state('climate.kueche', 'heat') %}1{% else %}0{% endif %}";
                name = "Küchenheizung";
              }
            ];
          }
          {
            sensor = [
              {
                state = "{{ state_attr('climate.wohnzimmer', 'current_temperature') }}";
                name = "Temperatur Wohnzimmer";
              }
            ];
          }
        ];
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
        sensor = [
          {
            platform = "rmvtransport";
            next_departure = [ { station = "3024634"; } ];
          }
          {
            platform = "filter";
            entity_id = "sensor.openweathermap_darmstadt_hourly_temperature";
            name = "Smoothed Outside Temperature";
            filters = {
              filter = "lowpass";
              time_constant = 30;
            };
          }
          {
            name = "Wall Humidity Schlafzimmer";
            platform = "mold_indicator";
            indoor_temp_sensor = "sensor.${sensor.schlafzimmer}_temperature";
            indoor_humidity_sensor = "sensor.${sensor.schlafzimmer}_humidity";
            outdoor_temp_sensor = "sensor.smoothed_outside_temperature";
            calibration_factor = 1.9;
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
          envstack = {
            type = "vertical-stack";
            cards = [
              { type = "custom:sun-card"; }
              {
                type = "picture";
                image = "https://www.dwd.de/DWD/wetter/radar/radfilm_hes_akt.gif";
              }
              {
                type = "weather-forecast";
                entity = "weather.openweathermap_darmstadt_hourly";
              }
              {
                type = "weather-forecast";
                entity = "weather.openweathermap_darmstadt_daily";
              }
              {
                type = "custom:rmv-card";
                entity = "sensor.darmstadt_schulstrasse";
              }
            ];
          };
          multiScaleGraphCards =
            config:
            let
              mkConfig = lib.recursiveUpdate (
                lib.recursiveUpdate config {
                  line_width = 3;
                  font_size = 60;
                  font_size_header = 12;
                  align_header = "left";
                  height = 200;
                  align_state = "right";
                  show = {
                    labels = true;
                    labels_secondary = "hover";
                    legend = false;
                    name = true;
                    icon = false;
                    state = true;
                  };
                }
              );
            in
            {
              type = "horizontal-stack";
              cards = [
                (mkConfig {
                  hours_to_show = 168;
                  points_per_hour = 1;
                })
                (mkConfig {
                  hours_to_show = 24;
                  points_per_hour = 3;
                })
                (mkConfig {
                  hours_to_show = 1;
                  points_per_hour = 60;
                })
              ];
            };
          graph.outside = {
            dew_point = {
              entity = "sensor.openweathermap_darmstadt_hourly_dew_point";
              name = "Taupunkt draußen";
              show_fill = false;
              color = colors.outside_humidity;
            };
            humidity = {
              entity = "sensor.openweathermap_darmstadt_hourly_humidity";
              name = "Luftfeuchtigkeit draußen";
              show_fill = false;
              color = colors.outside_humidity;
            };
            temperature = {
              entity = "sensor.openweathermap_darmstadt_hourly_temperature";
              name = "Temperatur draußen";
              show_fill = false;
              color = colors.outside_temperature;
            };
          };
          wohnzimmerstack = {
            type = "vertical-stack";
            cards = [
              {
                type = "entities";
                title = "Wohnzimmer";
                entities = [
                  {
                    entity = "climate.wohnzimmer";
                    name = "Heizung";
                  }
                  {
                    entity = "number.heizkorper_wohnzimmer_number_temperature_offset";
                    name = "Thermostatkorrektur";
                  }
                  {
                    entity = "group.wohnzimmer_lights";
                    name = "Lichter";
                  }
                  #{
                  #  entity = "button.restart_${esp.wohnzimmer}";
                  #  name = "Klimasensor Neustarten";
                  #}
                  #{
                  #  entity = "button.${sensor.wohnzimmer}_force_calibration";
                  #  name = "CO2 Kalibrieren";
                  #}
                ];
              }
              #(multiScaleGraphCards {
              #  type = "custom:mini-graph-card";
              #  entities = [
              #    {
              #      entity = "sensor.${sensor.wohnzimmer}_co2";
              #      name = "CO2";
              #      show_fill = false;
              #    }
              #    {
              #      entity = "input_number.ambient_co2";
              #      show_fill = false;
              #      color = colors.warn;
              #    }
              #  ];
              #  color_thresholds = [
              #    {
              #      value = 0;
              #      color = colors.okay;
              #    }
              #    {
              #      value = 1000;
              #      color = colors.warn;
              #    }
              #    {
              #      value = 2000;
              #      color = colors.alert;
              #    }
              #  ];
              #  color_thresholds_transition = "hard";
              #  show = {
              #    labels = true;
              #    labels_secondary = "hover";
              #  };
              #  hours_to_show = 24;
              #  update_interval = 30;
              #  line_width = 2;
              #  hour24 = true;
              #  decimals = 1;
              #  points_per_hour = 3;
              #})
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "climate.wohnzimmer";
                    attribute = "current_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "climate.wohnzimmer";
                    attribute = "temperature";
                    name = "Zieltemperatur";
                    show_fill = false;
                    color = colors.heating;
                  }
                  {
                    entity = "sensor.wohnzimmerheizung";
                    name = "Heizung";
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                    color = colors.heating;
                  }
                  {
                    entity = "sensor.balkontur";
                    name = "Balkontür";
                    color = colors.window;
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                  }
                ];
                lower_bound_secondary = 0;
                upper_bound_secondary = 1;
                update_interval = 30;
                line_width = 2;
                hour24 = true;
                decimals = 1;
                state_map = [
                  {
                    value = 0;
                    label = "Aus";
                  }
                  {
                    value = 1;
                    label = "An";
                  }
                ];
              })
              #(multiScaleGraphCards {
              #  type = "custom:mini-graph-card";
              #  entities = [
              #    {
              #      entity = "sensor.${sensor.wohnzimmer}_humidity";
              #      name = "Luftfeuchtigkeit";
              #      show_fill = false;
              #      color = colors.humidity;
              #    }
              #    graph.outside.humidity
              #  ];
              #  show = {
              #    labels = true;
              #    labels_secondary = "hover";
              #  };
              #  lower_bound_secondary = 0;
              #  upper_bound_secondary = 1;
              #  hour24 = true;
              #  decimals = 1;
              #  points_per_hour = 3;
              #  hours_to_show = 24;
              #  update_interval = 30;
              #  line_width = 2;
              #  state_map = [
              #    {
              #      value = 0;
              #      label = "Aus/Zu";
              #    }
              #    {
              #      value = 1;
              #      label = "An/Auf";
              #    }
              #  ];
              #})
              {
                type = "logbook";
                entities = [
                  "climate.wohnzimmer"
                  "number.heizkorper_wohnzimmer_number_temperature_offset"
                  "binary_sensor.wohnzimmerfenster"
                  "switch.lichterkette_fernseher"
                  "switch.lichterkette_schrank"
                  "switch.blaue_lichterkette"
                ];
              }
            ];
          };
          kuechenstack = {
            type = "vertical-stack";
            cards = [
              {
                type = "entities";
                title = "Küche";
                entities = [
                  {
                    entity = "climate.kueche";
                    name = "Heizung";
                  }
                  {
                    entity = "number.heizkorper_kuche_number_temperature_offset";
                    name = "Thermostatkorrektur";
                  }
                  #{
                  #  entity = "button.restart_${esp.kueche}";
                  #  name = "Klimasensor Neustarten";
                  #}
                ];
              }
              #(multiScaleGraphCards {
              #  type = "custom:mini-graph-card";
              #  entities = [
              #    {
              #      entity = "sensor.${sensor.kueche}_humidity";
              #      name = "Luftfeuchtigkeit";
              #      show_fill = false;
              #      color = colors.humidity;
              #    }
              #    graph.outside.humidity
              #    {
              #      entity = "sensor.kuchenfenster";
              #      name = "Fenster";
              #      color = colors.window;
              #      y_axis = "secondary";
              #      show_fill = true;
              #      show_points = false;
              #      show_line = false;
              #      smoothing = false;
              #    }
              #  ];
              #  show = {
              #    labels = true;
              #    labels_secondary = "hover";
              #  };
              #  lower_bound_secondary = 0;
              #  upper_bound_secondary = 1;
              #  hour24 = true;
              #  decimals = 1;
              #  points_per_hour = 3;
              #  hours_to_show = 24;
              #  update_interval = 30;
              #  line_width = 2;
              #  state_map = [
              #    {
              #      value = 0;
              #      label = "Zu";
              #    }
              #    {
              #      value = 1;
              #      label = "Auf";
              #    }
              #  ];
              #})
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "climate.kueche";
                    attribute = "current_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "climate.kueche";
                    attribute = "temperature";
                    name = "Zieltemperatur";
                    show_fill = false;
                    color = colors.heating;
                  }
                  #{
                  #  entity = "sensor.${sensor.kueche}_dew_point";
                  #  name = "Temperatur";
                  #  show_fill = false;
                  #  color = colors.humidity;
                  #}
                  {
                    entity = "sensor.kuchenheizung";
                    name = "Heizung";
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                    color = colors.heating;
                  }
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
                  {
                    value = 0;
                    label = "Aus";
                  }
                  {
                    value = 1;
                    label = "An";
                  }
                ];
              })
              {
                type = "logbook";
                entities = [
                  "climate.kueche"
                  "binary_sensor.kuechenfenster"
                ];
              }
            ];
          };
          schlafzimmerstack = {
            type = "vertical-stack";
            cards = [
              {
                type = "entities";
                title = "Schlafzimmer";
                entities = [
                  {
                    entity = "climate.schlafzimmer";
                    name = "Heizung";
                  }
                  {
                    entity = "number.heizkorper_schlafzimmer_number_temperature_offset";
                    name = "Thermostatkorrektur";
                    unit = "°C";
                  }
                  {
                    entity = "group.schlafzimmer_lights";
                    name = "Lichter";
                  }
                  {
                    entity = "button.restart_${esp.schlafzimmer}";
                    name = "Klimasensor Neustarten";
                  }
                  { type = "divider"; }
                  {
                    entity = "sensor.${sensor.schlafzimmer}_temperature";
                    name = "Temperatur";
                    unit = "°C";
                  }
                  {
                    type = "attribute";
                    entity = "sensor.wall_humidity_schlafzimmer";
                    name = "Geschätzte Wandtemperatur";
                    attribute = "estimated_critical_temp";
                    icon = "mdi:home-thermometer";
                    unit = "°C";
                    format = "precision1";
                  }
                  {
                    entity = "sensor.wall_humidity_schlafzimmer";
                    name = "Geschätzte Wandfeuchtigkeit";
                    icon = "mdi:water-percent";
                    unit = "%";
                    format = "precision0";
                  }
                  #{
                  #  entity = "button.${sensor.schlafzimmer}_force_calibration";
                  #  name = "CO2 Kalibrieren";
                  #}
                ];
              }
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "climate.schlafzimmer";
                    attribute = "current_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  {
                    entity = "sensor.wall_humidity_schlafzimmer";
                    name = "Geschätze Wandtemperatur";
                    attribute = "estimated_critical_temp";
                    show_fill = false;
                    color = colors.wall;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "sensor.${sensor.schlafzimmer}_dew_point";
                    name = "Taupunkt";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  {
                    entity = "climate.schlafzimmer";
                    attribute = "temperature";
                    name = "Zieltemperatur";
                    show_fill = false;
                    color = colors.heating;
                  }
                  {
                    entity = "sensor.schlafzimmerheizung";
                    name = "Heizung";
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                    color = colors.heating;
                  }
                  {
                    entity = "sensor.schlafzimmerfenster";
                    name = "Fenster";
                    color = colors.window;
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                  }
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
                  {
                    value = 0;
                    label = "Aus";
                  }
                  {
                    value = 1;
                    label = "An";
                  }
                ];
              })
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.wall_humidity_schlafzimmer";
                    name = "Geschätzte Wandfeuchtigkeit";
                    show_fill = false;
                    state_adaptive_color = true;
                  }
                  {
                    entity = "sensor.${sensor.schlafzimmer}_humidity";
                    name = "Luftfeuchtigkeit";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  graph.outside.humidity
                  {
                    entity = "sensor.luftentfeuchter";
                    name = "Entfeuchter";
                    color = colors.dehumidifier;
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                  }
                ];
                color_thresholds = [
                  {
                    value = 0;
                    color = colors.okay;
                  }
                  {
                    value = humidity_threshold.schlafzimmer.lower;
                    color = colors.warn;
                  }
                  {
                    value = humidity_threshold.schlafzimmer.upper;
                    color = colors.alert;
                  }
                ];
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
                  {
                    value = 0;
                    label = "Aus/Zu";
                  }
                  {
                    value = 1;
                    label = "An/Auf";
                  }
                ];
              })
              #(multiScaleGraphCards {
              #  type = "custom:mini-graph-card";
              #  entities = [
              #    {
              #      entity = "sensor.${sensor.schlafzimmer}_co2";
              #      name = "CO2";
              #      show_fill = false;
              #    }
              #  ];
              #  show = {
              #    labels = true;
              #    labels_secondary = "hover";
              #  };
              #  hours_to_show = 24;
              #  update_interval = 30;
              #  line_width = 2;
              #  hour24 = true;
              #  decimals = 1;
              #  points_per_hour = 3;
              #})
              {
                type = "logbook";
                entities = [
                  "switch.weihnachtsstern_schlafzimmer"
                  "switch.luftentfeuchter"
                  "number.heizkorper_schlafzimmer_number_temperature_offset"
                  "climate.schlafzimmer"
                  "binary_sensor.schlafzimmerfenster"
                ];
              }
            ];
          };
          badstack = {
            type = "vertical-stack";
            cards = [
              {
                type = "entities";
                title = "Bad";
                entities = [
                  {
                    entity = "button.restart_${esp.bad}";
                    name = "Klimasensor Neustarten";
                  }
                ];
              }
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.bad}_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "sensor.${sensor.bad}_dew_point";
                    name = "Taupunkt";
                    show_fill = false;
                    color = colors.humidity;
                  }
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
                  {
                    value = 0;
                    label = "Aus";
                  }
                  {
                    value = 1;
                    label = "An";
                  }
                ];
              })
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.bad}_humidity";
                    name = "Luftfeuchtigkeit";
                    show_fill = false;
                  }
                  graph.outside.humidity
                  {
                    entity = "sensor.luftung";
                    name = "Lüftung";
                    color = colors.dehumidifier;
                    y_axis = "secondary";
                    show_fill = true;
                    show_points = false;
                    show_line = false;
                    smoothing = false;
                  }
                ];
                show = {
                  labels = true;
                  labels_secondary = "hover";
                };
                color_thresholds = [
                  {
                    value = 0;
                    color = colors.okay;
                  }
                  {
                    value = humidity_threshold.bad.lower;
                    color = colors.warn;
                  }
                  {
                    value = humidity_threshold.bad.upper;
                    color = colors.alert;
                  }
                ];
                color_thresholds_transition = "hard";
                lower_bound_secondary = 0;
                upper_bound_secondary = 1;
                hour24 = true;
                decimals = 1;
                points_per_hour = 3;
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                state_map = [
                  {
                    value = 0;
                    label = "Aus";
                  }
                  {
                    value = 1;
                    label = "An";
                  }
                ];
              })
              {
                type = "logbook";
                entities = [ "switch.lueftung_bad" ];
              }
            ];
          };
        in
        {
          views = [
            # panels don’t support badges
            {
              icon = "mdi:sofa";
              type = "panel";
              cards = [ wohnzimmerstack ];
            }
            {
              icon = "mdi:countertop";
              type = "panel";
              cards = [ kuechenstack ];
            }
            {
              icon = "mdi:bed-king";
              type = "panel";
              cards = [ schlafzimmerstack ];
            }
            {
              icon = "mdi:shower-head";
              type = "panel";
              cards = [ badstack ];
            }
            {
              icon = "mdi:city";
              cards = [ envstack ];
            }
            {
              icon = "mdi:floor-plan";
              cards = [
                wohnzimmerstack
                schlafzimmerstack
                kuechenstack
                badstack
              ];
            }
          ];
        };
    };
    nginx.virtualHosts.${config.m-0.virtualHosts.home} = {
      serverAliases = [ "home.local.maralorn.de" ];
      extraConfig = "proxy_buffering off;";
      locations = {
        "/" = {
          proxyPass = "http://[::1]:8123";
          proxyWebsockets = true;
        };
        "/custom/".alias = "/run/hass/";
      };
    };
  };
}
