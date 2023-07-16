{
  pkgs,
  lib,
  config,
  ...
}:
let
  haLib = import ./lib.nix lib;
  colors = rec {
    okay = "#009933";
    warn = "#ffbf00";
    alert = "#ff0000";
    dehumidifier = humidity;
    heating = "#ff9021";
    temperature = "#7100ff";
    outside_temperature = "#d4b2ff";
    humidity = "#00bfff";
    outside_humidity = "#b2ebff";
    window = "#99046c";
    primary = "#858EFF";
  };
  esp = {
    schlafzimmer = "671af9";
    wohnzimmer = "670a54";
    bad = "670dbe";
    kueche = "670dcb";
  };
  sensor = {
    schlafzimmer = "${esp.schlafzimmer}_bme280";
    wohnzimmer = "${esp.wohnzimmer}_scd41";
    bad = "${esp.bad}_bme280";
    kueche = "${esp.kueche}_bme280";
  };
  humidity_threshold = {
    schlafzimmer = {
      upper = 60;
      lower = 58;
    };
    bad = {
      upper = 60;
      lower = 55;
    };
  };
  inherit (haLib) triggers actions;
  fenster = map (name: "binary_sensor.${name}") [
    "kuechenfenster"
    "wohnzimmerfenster"
    "schlafzimmerfenster"
  ];
  switches = map (name: "switch.${name}") [
    "weihnachtsstern_schlafzimmer"
    "luftentfeuchter"
    "lueftung_bad"
    "lichterkette_schrank"
    "lichterkette_fernseher"
    "blaue_lichterkette"
  ];
  batteries = map (name: "sensor.${name}") [
    "wohnzimmerfenster_battery"
    "thermostat_kueche_battery"
    "thermostat_schlafzimmer_battery"
    "thermostat_wohnzimmer_battery"
    "klimasensor_schlafzimmer_battery"
    "kuechenfenster_battery"
    "pegasus_battery_level"
    "kalliope_battery_level"
    "schlafzimmerfenster_battery"
  ];
  homeAssistantDir = "/disk/persist/home-assistant";
in
{
  imports = [ ./hexa-cards.nix ];

  systemd.tmpfiles.rules = [ "d ${homeAssistantDir} - - - - -" ];

  services = {
    avahi.enable = true;
    home-assistant = {
      enable = true;
      configDir = homeAssistantDir;
      extraComponents = [ ];
      config = {
        esphome = { };
        default_config = { };
        shopping_list = { };
        openweathermap = { };
        matrix = {
          homeserver = "https://matrix.maralorn.de";
          username = "@marabot:maralorn.de";
          password = pkgs.privateValue "" "matrix/marabot-pw";
        };
        notify = [ {
          name = "matrix";
          platform = "matrix";
          default_room = "#fluffy:maralorn.de";
        } ];
        group = {
          wohnzimmer_lights = {
            name = "Lichter Wohnzimmer";
            entities = [
              "switch.blaue_lichterkette"
              "switch.lichterkette_schrank"
              "switch.lichterkette_fernseher"
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
        automation =
          [
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
                (triggers.stateTrigger "sensor.${sensor.schlafzimmer}_humidity")
                (triggers.stateTrigger "binary_sensor.schlafzimmerfenster")
              ];
              action = [ {
                choose = [
                  {
                    conditions = [ {
                      condition = "or";
                      conditions = [
                        {
                          condition = "numeric_state";
                          entity_id = "sensor.${sensor.schlafzimmer}_humidity";
                          below = humidity_threshold.schlafzimmer.lower;
                        }
                        {
                          condition = "state";
                          entity_id = "binary_sensor.schlafzimmerfenster";
                          state = [ "on" ];
                        }
                      ];
                    } ];
                    sequence = {
                      service = "switch.turn_off";
                      target.entity_id = "switch.luftentfeuchter";
                    };
                  }
                  {
                    conditions = [ {
                      condition = "numeric_state";
                      entity_id = "sensor.${sensor.schlafzimmer}_humidity";
                      above = humidity_threshold.schlafzimmer.upper;
                    } ];
                    sequence = {
                      service = "switch.turn_on";
                      target.entity_id = "switch.luftentfeuchter";
                    };
                  }
                ];
              } ];
            }
            {
              alias = "Lüftungssteuerung Bad";
              trigger = [ (triggers.stateTrigger "sensor.${sensor.bad}_humidity") ];
              action = [ {
                choose = [
                  {
                    conditions = "{{ states('sensor.${sensor.bad}_dew_point')|float > states('sensor.openweathermap_darmstadt_hourly_dew_point')|float(0) + 2 and states('sensor.${sensor.bad}_humidity')|float > ${
                        toString humidity_threshold.bad.upper
                      } }}";
                    sequence = {
                      service = "switch.turn_on";
                      target.entity_id = "switch.lueftung_bad";
                    };
                  }
                  {
                    conditions = "{{ states('sensor.${sensor.bad}_dew_point')|float < states('sensor.openweathermap_darmstadt_hourly_dew_point')|float(0) + 1 or states('sensor.${sensor.bad}_humidity')|float < ${
                        toString humidity_threshold.bad.lower
                      } }}";
                    sequence = {
                      service = "switch.turn_off";
                      target.entity_id = "switch.lueftung_bad";
                    };
                  }
                ];
              } ];
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
              trigger = with triggers; [ (stateTrigger "binary_sensor.schlafzimmerfenster") ];
              action = {
                service = "timer.start";
                data = {
                  entity_id = "timer.block_heating_schlafzimmer";
                  duration = "00:10:00";
                };
              };
            }
            {
              alias = "Thermostatsteuerung Schlafzimmer";
              trigger = with triggers; [
                (stateTrigger "input_number.target_temperature_schlafzimmer")
                (stateTrigger "sensor.${sensor.schlafzimmer}_temperature")
                (stateTrigger "binary_sensor.schlafzimmerfenster")
                (stateTrigger "climate.schlafzimmer")
                (stateTrigger "timer.block_heating_schlafzimmer")
              ];
              action = [ {
                choose = [ {
                  conditions = [
                    {
                      condition = "numeric_state";
                      entity_id = "sensor.${sensor.schlafzimmer}_temperature";
                      below = "input_number.target_temperature_schlafzimmer";
                    }
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
                    service = "climate.set_temperature";
                    target.area_id = "schlafzimmer";
                    data = {
                      temperature = 30;
                      hvac_mode = "heat";
                    };
                  };
                } ];
                default = {
                  service = "climate.turn_off";
                  target.area_id = "schlafzimmer";
                };
              } ];
            }
            {
              alias = "Thermostatsteuerung Küche";
              trigger = with triggers; [
                (stateTrigger "input_number.target_temperature_kueche")
                (stateTrigger "sensor.${sensor.kueche}_temperature")
                (stateTrigger "binary_sensor.kuechenfenster")
                (stateTrigger "climate.kueche")
              ];
              action = [ {
                choose = [ {
                  conditions = [
                    {
                      condition = "numeric_state";
                      entity_id = "sensor.${sensor.kueche}_temperature";
                      below = "input_number.target_temperature_kueche";
                    }
                    {
                      condition = "state";
                      entity_id = "binary_sensor.kuechenfenster";
                      state = [
                        "off"
                        "unavailable"
                      ];
                    }
                  ];
                  sequence = {
                    service = "climate.set_temperature";
                    target.area_id = "kuche";
                    data = {
                      temperature = 30;
                      hvac_mode = "heat";
                    };
                  };
                } ];
                default = {
                  service = "climate.turn_off";
                  target.area_id = "kuche";
                };
              } ];
            }
            {
              alias = "Thermostatsteuerung Wohnzimmer";
              trigger = with triggers; [
                (stateTrigger "input_number.target_temperature_wohnzimmer")
                (stateTrigger "sensor.${sensor.wohnzimmer}_temperature")
                (stateTrigger "binary_sensor.wohnzimmerfenster")
                (stateTrigger "climate.wohnzimmer")
              ];
              action = [ {
                choose = [ {
                  conditions = [
                    {
                      condition = "numeric_state";
                      entity_id = "sensor.${sensor.wohnzimmer}_temperature";
                      below = "input_number.target_temperature_wohnzimmer";
                    }
                    {
                      condition = "state";
                      entity_id = "binary_sensor.wohnzimmerfenster";
                      state = [
                        "off"
                        "unavailable"
                      ];
                    }
                  ];
                  sequence = {
                    service = "climate.set_temperature";
                    target.area_id = "wohnzimmer";
                    data = {
                      temperature = 30;
                      hvac_mode = "heat";
                    };
                  };
                } ];
                default = {
                  service = "climate.turn_off";
                  target.area_id = "wohnzimmer";
                };
              } ];
            }
            {
              alias = "Warnung bei hohem CO2";
              trigger =
                map
                  (limit: {
                    platform = "numeric_state";
                    above = toString limit;
                    entity_id = [
                      "sensor.${sensor.schlafzimmer}_co2"
                      "sensor.${sensor.wohnzimmer}_co2"
                    ];
                  })
                  [
                    1500
                    2000
                    2500
                    3000
                  ]
              ;
              action = [
                (actions.notify
                  "{{ trigger.to_state.name }} ist {{ trigger.to_state.state }} ppm."
                )
              ];
            }
            {
              alias = "Warnung bei niedrigem Akkustand";
              trigger =
                map
                  (limit: {
                    platform = "numeric_state";
                    below = toString limit;
                    entity_id = batteries;
                  })
                  [
                    25
                    20
                    15
                    10
                    5
                    4
                    3
                    2
                    1
                    0
                  ]
              ;
              action = [
                (actions.notify
                  "{{ trigger.to_state.name }} ist {{ trigger.to_state.state }}%."
                )
              ];
            }
            {
              alias = "Abend";
              trigger = [ {
                platform = "time";
                at = "22:50:00";
              } ];
              action = [ (actions.notify "Es ist 22:50 Uhr.") ];
            }
          ]
          ++ (map
            (entity: {
              alias = "Unreachable Warnung";
              trigger = triggers.stateTrigger entity // {
                to = "unavailable";
                for = "01:00:00";
              };
              action = [
                (actions.notify "{{ trigger.to_state.name }} ist seit 1h unerreichbar.")
              ];
            })
            [
              "switch.luftentfeuchter"
              "switch.lueftung_bad"
              "sensor.${sensor.bad}_humidity"
              "sensor.${sensor.schlafzimmer}_humidity"
            ]
          )
          #condition = {
          #  condition = "numeric_state";
          #  entity_id = "weather.dwd_darmstadt";
          #  attribute = "temperature";
          #  below = 15;
          #};
          ++ (map
            (minutes: {
              alias = "Warnung bei ${minutes} Minuten offenem Fenster oder offener Tür";
              trigger =
                map
                  (
                    name:
                    triggers.stateTrigger name
                    // {
                      to = "on";
                      for = "00:${minutes}:00";
                    }
                  )
                  fenster
              ;
              condition = {
                condition = "numeric_state";
                entity_id = "sensor.openweathermap_darmstadt_hourly_temperature";
                attribute = "temperature";
                below = 16;
              };
              action = [
                (actions.notify
                  "{{ trigger.to_state.name }} ist seit mehr als ${minutes} Minuten offen."
                )
              ];
            })
            (
              map toString [
                10
                20
                30
                40
                50
                60
              ]
            )
          )
        ;
        recorder = { };
        template = [
          {
            sensor = [ {
              state = "{% if is_state('switch.luftentfeuchter', 'on') %}1{% else %}0{% endif %}";
              name = "Luftentfeuchter";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('switch.lueftung_bad', 'on') %}1{% else %}0{% endif %}";
              name = "Lüftung";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('binary_sensor.schlafzimmerfenster', 'on') %}1{% else %}0{% endif %}";
              name = "Schlafzimmerfenster";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('binary_sensor.wohnzimmerfenster', 'on') %}1{% else %}0{% endif %}";
              name = "Balkontür";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('binary_sensor.kuechenfenster', 'on') %}1{% else %}0{% endif %}";
              name = "Küchenfenster";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('binary_sensor.wohnungstuer', 'on') %}1{% else %}0{% endif %}";
              name = "Wohnungstür";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('climate.schlafzimmer', 'heat') %}1{% else %}0{% endif %}";
              name = "Schlafzimmerheizung";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('climate.wohnzimmer', 'heat') %}1{% else %}0{% endif %}";
              name = "Wohnzimmerheizung";
            } ];
          }
          {
            sensor = [ {
              state = "{% if is_state('climate.kueche', 'heat') %}1{% else %}0{% endif %}";
              name = "Küchenheizung";
            } ];
          }
          {
            sensor = [ {
              state = "{{ state_attr('climate.wohnzimmer', 'current_temperature') }}";
              name = "Temperatur Wohnzimmer";
            } ];
          }
        ];
        input_number = {
          ambient_co2 = {
            name = "CO2 in der Atmosphäre";
            unit_of_measurement = "ppm";
            min = "400";
            max = "1000";
            step = "1";
          };
          target_temperature_schlafzimmer = {
            name = "Zieltemperatur Schlafzimmer";
            unit_of_measurement = "°C";
            min = "16";
            max = "23";
            step = "0.5";
          };
          target_temperature_wohnzimmer = {
            name = "Zieltemperatur Wohnzimmer";
            unit_of_measurement = "°C";
            min = "16";
            max = "23";
            step = "0.5";
          };
          target_temperature_kueche = {
            name = "Zieltemperatur Küche";
            unit_of_measurement = "°C";
            min = "16";
            max = "23";
            step = "0.5";
          };
        };
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
            next_departure = [ { station = "3024634"; } ];
          }
          {
            platform = "dwd_weather_warnings";
            region_name = 106411000; # Stadt Darmstadt
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
              entities =
                map
                  (entity: {
                    inherit entity;
                    icon = "mdi:broadcast-off";
                  })
                  switches
              ;
              state_filter = [ "unavailable" ];
            }
            {
              type = "entity-filter";
              entities = fenster;
              state_filter = [ "on" ];
            }
            {
              type = "entity-filter";
              entities = batteries;
              state_filter = [ {
                value = 25;
                operator = "<";
              } ];
            }
          ];
          badges = alertbadges;
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
                  height = 200;
                  show = {
                    labels = true;
                    labels_secondary = "hover";
                    state = false;
                    legend = false;
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
                  name = "7d";
                })
                (mkConfig {
                  hours_to_show = 24;
                  points_per_hour = 3;
                  name = "24h";
                })
                (mkConfig {
                  hours_to_show = 1;
                  points_per_hour = 60;
                  show = {
                    name = false;
                    icon = false;
                    state = true;
                  };
                })
              ];
            }
          ;
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
                  "input_number.target_temperature_wohnzimmer"
                  "group.wohnzimmer_lights"
                  {
                    entity = "button.restart_${esp.wohnzimmer}";
                    name = "Klimasensor Neustarten";
                  }
                  {
                    entity = "button.${sensor.wohnzimmer}_force_calibration";
                    name = "CO2 Kalibrieren";
                  }
                ];
              }
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.wohnzimmer}_co2";
                    name = "CO2";
                    show_fill = false;
                  }
                  {
                    entity = "input_number.ambient_co2";
                    show_fill = false;
                    color = colors.warn;
                  }
                ];
                color_thresholds = [
                  {
                    value = 0;
                    color = colors.okay;
                  }
                  {
                    value = 1000;
                    color = colors.warn;
                  }
                  {
                    value = 2000;
                    color = colors.alert;
                  }
                ];
                color_thresholds_transition = "hard";
                show = {
                  labels = true;
                  labels_secondary = "hover";
                };
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                hour24 = true;
                decimals = 1;
                points_per_hour = 3;
              })
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.wohnzimmer}_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "sensor.${sensor.wohnzimmer}_dew_point";
                    name = "Taupunkt";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  {
                    entity = "input_number.target_temperature_wohnzimmer";
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
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.wohnzimmer}_humidity";
                    name = "Luftfeuchtigkeit";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  graph.outside.humidity
                ];
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
              {
                type = "logbook";
                entities = [
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
                  "input_number.target_temperature_kueche"
                  {
                    entity = "button.restart_${esp.kueche}";
                    name = "Klimasensor Neustarten";
                  }
                ];
              }
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.kueche}_humidity";
                    name = "Luftfeuchtigkeit";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  graph.outside.humidity
                  {
                    entity = "sensor.kuchenfenster";
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
                hour24 = true;
                decimals = 1;
                points_per_hour = 3;
                hours_to_show = 24;
                update_interval = 30;
                line_width = 2;
                state_map = [
                  {
                    value = 0;
                    label = "Zu";
                  }
                  {
                    value = 1;
                    label = "Auf";
                  }
                ];
              })
              (multiScaleGraphCards {
                type = "custom:mini-graph-card";
                entities = [
                  {
                    entity = "sensor.${sensor.kueche}_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
                  }
                  graph.outside.temperature
                  graph.outside.dew_point
                  {
                    entity = "sensor.${sensor.kueche}_dew_point";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.humidity;
                  }
                  {
                    entity = "input_number.target_temperature_kueche";
                    name = "Zieltemperatur";
                    show_fill = false;
                    color = colors.heating;
                  }
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
                  "input_number.target_temperature_schlafzimmer"
                  "group.schlafzimmer_lights"
                  {
                    entity = "button.restart_${esp.schlafzimmer}";
                    name = "Klimasensor Neustarten";
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
                    entity = "sensor.${sensor.schlafzimmer}_temperature";
                    name = "Temperatur";
                    show_fill = false;
                    color = colors.temperature;
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
                    entity = "input_number.target_temperature_schlafzimmer";
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
                    entity = "sensor.${sensor.schlafzimmer}_humidity";
                    name = "Luftfeuchtigkeit";
                    show_fill = false;
                    state_adaptive_color = true;
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
                entities = [ {
                  entity = "button.restart_${esp.bad}";
                  name = "Klimasensor Neustarten";
                } ];
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
              inherit badges;
              cards = [ envstack ];
            }
            {
              icon = "mdi:floor-plan";
              inherit badges;
              cards = [
                wohnzimmerstack
                schlafzimmerstack
                kuechenstack
                badstack
              ];
            }
          ];
        }
      ;
    };
    nginx.virtualHosts.${config.m-0.virtualHosts.home} = {
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
