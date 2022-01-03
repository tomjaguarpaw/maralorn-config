lib:
let
  # name refers to an internal name
  # title refers to a human readable name
  # mode = { name, title, options = { <option_name> : { title , icon } } }
in
rec {
  jinja = import ./jinja.nix lib;
  tap_actions =
    let
      fromServiceAction = action:
        {
          action = "call-service";
          inherit (action) service;
          service_data = action.data or { } // {
            inherit (action) entity_id;
          };
        };
    in
    {
      setMode = mode: option: fromServiceAction (actions.setMode mode option);
      cycleMode = mode: fromServiceAction (actions.cycleMode mode);
    };
  actions = {
    cycleMode = mode: {
      service = "input_select.select_next";
      entity_id = util.modeSelectEntity mode;
    };
    setMode = mode: option: {
      service = "input_select.select_option";
      data = { inherit option; };
      entity_id = util.modeSelectEntity mode;
    };
  };
  util = rec {
    mkIcon = name: "mdi:${name}";
    mkMode = name: options: { inherit name options; };
    modeSelectEntity = mode: "input_select.${modeSelectName mode}";
    modeSelectName = mode: "mode_${mode.name}";
    modeBinarySensorName = mode: option: "${modeSelectName mode}_is_${option}";
    modeBinarySensorEntity = mode: option: "binary_sensor.${modeBinarySensorName mode option}";
  };
  triggers = rec {
    stateTrigger = entity: { platform = "state"; entity_id = entity; };
    modeSwitchTrigger = mode: stateTrigger (util.modeSelectEntity mode);
  };
  conditions = {
    modeIs = mode: option: { condition = "state"; entity_id = util.modeSelectEntity mode; state = option; };
  };
  modules = rec {
    mkHAConfig = attrs: {
      services.home-assistant.config = attrs;
    };
    mkModeSwitcher = mode: attrs: { ... }:
      let
        options = builtins.attrNames mode.options;
      in
      mkHAConfig {
        input_select.${util.modeSelectName mode} = {
          inherit options;
          name = mode.title;
        } // attrs;
        template = builtins.map (templates.binarySensorForMode mode) options;
      };
  };
  cards = {
    modeSwitcher = mode:
      let
        mkEntity = optionName: option:
          {
            entity = util.modeBinarySensorEntity mode optionName;
            name = option.title;
            inherit (option) icon;
            tap_action = tap_actions.setMode mode optionName;
          };
      in
      {
        type = "glance";
        inherit (mode) title;
        columns = builtins.length (builtins.attrNames mode.options);
        show_state = false;
        entities = lib.mapAttrsToList mkEntity mode.options;
      };
  };
  templates = rec {
    binarySensor = state: attrs: { binary_sensor = [ ({ inherit state; } // attrs) ]; };
    binarySensorFromCondition = condition: binarySensor (jinja.if' condition "1" "0");
    binarySensorForMode = mode: option: binarySensorFromCondition (jinja.isState (util.modeSelectEntity mode) option) { name = util.modeBinarySensorName mode option; };
  };
}
