{ pkgs, lib, config, ... }:
let
  colors = config.common.colors;
  workspaces = config.common.workspaces;
  terminal = config.common.terminal;
  exec = "exec --no-startup-id";
  addMods = oldbindings: builtins.foldl' (newbindings: key:
    newbindings // {
      "Mod4+${key}" = oldbindings.${key};
      "Mod3+Mod4+${key}" = oldbindings.${key};
    })
    {}
    (builtins.attrNames oldbindings);
in {
  imports = [ 
    ./eventd.nix
    ./rofi
    ./urxvt.nix
  ];
  home.packages = [ pkgs.skippy-xd ];
  xsession = {
    windowManager.i3 = {
      enable = true;
      config = {
        focus = {
          followMouse = false;
          forceWrapping = true;
        };
        colors = {
          focused = {
            background = colors.blue;
            border = colors.blue;
            childBorder = colors.blue;
            indicator = colors.green;
            text = colors.foreground;
          };
          focusedInactive = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.green;
            text = colors.foreground;
          };
          unfocused = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.green;
            text = colors.foreground;
          };
          urgent = {
            background = colors.red;
            border = colors.red;
            childBorder = colors.red;
            indicator = colors.green;
            text = colors.foreground;
          };
        };
        bars = [ {
          mode = "hide";
          colors = {
            separator = colors.white;
            background = colors.background;
            activeWorkspace = {
              background = colors.blue;
              border = colors.blue;
              text = colors.white;
            };
            bindingMode = {
              background = colors.red;
              border = colors.red;
              text = colors.white;
            };
            focusedWorkspace = {
              background = colors.blue;
              border = colors.blue;
              text = colors.white;
            };
            inactiveWorkspace = {
              background = colors.background;
              border = colors.background;
              text = colors.white;
            };
          };
        } ];
        window = {
          titlebar = false;
          border = 1;
        };
        gaps = {
          inner = 0;
          outer = 0;
          smartBorders = "off";
          smartGaps = false;
        };
        keybindings = addMods ({
            "Left" = "focus left";
            "Down" = "focus down";
            "Up" = "focus up";
            "Right" = "focus right";
            "Tab" = "${exec} rofi -show window";
            "w" = "${exec} skippy-xd";
            "Prior" = "focus parent";
            "Next" = "focus child";
            "Ctrl+Escape" = "${exec} loginctl lock-session;";
            "Return" = "${exec} ${terminal}";
            "p" = "${exec} rofi-pass";
            "r" = "${exec} rofi -show combi";
            "o" = "${exec} rofi -show web";
            "n" = "${exec} rofi -show ssh";
            "a" = "${exec} tasklauncher";
            "shift+Left" = "move left";
            "shift+Down" = "move down";
            "shift+Up" = "move up";
            "shift+Right" = "move right";
            "d" = "split h";
            "f" = "fullscreen toggle";
            "t" = "layout tabbed";
            "s" = "layout toggle split";
            "q" = "kill";
            "shift+space" = "floating toggle";
            "shift+q" = "exec i3-nagbar -t warning -m 'do you want to exit i3?' -b 'yes' 'i3-msg exit'";
          } // builtins.foldl' (bindings: name: let
            number = toString ((builtins.length (builtins.attrNames bindings)) / 2);
          in
            bindings // {
              "${number}" = "workspace ${number}:${name}";
              "Shift+${number}" = "move container to workspace ${number}:${name}";
            }) {} workspaces
        );
      };
    };
  };
}
