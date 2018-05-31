{ pkgs, lib, config, ... }:
with lib;
let
  inherit (config.m-0) colors workspaces terminal;
  exec = "exec --no-startup-id";
  taskstatus = pkgs.writeShellScriptBin "taskstatus" ''
    while true;
    do
      echo \
      $(date "+%Y-%m-%d %a %H:%M") "|" \
      $(cat ~/.kassandra_state | tail -n3 | sed "s/$/ | /") \
      Inbox: $(task +PENDING -BLOCKED -TAGGED count) "|" \
      Active Task: $(task rc.verbose=nothing active || echo "No task active") "|" \
      Tags: $(task +PENDING -BLOCKED -project -optional -later rc.verbose=nothing tags | sed "s/\(.\)$/\1 |/" )
      sleep 10s;
    done
    '';
  addMods = oldbindings: builtins.foldl' (newbindings: key:
    newbindings // {
      "Mod4+${key}" = oldbindings.${key};
      "Mod3+Mod4+${key}" = oldbindings.${key};
    })
    {}
    (builtins.attrNames oldbindings);
in {

config = mkIf config.m-0.graphical.enable {
  xsession = {
    windowManager.i3 = {
      enable = true;
      config = {
        focus = {
          followMouse = false;
          forceWrapping = true;
        };
        fonts = [ "Monofur Nerd Font 10.5" ];
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
        bars = [
          {
            statusCommand = "${taskstatus}/bin/taskstatus";
            position = "top";
            mode = "dock";
            workspaceButtons = false;
            colors = {
              separator = colors.white;
              background = colors.background;
            };
          }
          {
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
        keybindings = {
            "XF86AudioMute" = "exec pactl set-sink-mute '@DEFAULT_SINK@' toggle";
            "XF86AudioLowerVolume" = "exec pactl set-sink-volume '@DEFAULT_SINK@' -5%";
            "XF86AudioRaiseVolume" = "exec pactl set-sink-volume '@DEFAULT_SINK@' +5%";
            "XF86AudioMicMute" = "exec pactl set-source-mute '@DEFAULT_SOURCE@' toggle";
            "XF86MonBrightnessUp" = "exec xbacklight +5";
            "XF86MonBrightnessDown" = "exec xbacklight -5";
            "XF86Display" = "${exec} ${pkgs.arandr}/bin/arandr";
            "Ctrl+Escape" = "${exec} loginctl lock-session;";
        } //
        addMods ({
            "Left" = "focus left";
            "Down" = "focus down";
            "Up" = "focus up";
            "Right" = "focus right";
            "Tab" = "${exec} ${pkgs.skippy-xd}/bin/skippy-xd";
            "Prior" = "focus parent";
            "Next" = "focus child";
            "Return" = "${exec} ${terminal}";
            "p" = "${exec} rofi-pass";
            "shift+Left" = "move left";
            "shift+Down" = "move down";
            "shift+Up" = "move up";
            "shift+Right" = "move right";
            "d" = "split h";
            "f" = "fullscreen toggle";
            "t" = "layout tabbed";
            "s" = "layout toggle split";
            "q" = "kill";
            "m" = "move workspace to output up";
            "n" = "move workspace to output right";
            "shift+space" = "floating toggle";
            "shift+q" = "exec i3-nagbar -t warning -m 'do you want to exit i3?' -b 'yes' 'i3-msg exit'";
            "space" = "exec ~/config/nixos/packages/rust-scripts/target/release/hotkeys";
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
};

}
