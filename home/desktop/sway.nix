{ pkgs, lib, config, ... }:
let my-pkgs = import ../../pkgs;
in {
  xdg.configFile."sway/config".text = builtins.readFile ./sway.config + (let
    inherit (config.m-0) colors workspaces terminal;
    swayColors = {
      focused = {
        background = colors.blue;
        border = colors.blue;
        childBorder = colors.blue;
        indicator = colors.green;
        text = colors.foreground;
      };
      focused_inactive = {
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
    barColors = {
      active_workspace = {
        background = colors.blue;
        border = colors.blue;
        text = colors.white;
      };
      binding_mode = {
        background = colors.red;
        border = colors.red;
        text = colors.white;
      };
      focused_workspace = {
        background = colors.blue;
        border = colors.blue;
        text = colors.white;
      };
      inactive_workspace = {
        background = colors.background;
        border = colors.background;
        text = colors.white;
      };
    };
    modlessBindings = {
      "ctrl+escape" = "exec $lock";
      "XF86AudioMute" = "exec pactl set-sink-mute '@DEFAULT_SINK@' toggle";
      "XF86AudioLowerVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' -5%";
      "XF86AudioRaiseVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' +5%";
      "XF86AudioMicMute" =
        "exec pactl set-source-mute '@DEFAULT_SOURCE@' toggle";
      "XF86MonBrightnessUp" =
        "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%+";
      "XF86MonBrightnessDown" =
        "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
    };
    bindings = {
      "Left" = "focus left";
      "Down" = "focus down";
      "Up" = "focus up";
      "Right" = "focus right";
      "Shift+Left" = "move left";
      "Shift+Down" = "move down";
      "Shift+Up" = "move up";
      "Shift+Right" = "move right";
      "d" = "splith";
      "t" = "layout tabbed";
      "s" = "layout toggle split";
      "f" = "fullscreen";
      "Shift+space" = "floating toggle";
      "prior" = "focus parent";
      "next" = "focus child";
      "shift+q" =
        "exec ${pkgs.sway}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";
      "Return" = "exec ${terminal}";
      "q" = "kill";
      "space" = "exec hotkeys";
      "m" = "bar mode toggle monitoring";
    };
    workspaceBindings = builtins.foldl' (bindings: name:
      let
        number = toString ((builtins.length (builtins.attrNames bindings)) / 2);
      in bindings // {
        "${number}" = "workspace ${number}:${name}";
        "Shift+${number}" = "move container to workspace ${number}:${name}";
      }) { } workspaces;
    modlessBindingsConfig = lib.concatStringsSep "\n" (lib.mapAttrsToList
      (binding: command: ''
        bindsym ${binding} ${command}
      '') modlessBindings);
    bindingsConfig = lib.concatStringsSep "\n" (lib.mapAttrsToList
      (binding: command: ''
        bindsym $mod+${binding} ${command}
      '') (bindings // workspaceBindings));
    colorConfig = lib.concatStringsSep "\n" (lib.mapAttrsToList (category:
      { border, background, text, indicator, childBorder }: ''
        client.${category} ${border}a0 ${background}c0 ${text} ${indicator} ${childBorder}
      '') swayColors);
    barsConfig = ''
      bar {
          status_command i3status-rs ${./status.toml};
          status_padding 0
          status_edge_padding 0
          font monospace 9.5
          height 17
          position top

          mode hide

          colors {
              statusline #ffffff
              background #00000000
              ${
                lib.concatStringsSep "\n" (lib.mapAttrsToList (category:
                  { background, border, text }: ''
                    ${category} ${background}cc ${border} ${text}
                  '') barColors)
              }
          }
      }
      bar {
          id monitoring
          status_command i3status-rs ${./status-monitoring.toml};
          status_padding 0
          status_edge_padding 0
          font monospace 9.5
          height 17
          workspace_buttons no
          position top
          modifier none

          mode invisible

          colors {
              statusline #ffffff
              background #00000000
              ${
                lib.concatStringsSep "\n" (lib.mapAttrsToList (category:
                  { background, border, text }: ''
                    ${category} ${background}cc ${border} ${text}
                  '') barColors)
              }
          }
      }

      exec ${pkgs.mako}/bin/mako --background-color ${colors.background}cc --text-color ${colors.foreground} --border-size 0
      exec ${my-pkgs.my-ssh-add}/bin/my-ssh-add
      exec ${pkgs.xorg.xrdb}/bin/xrdb ${builtins.toFile "Xresources" "Xft.dpi: 96"}
    '';
  in bindingsConfig + modlessBindingsConfig + colorConfig + barsConfig);
}
