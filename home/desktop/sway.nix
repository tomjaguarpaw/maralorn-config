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
        border = colors.background;
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
      "ctrl+space" = "exec ${pkgs.mako}/bin/makoctl dismiss";
      "XF86AudioMute" = "exec pactl set-sink-mute '@DEFAULT_SINK@' toggle";
      "XF86AudioLowerVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' -5%";
      "XF86AudioRaiseVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' +5%";
      "XF86AudioMicMute" =
        "exec pactl set-source-mute 'alsa_input.pci-0000_00_1f.3.analog-stereo' toggle";
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
      "shift+space" = "floating toggle";
      "prior" = "focus parent";
      "next" = "focus child";
      "shift+r" = "exec ${pkgs.sway}/bin/swaymsg reload";
      "shift+q" =
        "exec ${pkgs.sway}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";
      "Return" = "exec ${terminal}";
      "q" = "kill";
      "space" = "exec hotkeys";
    };
    workspaceBindings = pkgs.lib.fold (a: b: a // b) { } (builtins.map (num:
      let
        name = builtins.elemAt workspaces num;
        number = toString num;
      in {
        "${number}" = "workspace ${number}:${name}";
        "Shift+${number}" = "move container to workspace ${number}:${name}";
        "Ctrl+${number}" = "workspace x${number}:${name}";
        "Ctrl+Shift+${number}" =
          "move container to workspace x${number}:${name}";
      }) (lib.range 0 9));
    workspaceScreens = builtins.map (num:
      let
        name = builtins.elemAt workspaces num;
        number = toString num;
      in {
        name = "${number}:${name}";
        screen = "$intern";
      }) (lib.range 0 9);
    workspacehighScreens = builtins.map (num:
      let
        name = builtins.elemAt workspaces num;
        number = toString num;
      in {
        name = "x${number}:${name}";
        screen = "$high";
      }) (lib.range 2 7);
    workspacesmallScreens = builtins.map (num:
      let
        name = builtins.elemAt workspaces num;
        number = toString num;
      in {
        name = "x${number}:${name}";
        screen = "$small";
      }) (lib.range 0 1 ++ [ 8 ]);
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
      set $intern 'Unknown 0x2336 0x00000000'
      set $high 'Ancor Communications Inc ASUS VW248 B6LMTF011850'
      set $small 'Unknown X1910WDS 001367'

      ${pkgs.lib.concatMapStringsSep "\n"
      (p: "workspace ${p.name} output ${p.screen}")
      (workspaceScreens ++ workspacesmallScreens ++ workspacehighScreens)}
      bindsym Print exec pactl set-source-mute 'alsa_input.pci-0000_00_1f.3.analog-stereo' false
      bindsym --release Print exec pactl set-source-mute 'alsa_input.pci-0000_00_1f.3.analog-stereo' true

      bar {
          id standard
          status_command ${pkgs.i3status-rust}/bin/i3status-rs ${./status.toml};
          status_padding 0
          status_edge_padding 0
          font monospace 9.5
          height 17

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
          status_command ${pkgs.i3status-rust}/bin/i3status-rs ${
            ./status-monitoring.toml
          };
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

      exec ${pkgs.mako}/bin/mako --background-color "${colors.background}cc" --text-color "${colors.foreground}" --border-size 0
      exec ${my-pkgs.my-ssh-add}/bin/my-ssh-add
      exec ${pkgs.xorg.xrdb}/bin/xrdb ${
        builtins.toFile "Xresources" "Xft.dpi: 96"
      }
      exec ${pkgs.systemd}/bin/systemctl --user set-environment SWAYSOCK="$SWAYSOCK"
      exec ${pkgs.swayidle}/bin/swayidle -w before-sleep '$lock'
    '';
  in bindingsConfig + modlessBindingsConfig + colorConfig + barsConfig);
}
