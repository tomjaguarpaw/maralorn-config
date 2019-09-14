{ pkgs, lib, config, ... }:
with lib;
let
  inherit (config.m-0) colors workspaces terminal;
  exec = "exec --no-startup-id";
  conkyCommon = ''
    background = false,
    border_width = 0,
    cpu_avg_samples = 2,
    draw_borders = false,
    draw_graph_borders = true,
    draw_outline = false,
    draw_shades = false,
    double_buffer = true,
    use_xft = true,
    font = 'Roboto Mono Nerd Font:size=8',
    gap_x = 0,
    gap_y = 0,
    minimum_width = 316,
    maximum_width = 316,
    net_avg_samples = 2,
    no_buffers = true,
    out_to_console = false,
    out_to_stderr = false,
    extra_newline = false,
    own_window = true,
    own_window_class = 'Conky',
    own_window_type = 'override',
    own_window_colour = "${config.m-0.colors.background}",
    own_window_hints = 'undecorated,below,skip_taskbar,skip_pager,sticky',
    stippled_borders = 0,
    update_interval = 1.0,
  '';
  conkyOrgaConfig = pkgs.writeText "conky.conf" ''
    conky.config = {
      alignment = 'top_right',
      ${conkyCommon}
    }

    conky.text = [[
    ''${font Roboto Mono Nerd Font:bold:size=14}''${color #8888ff}$alignc''${exec date '+%a %_d. %B, %H:%M:%S'}
    ''${font Roboto Mono Nerd Font:size=12}
    ''${color #d0d0d0}''${execpi 60 ${pkgs.gcal}/bin/gcal -K -s1 --iso-week-number=yes | sed -e 's|5\d/1|01|' | sed -ne '3,10p' | sed -e 's/</ ''${color 8888ff}/'| sed -e 's/>/ ''${color}/' | sed 's/^/$alignc/'}
    $font
    $hr
    ''${execi 5 cat ~/.kassandra_state | tail -n4}
    $hr
    ''${execi 5 cat ~/tmp/today.md}
    ]]
  '';
  conkyMPDConfig = pkgs.writeText "conky.conf" ''
    conky.config = {
      alignment = 'bottom_right',
      ${conkyCommon}
      mpd_host = "::0",
      mpd_port = 6600
    }

    conky.text = [[
    MPD $mpd_status | Vol: $mpd_vol% | Ran: $mpd_random | Rep: $mpd_repeat
    $mpd_smart
    $mpd_album
    $mpd_elapsed/$mpd_length ($mpd_percent%) $mpd_bar
    ]]

  '';
  addMods = oldbindings:
    builtins.foldl' (newbindings: key:
      newbindings // {
        "Mod4+${key}" = oldbindings.${key};
        "Mod3+Mod4+${key}" = oldbindings.${key};
      }) { } (builtins.attrNames oldbindings);
in {

  xsession = {
    windowManager.i3 = {
      enable = true;
      extraConfig = ''
        gaps right 320
      '';
      package = (import <unstable> { }).i3-gaps;
      config = {
        startup = [
          {
            command = "${pkgs.conky}/bin/conky -c ${conkyOrgaConfig}";
            notification = false;
          }
          {
            command = "${pkgs.conky}/bin/conky -c ${conkyMPDConfig}";
            notification = false;
          }
        ];
        focus = {
          followMouse = false;
          forceWrapping = true;
        };
        fonts = [ "Inconsolata Nerd Font pixelsize=16" ];
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
        bars = [{
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
        }];
        window = {
          titlebar = false;
          border = 1;
          commands = [{
            command = "floating disable";
            criteria = { class = "Firefox"; };
          }];
        };
        keybindings = {
          "XF86AudioMute" = "exec pactl set-sink-mute '@DEFAULT_SINK@' toggle";
          "XF86AudioLowerVolume" =
            "exec pactl set-sink-volume '@DEFAULT_SINK@' -5%";
          "XF86AudioRaiseVolume" =
            "exec pactl set-sink-volume '@DEFAULT_SINK@' +5%";
          "XF86AudioMicMute" =
            "exec pactl set-source-mute '@DEFAULT_SOURCE@' toggle";
          "XF86MonBrightnessUp" = "exec xbacklight +5";
          "XF86MonBrightnessDown" = "exec xbacklight -5";
          "XF86Display" = "${exec} ${pkgs.arandr}/bin/arandr";
          "Ctrl+Escape" = "${exec} loginctl lock-session;";
        } // addMods ({
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
          "shift+q" =
            "${exec} ${pkgs.i3}/bin/i3-nagbar -t warning -m 'do you want to exit i3?' -b 'yes' 'i3-msg exit'";
          "space" = "${exec} hotkeys";
        } // builtins.foldl' (bindings: name:
          let
            number =
              toString ((builtins.length (builtins.attrNames bindings)) / 2);
          in bindings // {
            "${number}" = "workspace ${number}:${name}";
            "Shift+${number}" = "move container to workspace ${number}:${name}";
          }) { } workspaces);
      };
    };
  };

}
