{ pkgs, lib, config, ... }:
let inherit (import ../../pkgs) desktop-pkgs;
in {
  imports = [ ./rofi.nix ./ssh-agent.nix ./eventd.nix ./sleep-nag.nix ];
  m-0 = {
    workspaces = [
      "tasks"
      "chat"
      "mail"
      "code"
      "research"
      "work"
      "ccc"
      "orga"
      "leisure"
      "config"
    ];
    terminal = "${desktop-pkgs.terminal}/bin/terminal";
    colors = {
      "foreground" = "#dddbff";
      "background" = "#000000";
      "black" = "#000000";
      "brightBlack" = "#55508f";
      "red" = "#e34b4f";
      "brightRed" = "#e34b4f";
      "green" = "#67b779";
      "brightGreen" = "#45b75e";
      "yellow" = "#ff9c00";
      "brightYellow" = "#ff9c00";
      "blue" = "#5c67ff";
      "brightBlue" = "#5c67ff";
      "magenta" = "#cb85ff";
      "brightMagenta" = "#cb85ff";
      "cyan" = "#17d0f4";
      "brightCyan" = "#17d0f4";
      "white" = "#dddbff";
      "brightWhite" = "#ffffff";
    };
  };
  home = { packages = builtins.attrValues desktop-pkgs; };
  programs.browserpass.enable = true;
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc";
      package = pkgs.arc-theme;
    };
  };
  services = {
    mpd = {
      enable = true;
      network.listenAddress = "::1";
      musicDirectory = "${config.home.homeDirectory}/data/aktuell/media/musik";
      extraConfig = ''
        audio_output {
              type "pulse"
              name "Pulseaudio"
              server "localhost"
        }
      '';
    };
    mpdris2.enable = true;
  };
  systemd.user.services.mpdris2 = {
    Unit.Requires = [ "dbus.service" ];
    Install.WantedBy = [ "default.target" ];
  };
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
    bindings = {
      "XF86AudioMute" = "exec pactl set-sink-mute '@DEFAULT_SINK@' toggle";
      "XF86AudioLowerVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' -5%";
      "XF86AudioRaiseVolume" =
        "exec pactl set-sink-volume '@DEFAULT_SINK@' +5%";
      "XF86AudioMicMute" =
        "exec pactl set-source-mute '@DEFAULT_SOURCE@' toggle";
      "XF86MonBrightnessUp" =
        "exec ${pkgs.brightnessctl}/bin/brightnessctl +5%";
      "XF86MonBrightnessDown" =
        "exec ${pkgs.brightnessctl}/bin/brightnessctl +5%";
      "Tab" = "exec ${pkgs.skippy-xd}/bin/skippy-xd";
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
        "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";
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
    bindingsConfig = lib.concatStringsSep "\n" (lib.mapAttrsToList
      (binding: command: ''
        bindsym $mod+${binding} ${command}
      '') (bindings // workspaceBindings));
  in bindingsConfig + (lib.concatStringsSep "\n" (lib.mapAttrsToList (category:
    { border, background, text, indicator, childBorder }: ''
      client.${category} ${border}a0 ${background}c0 ${text} ${indicator} ${childBorder}
    '') swayColors)) + ''
      bar {
          status_command i3status-rs ${./status.toml};
          status_padding 0
          status_edge_padding 0
          font monospace 9.5
          height 17
          strip_workspace_numbers yes

          mode hide

          colors {
              statusline #ffffff
              background #00000000
              ${
                lib.concatStringsSep "\n" (lib.mapAttrsToList (category:
                  { background, border, text }: ''
                    ${category} ${background}c0 ${border} ${text}
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
                    ${category} ${background}c0 ${border} ${text}
                  '') barColors)
              }
          }
      }
          '');
}
