{
  pkgs,
  lib,
  config,
  ...
}:
let
  get-workspaces = pkgs.writeShellScript "get-workspaces" ''
    spaces (){
      hyprctl workspaces -j | jq -c 'sort_by(.id)'
    }

    spaces
    socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | while read -r line; do
      spaces
    done
  '';
  get-active-workspace = pkgs.writeShellScript "get-active-workspace" ''
    activeworkspace (){
      hyprctl monitors -j | jq '.[] | select(.focused) | .activeWorkspace.id'
    }

    activeworkspace
    socat -u UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | while read -r line; do
      activeworkspace
    done
  '';
in
{

  systemd.user.services.eww = {
    Unit = {
      X-Restart-Triggers = [ "${config.programs.eww.configDir}" ];
      Description = "EWW";
    };

    Service = {
      Environment = "PATH=${
          lib.makeBinPath [
            pkgs.coreutils
            pkgs.hyprland
            pkgs.jq
            pkgs.socat
            pkgs.bash
          ]
        }";
      ExecStart = "${lib.getExe config.programs.eww.package} daemon --no-daemonize";
      ExecStartPost = "${lib.getExe config.programs.eww.package} open bar";
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland;
    configDir = pkgs.recursiveLinkFarm "eww-config-dir" {
      "eww.scss" = pkgs.writeText "eww.scss" ''
        $background: #151f3d;
        $background-darker: #0d0f1c;
        $background-darker-80: #0d0f1ce0;
        $background-secondary: #323035;
        $background-lighter: #152f6d;

        .time {
          font-weight: bold;
        }

        .small {
          font-family: CozetteVector;
          font-size: 12px;
        }

        .component {
          color: #ffffff;
          background: $background;
          border-radius: 0.2em;
          padding: 0.4em;
          margin: 0.5em;
        }
        .current {
          background: $background-lighter;
        }
        .empty {
          color: #aaaaaa;
        }
        .bar {
          background: transparent;
        }
        .comment {
          color: #888888;
        }

      '';
      "eww.yuck" = pkgs.writeText "eww.yuck" ''
        (defpoll status :interval "1s" :initial "[]" "cat /run/user/1000/status-bar")

        (deflisten workspaces :initial "[]" "${get-workspaces}")

        (deflisten current_workspace :initial "1" "${get-active-workspace}")

        (defpoll time :interval "5s" `date +'{"year":"%Y","month":"%m","day":"%d","week":"%V","week_day":"%a","hour":"%H","minute":"%M"}'`)

        (defwidget workspaces []
          (box :orientation "vertical"
            (label :text "''${workspaces}''${current_workspace}" :visible false)
            (for workspace in workspaces
              (box :class "component ''${workspace.id == current_workspace ? "current" : ""} ''${workspace.windows > 0 ? "occupied" : "empty"}"
                (label :class "comment" :text "''${workspace.windows}")
              )
            )
          )
        )

        (defwidget time []
          (box :class "component time" :orientation "vertical" :valign "end" :space-evenly false
            "''${time.year}-''${time.month}-''${time.day}"
            "KW''${time.week} ''${time.week_day}"
            "''${time.hour}:''${time.minute}"
          )
        )

        (defwidget status []
          (box :orientation "vertical" :space-evenly false
            (for message in status
              (box :style "color: #''${message.color};" :class "component ''${message.small ? "small" : ""}" :orientation "vertical"
                (for line in {message.content} (label :text "''${line}"))
              )
            )
          )
        )

        (defwidget sidebar []
          (centerbox :orientation "vertical" :space-evenly false (status) (workspaces) (time))
        )

        (defwindow bar
          :monitor 0
          :exclusive true
          :focusable false
          :geometry (geometry :x "0%"
                              :y "0%"
                              :width "100px"
                              :height "100%"
                              :anchor "right center")
          (sidebar)
        )
      '';
    };
  };
}
