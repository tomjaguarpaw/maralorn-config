{ pkgs, lib, config, ... }:
let
  eventd-pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/fe61c3b84e8e81a8ec2bf6b3ed2a0e8652cea190.tar.gz) {};
  eventd = with eventd-pkgs; callPackage ../../packages/eventd {};
  colors = config.common.colors;
in {
  home = {
    packages = with pkgs; [
      eventd
    ];
  };
  systemd.user = {
    services = {
      eventd = {
        Unit = {
          Description = "eventd";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
        Service = {
          Type="notify";
          Sockets="eventd-control.socket eventd.socket";
          ExecStart="${eventd}/bin/eventd --listen systemd";
          ExecReload="${eventd}/bin/eventdctl reload";
        };
      };
    };
    sockets = {
      eventd-control = {
        Unit = {
          Description = "eventd control socket";
        };
        Socket = {
          Service = "eventd.service";
          SocketMode = "0600";
          ListenStream = "%t/eventd/private";
        };
      };
      eventd = {
        Unit = {
          Description = "eventd sockets";
        };
        Socket = {
          SocketMode = "0660";
          ListenStream= "%t/eventd/evp";
        };
      };
    };
  };
  xdg = {
    configFile = {
      "eventd/eventd.conf".text = lib.generators.toINI {} {
        "Queue default" = {
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue mode" = {
          Anchor = "top";
          Margin = 300;
          Limit = 1;
        };
        "Queue feedback" = {
          Anchor = "top";
          Margin = 450;
          Limit = 1;
        };
        "Queue workspace" = {
          Anchor = "bottom-left";
          Margin = 10;
          Limit = 1;
        };
        "Queue command" = {
          Anchor = "bottom-right";
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue critical" = {
          Anchor = "top";
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue state" = {
          Anchor = "top-left";
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue music" = {
          Anchor = "bottom";
          Margin = 10;
          Limit = 1;
        };
        Notification = {
          Text = "\${message}";
        };
        NotificationBubble = {
          Padding = 10;
          Radius = 0;
          Border = 4;
          BorderBlur = 4;
          Timeout = 5000;
          Colour = colors.background;
        };
        NotificationText = {
          Font = "Linux Libertine 12";
          Colour = colors.foreground;
        };
      };
      "eventd/notification.event".text = lib.generators.toINI {} {
        "Event mode *" = {
          Actions = "mode";
        };
        "Event notification *" = {
          Actions = "notification";
        };
        "Event feedback *" = {
          Actions = "feedback";
        };
        "Event workspace *" = {
          Actions = "workspace";
        };
        "Event command success" = {
          Actions = "command-success";
        };
        "Event command failure" = {
          Actions = "command-failure";
        };
        "Event critical *" = {
          Actions = "critical";
        };
        "Event state *" = {
          Actions = "state";
        };
      };
      "eventd/mode.action".text = lib.generators.toINI {} {
        Action = {
          Name = "mode";
        };
        Notification = {
        };
        NotificationBubble = {
          Timeout = 0;
          Queue = "mode";
          Padding = 40;
          MinWidth = 10;
        };
        NotificationText = {
          Font = "Linux Libertine 40";
        };
      };
      "eventd/command-success.action".text = lib.generators.toINI {} {
        Action = {
          Name = "command-success";
        };
        Notification = {
          Text="<b>\${command}</b>\\nsucceeded after \${time} @ \${host}";
        };
        NotificationBubble = {
          Colour = colors.black;
        };
      };
      "eventd/command-failure.action".text = lib.generators.toINI {} {
        Action = {
          Name = "command-failure";
        };
        Notification = {
          Text="<b>\${command}</b>\\nfailed after \${time} @ \${host}";
        };
        NotificationBubble = {
          Colour = colors.red;
        };
      };
      "eventd/workspace.action".text = lib.generators.toINI {} {
        Action = {
          Name = "workspace";
        };
        Notification = {
        };
        NotificationBubble = {
          Queue = "workspace";
          MinWidth = 10;
        };
      };
      "eventd/feedback.action".text = lib.generators.toINI {} {
        Action = {
          Name = "feedback";
        };
        Notification = {
        };
        NotificationBubble = {
          Timeout = 500;
          Queue = "feedback";
          Colour = colors.red;
        };
      };
      "eventd/notification.action".text = lib.generators.toINI {} {
        Action = {
          Name = "notification";
        };
        Notification = {
          Text = "<b>\${title}</b>\${message/^/\\n}";
        };
      };
    };
  };
}
