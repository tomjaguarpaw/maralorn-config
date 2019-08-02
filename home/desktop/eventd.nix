{ pkgs, lib, config, ... }:
let
  colors = config.m-0.colors;
  inherit (import ../../pkgs) eventd;
in {

  home.packages = [ eventd ];
  systemd.user = {
    services = {
      eventd = {
        Unit = {
          Description = "eventd";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Install = { WantedBy = [ "default.target" ]; };
        Service = {
          Type = "notify";
          Sockets = "eventd-control.socket eventd.socket";
          ExecStart = "${eventd}/bin/eventd --listen systemd";
          ExecReload = "${eventd}/bin/eventdctl reload";
        };
      };
    };
    sockets = {
      eventd-control = {
        Unit = { Description = "eventd control socket"; };
        Socket = {
          Service = "eventd.service";
          SocketMode = "0600";
          ListenStream = "%t/eventd/private";
        };
      };
      eventd = {
        Unit = { Description = "eventd sockets"; };
        Socket = {
          SocketMode = "0660";
          ListenStream = "%t/eventd/evp";
        };
      };
    };
  };
  xdg = {
    configFile = {
      "eventd/eventd.conf".text = lib.generators.toINI { } {
        "Queue default" = {
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue command" = {
          Anchor = "bottom-right";
          Margin = 10;
          Spacing = 2;
          Limit = 10;
        };
        "Queue critical" = {
          Anchor = "top";
          Margin = 450;
          Spacing = 2;
          Limit = 10;
        };
        "Queue tasks" = {
          Anchor = "bottom";
          Margin = 0;
          Spacing = 1;
          Limit = 20;
        };
        Notification = { Text = "\${message}"; };
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
      "eventd/notification.event".text = lib.generators.toINI { } {
        "Event notification *" = { Actions = "notification"; };
        "Event notification kassandra" = { Actions = "kassandra"; };
        "Event command success" = { Actions = "command-success"; };
        "Event command failure" = { Actions = "command-failure"; };
        "Event critical *" = { Actions = "critical"; };
      };
      "eventd/command-success.action".text = lib.generators.toINI { } {
        Action = { Name = "command-success"; };
        Notification = {
          Text = "<b>\${command}</b>\\nsucceeded after \${time} @ \${host}";
        };
        NotificationBubble = {
          Colour = colors.black;
          Queue = "command";
        };
      };
      "eventd/command-failure.action".text = lib.generators.toINI { } {
        Action = { Name = "command-failure"; };
        Notification = {
          Text = "<b>\${command}</b>\\nfailed after \${time} @ \${host}";
        };
        NotificationBubble = {
          Queue = "critical";
          Colour = colors.red;
        };
      };
      "eventd/critical.action".text = lib.generators.toINI { } {
        Action = { Name = "critical"; };
        Notification = { Text = "<b>\${title}</b>\${message/^/\\n}"; };
        NotificationBubble = {
          Queue = "critical";
          Colour = colors.red;
        };
      };
      "eventd/kassandra.action".text = lib.generators.toINI { } {
        Action = { Name = "kassandra"; };
        Notification = { Text = "<b>\${title}</b>\${message/^/\\n}"; };
        NotificationBubble = { Queue = "critical"; };
      };
      "eventd/notification.action".text = lib.generators.toINI { } {
        Action = { Name = "notification"; };
        Notification = { Text = "<b>\${title}</b>\${message/^/\\n}"; };
      };
    };
  };

}
