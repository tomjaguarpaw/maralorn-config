{
  pkgs,
  config,
  lib,
  ...
}:
let
  fork = cmd: "fork ${cmd}";
  link = url: fork "firefox ${url}";
  term = cmd: fork "${config.home.sessionVariables.TERMINAL} ${cmd}";
  edit_dir = dir: term (shell "cd ${dir}; hx ${dir}");
  shell = cmd: "sh -c '${cmd}'";
  with-mic-check = cmd: fork (shell "${config.home.sessionVariables.TERMINAL} mic-check; ${cmd}");
  hotkeys = [
    {
      Mode =
        let
          mode = mode: shell "echo ${mode} > ~/.mode";
        in
        {
          DND = mode "DND";
          Normal = mode "Normal";
          Sort = mode "Sort";
        };
    }
    {
      Orga = [
        { Tasks = link "https://todo.darmstadt.ccc.de/projects/33/kanban"; }
        { Checklisten = link "https://todo.darmstadt.ccc.de/projects/-4/list"; }
        { Kalendar = term "ikhal"; }
        { Notes = link "https://notes.maralorn.de/doc/scratchpad-VEtvbVwIAT"; }
      ];
    }
    {
      Heilmann = {
        Klog = shell "EDITOR='hx' klog edit";
        Chat = link "https://3.basecamp.com/3601168/projects";
        Issueboard = link "https://github.com/orgs/heilmannsoftware/projects/17/views/5?groupedBy%5BcolumnId%5D=126215311\\&filterQuery=assignee%3A%40me+-status%3ADone";
        Support = link "https://github.com/orgs/heilmannsoftware/projects/18/views/6";
        Actions = link "https://github.com/heilmannsoftware/connect/actions?query=actor%3Amaralorn";
        "Pull Requests" =
          link "https://github.com/pulls?q=is%3Aopen+is%3Apr+author%3Amaralorn+archived%3Afalse+user%3Aheilmannsoftware";
      };
    }
    {
      Power = {
        Shutdown = "systemctl poweroff";
        Suspend = "systemctl suspend";
        Reboot = "systemctl reboot";
        Logout = "hyprctl dispatch exit";
        Lock = "swaylock";
        "Idle-Daemon" = {
          Disable = "systemctl --user stop swayidle";
          Enable = "systemctl --user start swayidle";
        };
        "Reset Graphics-Driver" = "sudo cat /sys/kernel/debug/dri/1/amdgpu_gpu_recover";
      };
    }
    {
      SSH =
        let
          ssh = host: term "kitten ssh ${host}";
        in
        [
          { "hera via vpn" = ssh "hera.vpn.m-0.eu"; }
          { "athene via vpn" = ssh "athene.vpn.m-0.eu"; }
          { "zeus via vpn" = ssh "zeus.vpn.m-0.eu"; }
          { remote-builder = ssh "phoibe.cased.de"; }
          { ag = ssh "ag-forward"; }
          { mathe-gateway = ssh "gw"; }
          { backup-server = ssh "borg.cased.de"; }
          { shells = ssh "shells"; }
          { "bach (ved)" = ssh "bach.vocalensemble-darmstadt.de"; }
          { "nixbuild.net" = "${pkgs.rlwrap}/bin/rlwrap ssh eu.nixbuild.net shell"; }
          { "athene via local network" = ssh "home.local.maralorn.de"; }
          { "hera via public v4" = ssh "hera-v4"; }
          { "TU Tunnel" = "sshuttle --python python3.9 -r gw 130.83.0.0/16"; }
        ];
    }
    {
      Sound =
        let
          mpdclient = host: shell "switch-mpd ${host}; ncmpcpp -h ${host}";
        in
        [
          { "Play/Pause" = "${pkgs.playerctl}/bin/playerctl play-pause"; }
          { "Toggle Output Mute" = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle"; }
          { "Toggle Input Mute" = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle"; }
          {
            MPD = {
              Lokal = mpdclient "::";
              Athene = mpdclient "athene";
              Lounge = mpdclient "lounge.cccda.de";
              Kitchen = mpdclient "kitchen.cccda.de";
              Space = mpdclient "burbon.cccda.de";
            };
          }
          {
            "Forward to Athene" = {
              Connect = "pactl load-module module-tunnel-sink server=athene";
              Disconnect = "pactl unload-module module-tunnel-sink";
            };
          }
          { "Lautstärke" = fork "pavucontrol"; }
          {
            Headset = {
              Earplugs = {
                connect = "bluetoothctl connect 00:00:AB:BD:7D:68";
                disconnect = "bluetoothctl disconnect 00:00:AB:BD:7D:68";
              };
              Overears = {
                connect = "bluetoothctl connect E8:EE:CC:25:66:C3";
                disconnect = "bluetoothctl disconnect E8:EE:CC:25:66:C3";
              };
            };
          }
        ];
    }
    {
      Apps = {
        Config = edit_dir "~/git/config";
        Files = fork "nautilus";
        Accounting = {
          Update = term (shell "cd ~/git/buchhaltung; nix develop -c interactive-update");
          Display = term "hledger -f ~/git/buchhaltung/buchhaltung.journal ui -- --watch --theme=terminal -X€ -t -E";
        };
        Games = {
          "Heroic Launcher" = fork "heroic";
          "Steam Lanucher" = fork "steam";
          "The Witcher 3" = fork "xdg-open heroic://launch/gog/1495134320";
          "Baldurs Gate 3" = fork "steam steam://rungameid/1086940";
          "Guild Wars 2" = fork "xdg-open heroic://launch/sideload/9gC1jhFqE9cV2xNz43ciaE";
          "Minecraft" = fork "prismlauncher";
          "Factorio" = fork "factorio";
        };
      };
    }
    {
      Web = {
        Browser = fork "firefox";
        "Private Browser" = fork "firefox --private-window";
        Chromium = fork "chromium";
        "Software-Updates" = term "software-updates";
        News = term "news";
        Watchfeeds = term "watchfeeds";
      };
    }
    {
      Passmenu =
        let
          copy-password = cmd: shell "${cmd} | wl-copy -o";
        in
        {
          Unlock = copy-password "rbw get bitwarden";
          Password = copy-password "rbw-fzf";
          "OTP" = copy-password "rbw-totp-fzf";
        };
    }
    {
      Communication = [
        { Matrix = fork "element-desktop"; }
        {
          Mail =
            let
              mail = param: "neomutt ${param}; notmuch new";
            in
            term (shell ''
              case "$(cat ~/.mode || echo Normal)" in Normal) ${mail "-f \"notmuch://?query=tag:unread and folder:/Inbox/\""};; DND) ${mail ""};; *) ${mail "-f \"notmuch://?query=folder:/Inbox/\""};; esac
            '');
        }
        {
          Mumble = {
            CDA = with-mic-check "mumble mumble://maralorn@mumble.hax404.de";
            Nixos = with-mic-check "mumble mumble://maralorn@lassul.us/nixos";
            CCC = with-mic-check "mumble mumble://maralorn@mumble.c3pb.de";
          };
        }
        { Weechat = "weechat"; }
        { Signal = fork "signal-desktop"; }
        { Zoom = with-mic-check "zoom"; }
        { Telegram = fork "telegram-desktop"; }
        { Rocket = fork "rocketchat-desktop"; }
        { Discord = with-mic-check "Discord"; }
        { Tmate = "tmate"; }
      ];
    }
    { "Monitor" = "htop"; }
    {
      W17 = {
        Strichliste = link "https://strichliste.cccda.de/#!/user/56";
        Hub = link "https://hub.cccda.de";
        Summer = "ssh door@burbon.cccda.de buzzer";
        Open = "ssh door@burbon.cccda.de open";
        Close = "ssh door@burbon.cccda.de close";
      };
    }
    { "Dismiss last notification" = "makoctl dismiss"; }
    {
      Timers = {
        "Start Pomodoro" = shell "set-timer Pause; set-timer Pomodoro 25minutes";
        "Pause Pomodoro" = shell "set-timer Pomodoro; set-timer Pause 5minutes";
        "Stop Pomodoro" = shell "set-timer Pause; set-timer Pomodoro";
        "Start Tee" = "set-timer Tee 7minutes";
        "Stop Tee" = "set-timer Tee";
        "Clear Timers" = shell "echo [] > ~/.timers";
      };
    }
    { Emoji = lib.getExe pkgs.emote; }
  ];
  my-hotkeys = pkgs.writeShellScriptBin "my-hotkeys" "${lib.getBin pkgs.wizards-dialog}/bin/hotkeys ${pkgs.writeText "hotkeys.yaml" (builtins.toJSON hotkeys)}";
in
{
  home.packages = [ my-hotkeys ];
}
