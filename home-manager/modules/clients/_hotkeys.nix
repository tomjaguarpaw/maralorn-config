{
  pkgs,
  config,
  lib,
  ...
}:
let
  fork = cmd: "fork ${cmd}";
  term = cmd: fork "foot ${cmd}";
  edit_dir = dir: term (shell "cd ${dir}; hx ${dir}");
  shell = cmd: "sh -c '${cmd}'";
  with-mic-check =
    cmd: fork (shell "${config.home.sessionVariables.TERMINAL} mic-check; ${cmd}");
in
[
  {
    "M: Toggle Keyboard Layout" =
      fork
        "hyprctl switchxkblayout keyboardio-model-100-keyboard next";
  }
  {
    Orga = [
      { Kassandra = fork "kassandra2"; }
      { Kalendar = term "ikhal"; }
      { Habitica = fork "firefox https://habitica.com"; }
      { Tasks = term "tasksh"; }
      { Meditate = term "meditate"; }
      { Pythia = term "pythia"; }
      { Notes = edit_dir "~/git/notes"; }
    ];
  }
  {
    Research = {
      Zotero = fork "zotero";
      Open = fork "evince ~/git/promotion/out/thesis.pdf";
      Build = term (shell "cd ~/git/promotion; nix run .# -- watch");
      Directory = fork "${config.home.sessionVariables.TERMINAL} -D ~/git/promotion";
      Edit = edit_dir "~/git/promotion";
    };
  }
  {
    Power = {
      Shutdown = "systemctl poweroff";
      Suspend = "systemctl suspend";
      Reboot = "systemctl reboot";
      Logout = "hyprctl dispatch exit";
      Lock = "swaylock";
      "Reload Hyprland" = "hyrpctl reload";
    };
  }
  {
    SSH =
      let
        ssh = host: term "ssh ${host}";
      in
      [
        { "hera via vpn" = ssh "hera.vpn.m-0.eu"; }
        { "fluffy via vpn" = ssh "fluffy.vpn.m-0.eu"; }
        { remote-builder = ssh "phoibe.cased.de"; }
        { ag = ssh "ag-forward"; }
        { mathe-gateway = ssh "gw"; }
        { backup-server = ssh "borg.cysec.de"; }
        { shells = ssh "shells"; }
        { "bach (ved)" = ssh "bach.vocalensemble-darmstadt.de"; }
        { "nixbuild.net" = "${pkgs.rlwrap}/bin/rlwrap ssh eu.nixbuild.net shell"; }
        { "fluffy via local network" = ssh "fluffy.lo.m-0.eu"; }
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
        { "MPD lokal" = mpdclient "::"; }
        { "Lautstärke" = "ncpamixer"; }
        { Pavucontrol = fork "pavucontrol"; }
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
        { "MPD Lounge" = mpdclient "lounge.w17.io"; }
        { "MPD Kitchen" = mpdclient "kitchen.w17.io"; }
        { "MPD Space" = mpdclient "burbon.w17.io"; }
      ];
  }
  {
    Apps = {
      Config = edit_dir "~/git/config";
      Files = fork "nautilus";
      Accounting = {
        Update = term (shell "cd ~/git/buchhaltung; nix develop -c interactive-update");
        Display =
          term
            "hledger -f ~/git/buchhaltung/buchhaltung.journal ui -- --watch --theme=terminal -X€ -t -E";
      };
      Games = {
        "Heroic Launcher" = fork "heroic";
        "Steam Lanucher" = fork "steam";
        "The Witcher 3" = fork "xdg-open heroic://launch/gog/1495134320";
        "Baldurs Gate 3" = fork "steam steam://rungameid/1086940";
        "Guild Wars 2" =
          fork
            "xdg-open heroic://launch/sideload/9gC1jhFqE9cV2xNz43ciaE";
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
    "Select Mode" = lib.pipe pkgs.flake-inputs.self.homeConfigurations [
      builtins.attrNames
      (builtins.filter (lib.hasPrefix "${config.m-0.hostName}-"))
      (map (lib.removePrefix "${config.m-0.hostName}-"))
      (x: lib.genAttrs x (name: "select-mode ${name}"))
    ];
  }
  {
    Communication = [
      { Matrix = fork "element-desktop"; }
      {
        Mail = {
          Open = term "neomutt";
          Inbox = term "neomutt -f ~/Maildir/hera/Inbox";
          Code = term "neomutt -f ~/Maildir/hera/Code";
        };
      }
      {
        Mumble = {
          CDA = with-mic-check "mumble mumble://maralorn@mumble.hax404.de";
          Nixos = with-mic-check "mumble mumble://maralorn@lassul.us/nixos";
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
      Strichliste = "firefox https://strichliste.w17.io/#!/user/56";
      Hub = "firefox https://hub.w17.io";
      Summer = "ssh door@burbon.w17.io buzzer";
      Open = "ssh door@burbon.w17.io open";
      Close = "ssh door@burbon.w17.io close";
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
]
