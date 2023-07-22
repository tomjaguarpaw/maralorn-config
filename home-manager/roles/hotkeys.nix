{
  pkgs,
  config,
  lib,
  ...
}:
let
  fork = cmd: "fork ${cmd}";
  edit_dir = dir: "sh -c 'cd ${dir}; hx ${dir}'";
  with-mic-check =
    cmd: fork "sh -c '${config.home.sessionVariables.TERMINAL} mic-check; ${cmd}'";
in
[
  {
    Orga = [
      { Kassandra = fork "kassandra2"; }
      { Kalendar = "ikhal"; }
      { Habitica = fork "firefox https://habitica.com"; }
      { Tasks = "tasksh"; }
      { Meditate = "meditate"; }
      { Pythia = "pythia"; }
      { Notes = edit_dir "~/git/notes"; }
    ];
  }
  {
    Research = {
      Zotero = fork "zotero";
      Open = fork "evince ~/git/promotion/out/thesis.pdf";
      Build = "sh -c 'cd ~/git/promotion; nix run .# -- watch'";
      Directory = fork "${config.home.sessionVariables.TERMINAL} -D ~/git/promotion";
      Edit = edit_dir "~/git/promotion";
    };
  }
  {
    Power = {
      Shutdown = "systemctl poweroff";
      Suspend = "systemctl suspend";
      Reboot = "systemctl reboot";
    };
  }
  {
    SSH =
      let
        ssh = host: "ssh ${host}";
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
        mpdclient = host: "sh -c 'switch-mpd ${host}; ncmpcpp -h ${host}'";
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
        Update = "sh -c 'cd ~/git/buchhaltung; nix develop -c interactive-update'";
        Display = "hledger -f ~/git/buchhaltung/buchhaltung.journal ui -- --watch --theme=terminal -X€ -t -E";
      };
      Games = {
        GW2 = fork "gw2";
        Steam = fork "steam";
        Minecraft = fork "minecraft-launcher";
      };
    };
  }
  {
    Web = {
      Browser = fork "firefox";
      "Private Browser" = fork "firefox --private-window";
      Chromium = fork "chromium";
      "Software-Updates" = "software-updates";
      News = "news";
      Watchfeeds = "watchfeeds";
    };
  }
  {
    Passmenu =
      let
        copy-password =
          cmd:
          "sh -c '(${cmd} | wl-copy) && ${
            lib.getExe pkgs.termdown
          } -T \"Clearing password in\" -f term 20 && wl-copy -c'";
      in
      {
        Unlock = copy-password "rbw get bitwarden";
        Password = copy-password "rbw-fzf";
        "OTP" = copy-password "rbw-totp-fzf";
      };
  }
  {
    "Select Mode" =
      lib.mapAttrs (name: _: "select-mode ${name}")
        (import ../machines.nix).${config.m-0.hostName};
  }
  {
    Communication = [
      { Matrix = fork "element-desktop"; }
      {
        Mail = {
          Open = "neomutt";
          Inbox = "neomutt -f ~/Maildir/hera/Inbox";
          Code = "neomutt -f ~/Maildir/hera/Code";
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
  { "Monitor (btop)" = "btop"; }
  {
    "W17" = {
      Strichliste = "firefox https://strichliste.w17.io/#!/user/56";
      Hub = "firefox https://hub.w17.io";
      Summer = "ssh door@burbon.w17.io buzzer";
      Open = "ssh door@burbon.w17.io open";
      Close = "ssh door@burbon.w17.io close";
    };
  }
  { "Clear Notifications" = "sh -c 'rm -r ~/.notifications/*'"; }
]
