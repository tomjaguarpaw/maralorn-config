{ pkgs, ... }:
let
  fork = cmd: "fork ${cmd}";
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
    ];
  }
  { Research = { Zotero = fork "zotero"; }; }
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
        ssh = host: "kitty +kitten ssh ${host}";
      in
      {
        hera = ssh "hera";
        fluffy = ssh "fluffy.lo.m-0.eu";
        remote-builder = ssh "phoibe.cased.de -i /etc/nixos/private/id_ed25519-nix-builder";
        ag = ssh "ag-forward";
        gwres1 = ssh "gw";
        backup-server = ssh "borg.cysec.de";
        shells = ssh "shells";
        "bach (ved)" = ssh "bach.vocalensemble-darmstadt.de";
        "nixbuild.net" = "${pkgs.rlwrap}/bin/rlwrap ssh eu.nixbuild.net -i /etc/nixos/private/id_ed25519-nix-builder shell";
      };
  }
  {
    Sound = {
      "Play/Pause" = "${pkgs.playerctl}/bin/playerctl play-pause";
      MPD = "ncmpcpp";
      "Lautstärke" = "ncpamixer";
      Pavucontrol = fork "pavucontrol";
      Headset = {
        connect = "bluetoothctl connect AC:12:2F:4F:EB:FA";
        disconnect = "bluetoothctl disconnect AC:12:2F:4F:EB:FA";
      };
    };
  }
  {
    Web = {
      Browser = fork "firefox";
      "Private Browser" = fork "firefox --private-window";
      Chromium = fork "chromium";
      Games = {
        Steam = fork "steam";
      };
      Deluge = fork "deluge";
    };
  }
  { Files = fork "nautilus"; }
  { Passmenu = "pass clip -f"; }
  { "Select Mode" = "select-mode"; }
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
          CDA = fork "mumble mumble://maralorn@mumble.hax404.de";
          Nixos = fork "mumble mumble://maralorn@lassul.us/nixos";
        };
      }
      { Weechat = "weechat"; }
      { Signal = fork "signal-desktop"; }
      { Zoom = fork "zoom"; }
      { Telegram = fork "telegram-desktop"; }
      { Discord = fork "Discord"; }
      { Tmate = "tmate"; }
    ];
  }
  { "Monitor (htop)" = "htop"; }
  {
    "W17" = {
      Summer = "ssh door@burbon.w17.io buzzer";
      Open = "ssh door@burbon.w17.io open";
      Close = "ssh door@burbon.w17.io close";
    };
  }
]
