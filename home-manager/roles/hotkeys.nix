{pkgs, ...}: let
  fork = cmd: "fork ${cmd}";
in [
  {
    Orga = [
      {Kassandra = fork "kassandra2";}
      {Kalendar = "calendar";}
      {Habitica = fork "firefox https://habitica.com";}
      {Tasks = "tasksh";}
      {Meditate = "meditate";}
      {Pythia = "pythia";}
      {Notes = "vim ~/git/notes";}
    ];
  }
  {
    Research = {
      Zotero = fork "zotero";
      Open = fork "evince ~/git/promotion/out/print.pdf";
      Build = "sh -c 'cd ~/git/promotion; flix develop -c flix run'";
      Directory = fork "footclient -d ~/git/promotion";
      Edit = "vim ~/git/promotion";
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
    SSH = let
      ssh = host: "ssh ${host}";
    in [
      {"hera via vpn" = ssh "hera.vpn.m-0.eu";}
      {"fluffy via vpn" = ssh "fluffy.vpn.m-0.eu";}
      {remote-builder = ssh "phoibe.cased.de -i /etc/nixos/private/id_ed25519-nix-builder";}
      {ag = ssh "ag-forward";}
      {mathe-gateway = ssh "gw";}
      {backup-server = ssh "borg.cysec.de";}
      {shells = ssh "shells";}
      {"bach (ved)" = ssh "bach.vocalensemble-darmstadt.de";}
      {"nixbuild.net" = "${pkgs.rlwrap}/bin/rlwrap ssh eu.nixbuild.net -i /etc/nixos/private/id_ed25519-nix-builder shell";}
      {"fluffy via local network" = ssh "fluffy.lo.m-0.eu";}
      {"hera via public v4" = ssh "hera-v4";}
      {"TU Tunnel" = "sshuttle --python python3.9 -r gw 130.83.0.0/16";}
    ];
  }
  {
    Sound = {
      "Play/Pause" = "${pkgs.playerctl}/bin/playerctl play-pause";
      MPD = "ncmpcpp";
      "Lautstärke" = "ncpamixer";
      Pavucontrol = fork "pavucontrol";
      Headset = {
        connect = "bluetoothctl connect 00:00:AB:BD:7D:68";
        disconnect = "bluetoothctl disconnect 00:00:AB:BD:7D:68";
      };
    };
  }
  {
    Apps = {
      Editor = fork "codium";
      Config = "vim ~/git/config";
      Files = fork "nautilus";
      DarkTerminal = fork "footclient -o 'color.background=000000' -o 'foreground=ffffff'";
      Accounting = {
        Update = "nix run ./git/buchhaltung#update";
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
      Mastodon = fork "whalebird";
      "Private Browser" = fork "firefox --private-window";
      Chromium = fork "chromium";
      "Software-Updates" = "software-updates";
      Watchfeeds = "watchfeeds";
      News = "news";
      Deluge = fork "deluge";
      VoxMachina = "mpv https://www.youtube.com/playlist?list=PL1tiwbzkOjQz7D0l_eLJGAISVtcL7oRu_";
    };
  }
  {
    Passmenu = {
      Password = "pass-fzf";
      OTP = "pass-fzf otp";
    };
  }
  {"Select Mode" = "select-mode";}
  {
    Communication = [
      {Matrix = fork "element-desktop";}
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
      {Weechat = "weechat";}
      {Signal = fork "signal-desktop";}
      {Zoom = fork "zoom";}
      {Telegram = fork "telegram-desktop";}
      {Discord = fork "Discord";}
      {Tmate = "tmate";}
    ];
  }
  {"Monitor (btop)" = "btop";}
  {
    "W17" = {
      "MPD Whisky" = "ncmpcpp -h whisky.w17.io";
      "MPD Burbon" = "ncmpcpp -h burbon.w17.io";
      Summer = "ssh door@burbon.w17.io buzzer";
      Open = "ssh door@burbon.w17.io open";
      Close = "ssh door@burbon.w17.io close";
    };
  }
]
