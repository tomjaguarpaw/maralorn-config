let
  inherit (import (import ../nix/sources.nix).nixos-unstable { }) lib;
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./roles/default.nix ];
      m-0.hostName = hostName;
      nixpkgs.overlays = [ (_: _: (import ../channels.nix).${hostName}) ];
    };
in {
  apollo = let
    install = f: ({ pkgs, ... }: { home.packages = f pkgs; });
    makeAutostart = name:
      { config, ... }: {
        config.home.file.".config/autostart/${name}.desktop".source =
          "${config.home.path}/share/applications/${name}.desktop";
      };
    setStartpage = startpage:
      { ... }: {
        programs.firefox.profiles."fz2sm95u.default".settings = {
          "browser.startup.homepage" = startpage;
        };
      };
    makeBlock = list:
      { pkgs, lib, ... }: {
        systemd.user.services.blockserver = {
          Unit.Description = "Serve a blocklist";
          Service = {
            ExecStart = "${pkgs.python3}/bin/python -m http.server 8842 -d ${
                pkgs.writeTextDir "blocklist" (lib.concatStringsSep "\r\n" list)
              }";
            Restart = "always";
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
    tinkerPages = [
      "reddit.com"
      "github.com"
      "*.ccc.de"
      "haskell.org"
      "*.haskell.org"
      "*.nixos.org"
      "nixos.org"
      "matrix.org"
      "element.io"
      "youtube.de"
      "youtube.com"
      "*.element.io"
    ];
    leisurePages = [
      "zeit.de"
      "heise.de"
      "spiegel.de"
      "xkcd.com"
      "smbc-comics.com"
      "tagesschau.de"
      "welt.de"
      "ndr.de"
      "ard.de"
      "zdf.de"
      "twitter.com"
      "chaos.social"
    ];
    apolloConfig = imports:
      makeConfig "apollo" (imports ++ [
        ./roles/arbtt
        ./roles/zettelkasten.nix
        ./roles/hoogle.nix
        ./roles/battery.nix
        ./roles/mpd.nix
        ./roles/beets.nix
        ./roles/mpclient.nix
        ./roles/on-my-machine.nix
        ./roles/desktop
        ./roles/git-sign.nix
        ./roles/laptop.nix
        ./roles/mail.nix
        ./roles/update_tasks.nix
        ./roles/research.nix
        ./roles/vdirsyncer.nix
        ./roles/khard.nix
        ./roles/khal.nix
        ./roles/taskwarrior.nix
        ./roles/taskwarrior-git.nix
        ./roles/taskwarrior-notify.nix
      (makeAutostart "unlock-ssh")
      ]);
    unrestricted = [
      ./roles/accounting.nix
      ./roles/mail-client.nix
      ./roles/pythia.nix
      ./roles/tinkering.nix
      ./roles/chat.nix
      (setStartpage "https://stats.maralorn.de/d/health-status")
      (makeBlock [ ])
    ];
  in {
    unrestricted = apolloConfig unrestricted;
    orga = apolloConfig [
      ./roles/mail-client.nix
      ./roles/accounting.nix
      ./roles/pythia.nix
      (setStartpage "https://habitica.com")
      (makeBlock (tinkerPages ++ leisurePages))
      (makeAutostart "firefox")
      (makeAutostart "kassandra")
      (makeAutostart "kassandra2")
      (install (p: [ p.discord ])) # For teaching
    ];
    research = apolloConfig [
      (makeBlock (tinkerPages ++ leisurePages))
      (setStartpage "http://localhost:8042")
    ];
    gaming = apolloConfig (unrestricted ++ [ ./roles/games.nix ]);
  };
  hera = {
    default = makeConfig "hera" [
      ./roles/on-my-machine.nix
      ./roles/headless.nix
      ./roles/weechat
      ./roles/headless-mpd.nix
      ./roles/mail.nix
      ./roles/mail2rss.nix
      ./roles/taskwarrior.nix
      ./roles/vdirsyncer.nix
      ./roles/khard.nix
      ./roles/khal.nix
    ];
  };
}
