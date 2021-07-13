let
  inherit (import (import ../nix/sources.nix).nixos-unstable { }) lib;
  restrictedPages = [
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
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./roles/default.nix ];
      m-0.hostName = hostName;
      nixpkgs.overlays = [ (_: _: (import ../channels.nix).${hostName}) ];
    };
  makeAutostart = name:
    { config, ... }: {
      config.xdg.configFile."autostart/${name}.desktop".source =
        "${config.home.path}/share/applications/${name}.desktop";
    };
  makeBlock = list:
    { pkgs, lib, ... }: {
      systemd.user.services.blockserver = {
        Unit.Description = "Serve a blocklist";
        Service = {
          ExecStart = "${pkgs.python3}/bin/python -m http.server 8842 -d ${pkgs.writeTextDir "blocklist" (lib.concatStringsSep "\r\n" list)}";
          Restart = "always";
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
in
{
  apollo =
    let
      install = f: ({ pkgs, ... }: { home.packages = f pkgs; });
      setStartpage = startpage:
        { ... }: {
          programs.firefox.profiles."fz2sm95u.default".settings = {
            "browser.startup.homepage" = startpage;
          };
        };
      apolloConfig = name: imports:
        makeConfig "apollo" (
          imports ++ [
            ./roles/systemd-exporter.nix
            ./roles/battery.nix
            ./roles/beets.nix
            ./roles/daily-driver-programs.nix
            ./roles/desktop
            ./roles/git-sign.nix
            ./roles/hoogle.nix
            ./roles/kassandra.nix
            ./roles/khal.nix
            ./roles/khard.nix
            ./roles/mail.nix
            ./roles/mpclient.nix
            ./roles/mpd.nix
            ./roles/on-my-machine.nix
            ./roles/research.nix
            ./roles/taskwarrior-git.nix
            ./roles/taskwarrior.nix
            ./roles/update_tasks.nix
            ./roles/vdirsyncer.nix
            ./roles/zettelkasten.nix
            (import ./roles/mode-switching.nix { modeDir = ".modes"; modeFile = "volatile/mode"; })
            (makeAutostart "unlock-ssh")
          ]
        );
      unrestricted = [
        ./roles/accounting.nix
        ./roles/mail-client.nix
        ./roles/pythia.nix
        ./roles/tinkering.nix
        ./roles/chat.nix
        (setStartpage "https://stats.maralorn.de/d/health-status")
        (makeBlock [ ])
      ];
    in
    {
      leisure = apolloConfig "Leisure" (unrestricted ++ [ ./roles/games.nix ]);
      orga = apolloConfig "Orga" [
        ./roles/mail-client.nix
        ./roles/accounting.nix
        ./roles/pythia.nix
        (setStartpage "https://habitica.com")
        (makeBlock restrictedPages)
        (makeAutostart "kassandra2")
        (install (p: [ p.discord ])) # For teaching
      ];
      research = apolloConfig "Research" [
        (makeBlock restrictedPages)
      ];
    };
  zeus =
    let
      all = [
        ./roles/accounting.nix
        ./roles/beets.nix
        ./roles/daily-driver-programs.nix
        ./roles/desktop
        ./roles/git-sign.nix
        ./roles/hoogle.nix
        ./roles/hourly-maintenance.nix
        ./roles/kassandra.nix
        ./roles/khal.nix
        ./roles/khard.nix
        ./roles/mail.nix
        ./roles/mpclient.nix
        ./roles/mpd.nix
        ./roles/on-my-machine.nix
        ./roles/pythia.nix
        ./roles/research.nix
        ./roles/state.nix
        ./roles/taskwarrior.nix
        ./roles/tinkering.nix
        ./roles/update_tasks.nix
        ./roles/vdirsyncer.nix
        (import ./roles/mode-switching.nix { modeDir = ".volatile/modes"; modeFile = ".mode"; })
        (makeAutostart "kassandra2")
        (makeAutostart "unlock-ssh")
      ];
      orga = all ++ [
        ./roles/mail-client.nix
      ];
      leisure = orga ++ [
        ./roles/games.nix
        ./roles/chat.nix
      ];
    in
    {
      research = makeConfig "zeus" all;
      orga = makeConfig "zeus" orga;
      leisure = makeConfig "zeus" leisure;
    };
  hera = {
    default = makeConfig "hera" [
      ./roles/on-my-machine.nix
      ./roles/headless.nix
      ./roles/weechat
      ./roles/headless-mpd.nix
      ./roles/mail.nix
      ./roles/kassandra.nix
      ./roles/mail2rss.nix
      ./roles/taskwarrior.nix
      ./roles/vdirsyncer.nix
      ./roles/khard.nix
      ./roles/khal.nix
    ];
  };
}
